# =============================================================================
# 00_helpers.R  -  shared invariants + origin-routing helpers (FABIO-BCP)
# -----------------------------------------------------------------------------
# Sourced from the tail of R/00_system_variables.R, so every script that already
# does `source("R/00_system_variables.R")` gets these for free (06, 08_04, 09_1,
# 10_1a, 11, 12_a, 12_b, 13, 14). 08_03 sources it directly.
#
# Provides
#   fabio_assert()        hard-stop (STRICT=TRUE) or warn (STRICT=FALSE)
#   bilateralise_topup()  spread an import top-up over ORIGINS using the existing
#                         btd import mix -> world export mix, capped by origin
#                         headroom, water-filled, residual returned as an attr
#   btd_add()             additive upsert of bilateral flows into a btd table
#   commodity_kind()      PRIMARY / PROCESSED classification from the SUT itself
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# --- STRICT flag -------------------------------------------------------------
# 00_run_config.R sets it; default TRUE so a script sourced on its own still stops.
if (!exists("STRICT")) STRICT <- as.logical(Sys.getenv("FABIO_STRICT", unset = "TRUE"))

fabio_assert <- function(cond, ..., data = NULL) {
  ok <- isTRUE(all(cond))
  if (ok) return(invisible(TRUE))
  
  dots <- list(...)
  # Split dots into (format string) + (args for sprintf). The format string is the leading
  # run of character scalars; the args follow.
  #
  # The old rule — "fmt = EVERY leading character scalar" — silently breaks when an arg is
  # itself a character scalar (e.g. `year` from `for (year in as.character(years))`): the
  # year got swallowed into fmt, leaving sprintf one short -> "too few arguments". An assert
  # that should REPORT a failure then crashes instead, hiding what it caught.
  #
  # Robust split: choose the boundary k so the number of %-specifiers in fmt equals the
  # number of trailing args. This is unique — specifier count is non-decreasing as k grows
  # while the arg count (n - k) strictly decreases, so they coincide at most once. Multi-line
  # format strings (several adjacent string literals) still work; a character arg no longer
  # gets absorbed. Falls back to the old boundary if nothing matches (malformed call).
  is_chr <- vapply(dots, function(x) is.character(x) && length(x) == 1L, logical(1))
  m      <- if (length(is_chr)) match(FALSE, is_chr, nomatch = length(dots) + 1L) - 1L else 0L
  n      <- length(dots)
  count_specs <- function(s) {
    s    <- gsub("%%", "", s)   # literal percents consume no arg
    hits <- gregexpr("%[-+ 0#]*[*]?[0-9]*(?:\\.[0-9]+)?[diouxXeEfgGaAcs]", s, perl = TRUE)[[1]]
    if (length(hits) == 1L && hits[1L] == -1L) 0L else length(hits)
  }
  k <- m                                   # fallback = old behaviour
  for (kk in seq_len(m))
    if (count_specs(paste0(unlist(dots[seq_len(kk)]), collapse = "")) == n - kk) { k <- kk; break }
  fmt  <- paste0(unlist(dots[seq_len(k)]), collapse = "")
  args <- if (k < n) dots[(k + 1L):n] else list()
  
  txt <- if (length(args)) do.call(sprintf, c(list(fmt), args)) else fmt
  if (!is.null(data))
    txt <- paste0(txt, "\n--- offending rows (head) ---\n",
                  paste(utils::capture.output(print(utils::head(data, 10))), collapse = "\n"))
  if (isTRUE(STRICT)) stop("[FABIO-ASSERT] ", txt, call. = FALSE)
  warning("[FABIO-ASSERT] ", txt, call. = FALSE)
  invisible(FALSE)
}

# -----------------------------------------------------------------------------
# bilateralise_topup()
# -----------------------------------------------------------------------------
# need     : data.table(area_code, year, comm_code, item_code, need)   need > 0
#            = the tonnage a DESTINATION must import that nothing supplies today
# btd      : data.table(from_code, to_code, year, comm_code, item_code, value)
# headroom : optional data.table(area_code, year, comm_code, cap) = how much each
#            ORIGIN may still export before we are inventing its production too.
#            NULL => uncapped.
#
# Allocation prior, per (year, comm, destination):
#   1. the destination's own existing import mix in `btd`   (IDN/MYS for NL palm)
#   2. if it imports nothing today: world export shares of that commodity
#   3. if the commodity has no cross-border trade at all: unallocated (logged)
# Origins that hit their cap are frozen and the remainder is water-filled onto the
# origins that still have headroom (max_pass passes).
#
# Returns btd-schema flows to ADD (from_code, to_code, year, comm_code, item_code,
# value), with attr(out, "residual") = the tonnage that could not be sourced.
# -----------------------------------------------------------------------------
bilateralise_topup <- function(need, btd, headroom = NULL,
                               exclude_comms = c("c901", "c999"),
                               tol = 1e-6, max_pass = 5L) {
  
  empty <- data.table(from_code = integer(), to_code = integer(), year = integer(),
                      comm_code = character(), item_code = integer(), value = numeric())
  no_resid <- data.table(year = integer(), comm_code = character(),
                         item_code = integer(), area_code = integer(),
                         unallocated = numeric())
  
  need <- as.data.table(need)
  need <- need[!is.na(need) & need > tol & !comm_code %in% exclude_comms]
  if (!nrow(need)) { setattr(empty, "residual", no_resid); return(empty) }
  
  trade <- as.data.table(btd)[from_code != to_code & !is.na(value) & value > 0,
                              .(value = sum(value)),
                              by = .(year, comm_code, from_code, to_code)]
  
  # (1) destination-specific import mix
  mix_d <- trade[, .(from_code, w = value / sum(value)), by = .(year, comm_code, to_code)]
  setnames(mix_d, "to_code", "area_code")
  
  # (2) world export mix (fallback for destinations that import nothing)
  mix_w <- trade[, .(w = sum(value)), by = .(year, comm_code, from_code)]
  mix_w[, w := w / sum(w), by = .(year, comm_code)]
  
  cand <- merge(need, mix_d, by = c("year", "comm_code", "area_code"),
                all.x = TRUE, allow.cartesian = TRUE)
  
  fb   <- unique(cand[is.na(from_code), .(year, comm_code, item_code, area_code, need)])
  cand <- cand[!is.na(from_code)]
  
  dead <- no_resid
  if (nrow(fb)) {
    fb2 <- merge(fb, mix_w, by = c("year", "comm_code"), all.x = TRUE, allow.cartesian = TRUE)
    dead <- unique(fb2[is.na(from_code), .(year, comm_code, item_code, area_code,
                                           unallocated = need)])
    fb2 <- fb2[!is.na(from_code) & from_code != area_code]
    if (nrow(fb2)) {
      fb2[, w := w / sum(w), by = .(year, comm_code, area_code)]
      cand <- rbind(cand, fb2[, names(cand), with = FALSE], use.names = TRUE)
    }
  }
  if (!nrow(cand)) { setattr(empty, "residual", dead); return(empty) }
  
  cand[, `:=`(remaining = need, value = 0)]
  
  hr <- NULL
  if (!is.null(headroom)) {
    hr <- as.data.table(headroom)[, .(year, comm_code, from_code = area_code,
                                      cap = pmax(cap, 0))]
  }
  
  for (p in seq_len(max_pass)) {
    cand[, wsum := sum(w), by = .(year, comm_code, area_code)]
    live <- cand[wsum > 0 & remaining > tol]
    if (!nrow(live)) break
    
    cand[, add := 0]
    cand[wsum > 0 & remaining > tol, add := remaining * (w / wsum)]
    
    if (!is.null(hr)) {
      tot <- cand[add > 0, .(add_tot = sum(add)), by = .(year, comm_code, from_code)]
      tot <- merge(tot, hr, by = c("year", "comm_code", "from_code"), all.x = TRUE)
      tot[is.na(cap), cap := Inf]
      tot[, f := pmin(1, cap / pmax(add_tot, 1e-12))]
      
      cand[tot, on = .(year, comm_code, from_code), f := i.f]
      cand[is.na(f), f := 1]
      cand[, add := add * f]
      cand[f < 1 - 1e-9, w := 0]          # capped origin: frozen for the next pass
      cand[, f := NULL]
      
      used <- cand[, .(used = sum(add)), by = .(year, comm_code, from_code)]
      hr[used, on = .(year, comm_code, from_code), cap := pmax(cap - i.used, 0)]
    }
    
    cand[, value := value + add]
    got <- cand[, .(got = sum(add)), by = .(year, comm_code, area_code)]
    cand[got, on = .(year, comm_code, area_code), remaining := pmax(remaining - i.got, 0)]
    
    if (max(cand$remaining) < tol) break
  }
  
  resid <- unique(cand[remaining > tol,
                       .(year, comm_code, item_code, area_code, unallocated = remaining)])
  resid <- rbind(resid, dead, use.names = TRUE)
  
  out <- cand[value > tol, .(value = sum(value)),
              by = .(from_code, to_code = area_code, year, comm_code, item_code)]
  setcolorder(out, c("from_code", "to_code", "year", "comm_code", "item_code", "value"))
  setattr(out, "residual", resid)
  out
}

# -----------------------------------------------------------------------------
# btd_add(): ADD flows onto an existing btd table (upsert that sums, not replaces)
# -----------------------------------------------------------------------------
btd_add <- function(btd, add) {
  if (!nrow(add)) return(btd)
  btd <- as.data.table(copy(btd))
  add <- as.data.table(add)
  key_cols <- c("from_code", "to_code", "year", "comm_code")
  btd[add, on = key_cols, value := fcoalesce(value, 0) + i.value]
  new_rows <- add[!btd, on = key_cols]
  if (nrow(new_rows)) {
    miss <- setdiff(names(btd), names(new_rows))
    for (m in miss) new_rows[, (m) := NA]
    btd <- rbind(btd, new_rows[, names(btd), with = FALSE], use.names = TRUE)
  }
  btd[]
}

# PRIMARY vs PROCESSED from the authoritative ISIC column in items_full_bcp.csv.
#   ISIC == "A" -> PRIMARY,  ISIC == "C" -> PROCESSED  (covers c001-c145, c171)
#   c146-c170 (biofuels + biopolymers) -> PROCESSED
#   c062 (Grazing), c901 (Other, Waste), c999 (Other, Unknown) -> PRIMARY
#   c145 (UCO) kept PRIMARY by waste-stream convention (collected, not manufactured)
commodity_kind <- function(items_full, waste_comms = c("c145", "c901")) {
  it <- unique(as.data.table(items_full)[, .(comm_code, ISIC)], by = "comm_code")
  it[, n := as.integer(sub("c", "", comm_code))]
  it[, kind := fifelse(ISIC == "A", "PRIMARY",
                       fifelse(ISIC == "C", "PROCESSED", NA_character_))]
  it[is.na(kind) & n >= 146 & n <= 170, kind := "PROCESSED"]
  it[comm_code %in% c("c062", "c901", "c999"), kind := "PRIMARY"]
  it[comm_code %in% waste_comms, kind := "PRIMARY"]        # documented override, keeps c145/c901
  if (any(is.na(it$kind)))
    stop("commodity_kind: unclassified commodities: ", paste(it[is.na(kind), comm_code], collapse = ", "))
  it[, .(comm_code, kind)]
}