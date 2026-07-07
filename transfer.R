# setup_fabio_workspace.R
# ------------------------------------------------------------------
# Copies the raw input folders ("input" and "own_data") from an existing
# FABIO-BFP project root into a new one, then creates the empty folder
# tree needed for intermediate and final outputs.
#
# "R" and "inst" are assumed to come with the git repo; the shared
# /mnt/nfs_fineprint/... mounts are created by the R pipeline itself,
# so they are not handled here.
# ------------------------------------------------------------------

src <- "/home/mmondolfo/fabio_bfp"
dst <- "/home/vcoco/fabio_bfp"

# Folders to copy whole (with all contents)
copy_folders <- c("input", "own_data", "inputs_for_final_data", "intermediate_data")

# Empty folders to create for intermediate + final outputs
make_folders <- c(
  "data", "data/tidy", "data/sua", "data/fao", "data/NPK", "data/dataflow",
  "data/extensions", "data/extensions/cbs", "data/extensions/sua",
  "data/extensions/fd_cbs", "data/extensions/fd_sua", "data/extensions/tidy",
  "intermediate_data",
  "losses", "calories", "output", "output/plot"
)

# --- Sanity checks ------------------------------------------------
if (!dir.exists(src)) stop("Source directory does not exist: ", src)
for (f in copy_folders) {
  if (!dir.exists(file.path(src, f)))
    stop("Expected folder to copy is missing: ", file.path(src, f))
}

if (!dir.exists(dst)) dir.create(dst, recursive = TRUE)

# --- Copy the input folders ---------------------------------------
# Copying the *folder itself* (recursive) recreates it inside dst,
# e.g. <src>/input -> <dst>/input.
for (f in copy_folders) {
  message("Copying ", f, " ...")
  ok <- file.copy(
    from      = file.path(src, f),
    to        = dst,
    overwrite = TRUE,
    recursive = TRUE,
    copy.date = TRUE,
    copy.mode = TRUE
  )
  if (!ok) message("  WARNING: copy reported a failure for ", f)
}

# --- Create the output folder tree --------------------------------
message("Creating intermediate/output folders ...")
for (d in make_folders) {
  dir.create(file.path(dst, d), recursive = TRUE, showWarnings = FALSE)
}

message("Done. Workspace ready at: ", dst)