###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(janitor)
library(zoo)
library(httr2)
library(xml2)
library(comtradr)
library(censusapi)
library(purrr)


###########################################################
########### LOADING DATA #########
###########################################################

## --- portable repo root: FABIO_BFP_ROOT override, else walk up to the repo marker ---
fabio_root <- Sys.getenv("FABIO_BFP_ROOT", unset = "")
if (!nzchar(fabio_root)) {
  fabio_root <- getwd()
  while (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")) &&
         dirname(fabio_root) != fabio_root) fabio_root <- dirname(fabio_root)
  if (!file.exists(file.path(fabio_root, "R", "00_system_variables.R")))
    stop("Repo root not found above ", getwd(), " - set FABIO_BFP_ROOT or run from inside the repo.")
}

setwd(fabio_root)

########### FABIO regions #########

regions <- read.csv("inst/regions_full.csv", fileEncoding = "latin1") %>% filter(current == TRUE)

########### BACI trade data #########

# baci_hs07 <- readRDS("/mnt/nfs_fineprint/tmp/baci/baci_hs07.rds")
baci_hs12 <- readRDS("/mnt/nfs_fineprint/tmp/baci/baci_hs12.rds")

########### Trade data from Eurostat #########

### Combined Eurostat API request: biofuels + biopolymers ###

fetch_comext <- function(url_data,
                         poll_every = 15,    # seconds between status checks
                         max_wait   = 1800,  # give up after 30 min
                         verbose    = TRUE) {
  
  is_async <- function(txt) str_detect(txt, "syncResponse|asyncResponse|<env:Envelope")
  
  # Derive the async base from the data URL.
  # .../comext/dissemination/sdmx/3.0/...  ->  .../comext/dissemination
  async_base <- str_replace(url_data, "/sdmx/.*$", "")
  
  # --- Step 1: initial request -------------------------------------------
  body <- request(url_data) |>
    req_timeout(120) |>
    req_retry(max_tries = 5) |>          # only helps with 429/5xx, NOT the async case
    req_perform() |>
    resp_body_string()
  
  # Fast path: a cached/synchronous CSV came straight back
  if (!is_async(body)) return(read_csv(I(body), show_col_types = FALSE))
  
  # --- Async path: parse the request id ----------------------------------
  id <- read_xml(body) |> xml_ns_strip() |>
    xml_find_first(".//queued/id") |> xml_text()
  if (is.na(id) || !nzchar(id))
    stop("Async envelope received but no request id found:\n", body)
  if (verbose) message("Queued async extraction, id = ", id)
  
  status_url <- sprintf("%s/1.0/async/status/%s", async_base, id)
  data_url   <- sprintf("%s/1.0/async/data/%s",   async_base, id)
  
  # --- Step 2: poll until AVAILABLE --------------------------------------
  waited <- 0
  repeat {
    status <- request(status_url) |>
      req_timeout(60) |> req_retry(max_tries = 5) |>
      req_perform() |> resp_body_string() |>
      read_xml() |> xml_ns_strip() |>
      xml_find_first(".//status/status") |> xml_text()
    
    if (verbose) message(sprintf("[%5ds] %s", waited, status))
    
    if (identical(status, "AVAILABLE")) break
    if (status %in% c("EXPIRED", "UNKNOWN_REQUEST", "ERROR"))
      stop("Async request ended with status: ", status)
    if (waited >= max_wait)
      stop("Timed out after ", max_wait, "s waiting for the extraction.")
    
    Sys.sleep(poll_every); waited <- waited + poll_every
  }
  
  # --- Step 3: download the prepared data --------------------------------
  out <- request(data_url) |>
    req_timeout(300) |> req_retry(max_tries = 5) |>
    req_perform() |> resp_body_string()
  
  if (is_async(out) || str_detect(out, "<S:Fault"))
    stop("Data endpoint returned a fault instead of CSV:\n", substr(out, 1, 500))
  
  read_csv(I(out), show_col_types = FALSE)
}

url_data <- paste0(
  "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/",
  "data/dataflow/ESTAT/ds-045409/1.0/*.*.*.*.*.*",
  "?c[freq]=A",
  "&c[reporter]=AT,BE,BG,CY,CZ,DE,DK,EE,ES,FI,FR,GR,HR,HU,IE,IT,LT,LU,LV,MT,NL,PL,PT,RO,SE,SI,SK",
  "&c[partner]=AD,AE,AL,AM,AN,AR,AT,AU,AW,BA,BD,BE,BG,BH,BR,BY,BZ,CA,CH,CL,CN,CY,CZ,DE,DK,EE,EG,",
  "ES,FI,FR,GB,GI,GR,HK,HR,HU,ID,IE,IL,IN,IS,IT,JP,KR,KW,LA,LK,LT,LU,LV,MO,MT,MY,NL,NO,OM,PH,",
  "PK,PL,PT,QA,RO,RU,SA,SE,SG,SI,SK,SX,TH,TR,TW,UA,US,VN,XS,ZA",
  "&c[product]=2207,271020,29012190,29012290,29053926,29053995,29091910,29171310,29171920,38260010,38260090,39121100,39121200",
  "&c[flow]=1,2",
  "&c[indicators]=QUANTITY_IN_100KG,VALUE_IN_EUROS",
  "&c[TIME_PERIOD]=2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010",
  "&compress=false&format=csvdata&formatVersion=1.0&lang=en&labels=both")

bilateral_bf_bp_eu <- fetch_comext(url_data) %>%
  mutate(across(c(reporter, partner, product, indicators), ~ sub(":.*", "", .x)),
         across(c(freq, flow),                             ~ sub(".*:", "", .x))) %>%
  rename(FLOW = flow)

########### Total imports and exports by product for Sweden #########

setwd(fabio_root)

SWE_raw <- read_excel("own_data/swe_data_2018_2022.xlsx", skip = 2, n_max = 8, na = "..")

########### Trade data from own collection #########

im_own <- read_excel("own_data/Compilation_data_sources.xlsx", sheet = "imports")
ex_own <- read_excel("own_data/Compilation_data_sources.xlsx", sheet = "exports")

########### Years / region vectors #########

years        <- as.character(2010:2022)
baci_regions <- unique(regions$baci)
iso_regions  <- unique(regions$iso3c)



########### ComTrade for products missing in BACI #########

#######################################################
### PERSONAL KEY TO DELETE WHEN CODE MADE PUBLIC ###
set_primary_comtrade_key("ec6b584d5b364bc39580ac71f1cd438f")
#######################################################


hs_codes <- c("152000","271020","290531","291030","291811","382490","390770","391211","391212")
safe_get <- possibly(
  \(yr) ct_get_data(
    type                     = "goods",
    frequency                = "A",
    commodity_classification = "HS",
    commodity_code           = hs_codes,
    reporter                 = "all_countries",
    partner                  = "all_countries",
    flow_direction           = c("import", "export"),
    start_date               = as.character(yr),
    end_date                 = as.character(yr),
    extra_params             = list(customsCode = "C00", motCode = "0")
  ),
  otherwise = NULL
)


raw_list <- map(2012:2022, \(yr) { message("Fetching ", yr, " ..."); safe_get(yr) })

bilateral_comtrade <- bind_rows(raw_list)


reporter_codes <- iso_regions

dir.create("comtrade_cache", showWarnings = FALSE)

walk(reporter_codes, \(r) {
  fname <- paste0("comtrade_cache/", r, ".rds")
  if (file.exists(fname)) return(invisible())   # resume support
  
  message(Sys.time(), " | ", r)
  
  result <- tryCatch(
    ct_get_data(
      type                     = "goods",
      frequency                = "A",
      commodity_classification = "HS",
      commodity_code           = hs_codes,
      reporter                 = r,
      partner                  = "everything",
      flow_direction           = c("import", "export"),
      start_date               = "2012",
      end_date                 = "2022",
      extra_params             = list(customsCode = "C00", motCode = "0")
    ),
    error = \(e) { message("  SKIP ", r, ": ", conditionMessage(e)); NULL }
  )
  
  if (!is.null(result) && nrow(result) > 0)
    saveRDS(result, fname)
  # no Sys.sleep() — comtradr handles throttle internally
})

bilateral_comtrade <- map(
  list.files("comtrade_cache", full.names = TRUE, pattern = "\\.rds$"),
  readRDS
) |> bind_rows()

###########################################################
########### BTD USA (Sebacic acid only) #########
###########################################################

#######################################################
### PERSONAL KEY TO DELETE WHEN CODE MADE PUBLIC ###
Sys.setenv(CENSUS_KEY = "640fd224e6aae45d5147836df7cead135cf8f1ce")
#######################################################

# Imports
us_imports <- map_dfr(2012:2024, function(yr) {
  getCensus(
    name = "timeseries/intltrade/imports/hs",
    vars = c("I_COMMODITY", "CTY_CODE", "CTY_NAME",
             "GEN_VAL_YR", "GEN_QY1_YR", "UNIT_QY1"),
    time = yr,
    I_COMMODITY = "2917130030",
    SUMMARY_LVL = "DET"
  ) %>%
    mutate(year = yr)
})

cty_iso3c <- tibble(
  CTY_NAME = c("CHINA", "INDIA", "BELGIUM", "CANADA", "ITALY",
               "NETHERLANDS", "SWITZERLAND", "TAIWAN", "FRANCE",
               "JAPAN", "UNITED KINGDOM", "GERMANY", "KOREA, SOUTH",
               "OMAN", "SPAIN", "PAKISTAN"),
  exporter_iso3 = c("CHN", "IND", "BEL", "CAN", "ITA",
                    "NLD", "CHE", "TWN", "FRA",
                    "JPN", "GBR", "DEU", "KOR",
                    "OMN", "ESP", "PAK")
)

# --- Shared pre-cleaning ---
us_imports_base <- us_imports %>%
  rename(commodity = I_COMMODITY, monetary_value = GEN_VAL_YR, qty = GEN_QY1_YR) %>%
  mutate(qty             = as.numeric(qty),
         monetary_value  = as.numeric(monetary_value)) %>%
  select(-time, -commodity, -CTY_CODE, -SUMMARY_LVL, -I_COMMODITY_1) %>%
  filter(CTY_NAME != "TOTAL FOR ALL COUNTRIES")

# --- Quantity-only subset (existing pipeline) ---
us_imports_clean <- us_imports_base %>%
  group_by(year, CTY_NAME) %>%
  summarise(value   = sum(qty / 1000000, na.rm = TRUE),
            unit    = "kt",
            .groups = "drop") %>%
  left_join(cty_iso3c, by = "CTY_NAME") %>%
  mutate(importer_iso3 = "USA",
         product       = "Sebacic acid") %>%
  select(-CTY_NAME)

# --- Value subset (keeps monetary values) ---
prices_sebacic_us <- us_imports_base %>%
  group_by(year, CTY_NAME) %>%
  summarise(value          = sum(qty / 1000000, na.rm = TRUE),
            monetary_value = sum(monetary_value, na.rm = TRUE),
            unit           = "kt",
            .groups        = "drop") %>%
  left_join(cty_iso3c, by = "CTY_NAME") %>%
  mutate(importer_iso3 = "USA",
         product       = "Sebacic acid") %>%
  select(-CTY_NAME)


###########################################################
########### CLEANING #########
###########################################################

###########################################################
#1. Monthly trade data from Sweden (incl. HVO) #########
###########################################################

date_header <- SWE_raw %>% slice(1) %>% select(-1) %>% unlist() %>% as.character()
products    <- SWE_raw %>% slice(2:7) %>% pull(1) %>% as.character()

prod_col_names   <- paste0(date_header[1:60],    "_prod")
import_col_names <- paste0(date_header[61:120],  "_import")
export_col_names <- paste0(date_header[121:180], "_export")

data_section <- SWE_raw %>%
  slice(2:7) %>% select(-1) %>%
  set_names(c(prod_col_names, import_col_names, export_col_names)) %>%
  mutate(product = products, .before = 1)

pivot_metric <- function(data, metric_cols, metric_name) {
  data %>%
    select(product, all_of(metric_cols)) %>%
    pivot_longer(cols = -product, names_to = "year_month", values_to = "value") %>%
    mutate(
      product    = case_when(
        product == "ren FAME"             ~ "Biodiesel",
        product == "ren HVO"              ~ "Renewable diesel",
        product == "ren annan bio-bensin" ~ "Biogasoline other than ethanol",
        product == "ren bio-flygfotogen"  ~ "Bio jet kerosene",
        product == "ren bio-nafta"        ~ "Bionaphtha",
        product == "ren etanol"           ~ "Bioethanol"
      ),
      year_month = str_remove(year_month, "_(import|export)$"),
      year       = as.integer(str_extract(year_month, "\\d{4}")),
      month      = as.integer(str_extract(year_month, "(?<=M)\\d{2}")),
      data_type  = metric_name,
      unit       = "ton",
      value      = as.numeric(value)
    ) %>%
    select(product, year, month, data_type, value, unit)
}

SWE_cleaned_df <- bind_rows(
  pivot_metric(data_section, import_col_names, "Import"),
  pivot_metric(data_section, export_col_names, "Export")
) %>%
  group_by(product, year, data_type) %>%
  summarize(value = ifelse(any(is.na(value)), NA, sum(value) / 1000), unit = "kt") %>%
  filter(!is.na(value)) %>%
  mutate(
    exporter_iso3 = case_when(data_type == "Export" ~ "SWE", TRUE ~ "Total"),
    importer_iso3 = case_when(data_type == "Export" ~ "Total", TRUE ~ "SWE"),
    subtype       = "Fuel",
    source        = "SCB Varufloden per branslelag"
  )


###########################################################
#2. Eurostat trade data (biofuels + biopolymers) ##########
###########################################################

# --- Shared cleaning (both indicators pass through) ---
bf_bp_eu_base <- bilateral_bf_bp_eu %>%
  rename(year = TIME_PERIOD,
         exporter_iso2 = reporter, importer_iso2 = partner) %>%
  mutate(
    exporter_iso2_new = if_else(FLOW == "IMPORT", importer_iso2, exporter_iso2),
    importer_iso2_new = if_else(FLOW == "IMPORT", exporter_iso2, importer_iso2),
    across(c(exporter_iso2_new, importer_iso2_new), ~ case_when(
      .x %in% c("AD","GI","GQ","LI","MH","VG","XK") ~ "ROW",
      .x %in% c("AW","CW","BQ","SX")                ~ "AN",
      .x == "UK"                                     ~ "GB",
      .x == "EL"                                     ~ "GR",
      TRUE                                           ~ .x
    ))
  ) %>%
  select(-c(exporter_iso2, importer_iso2),
         exporter_iso2 = exporter_iso2_new,
         importer_iso2 = importer_iso2_new) %>%
  filter(exporter_iso2 != "EU27_2020", importer_iso2 != "EU27_2020") %>%
  left_join(regions %>% select(iso3c, iso2c), by = c("exporter_iso2" = "iso2c")) %>%
  rename(exporter_iso3 = iso3c) %>%
  left_join(regions %>% select(iso3c, iso2c), by = c("importer_iso2" = "iso2c")) %>%
  rename(importer_iso3 = iso3c) %>%
  filter(!is.na(exporter_iso3), !is.na(importer_iso3)) %>%
  select(-any_of(c("STRUCTURE","STRUCTURE_ID","STRUCTURE_NAME","freq","Frequency",
                   "PRODUCT","flow","INDICATORS","TIME_PERIOD.1","Observation.Value")))

# --- Price subset: pivot so qty and monetary value sit side by side ---
bf_bp_eu_prices <- bf_bp_eu_base %>%
  pivot_wider(names_from = indicators, values_from = OBS_VALUE) %>%
  rename(qty_100kg = QUANTITY_IN_100KG, value_eur = VALUE_IN_EUROS) %>%
  select(-DATAFLOW, -`LAST UPDATE`, -exporter_iso2, -importer_iso2)

# --- Quantity-only subset (existing pipeline) ---
bilateral_bf_bp_eu <- bf_bp_eu_base %>%
  filter(indicators == "QUANTITY_IN_100KG") %>%
  mutate(value = OBS_VALUE / 10000, unit = "kt") %>%
  select(-OBS_VALUE, -indicators)



##### Blend share estimation #####
estimated_blend_by_exporter <- bilateral_bf_bp_eu %>%
  filter(product %in% c(38260010, 38260090)) %>%
  group_by(year, exporter_iso3) %>%
  summarise(
    tot_3826       = sum(value, na.rm = TRUE),
    val_38260010   = sum(if_else(product == 38260010, value, 0), na.rm = TRUE),
    share_38260010 = if_else(tot_3826 > 0, val_38260010 / tot_3826, NA_real_),
    est_blend      = share_38260010 * 0.99 + (1 - share_38260010) * 0.95,
    .groups = "drop"
  )

est_blend_avg <- estimated_blend_by_exporter %>%
  filter(!is.na(est_blend)) %>%
  group_by(year) %>%
  summarize(avg = sum(tot_3826 * est_blend) / sum(tot_3826), .groups = "drop")

#### Aggregate by named product (biofuels + biopolymers together) ####
bilateral_bf_bp_eu %<>%
  mutate(
    value = case_when(
      product == 2207                            ~ value,
      product == 271020                          ~ 0.05 * value,
      product == 29091910 ~ 0.45 * value,
      product == 38260010                        ~ 0.99 * value,
      product == 38260090                        ~ 0.95 * value,
      TRUE                                       ~ value
    ),
    product = case_when(
      product %in% c(271020, 38260010, 38260090) ~ "Biodiesel",
      product == 2207                            ~ "Bioethanol",
      product == 29171310                        ~ "Sebacic acid",
      product == 29171920                        ~ "Succinic acid",
      product == 29053926                        ~ "1,4-butanediol",
      product == 29053995                        ~ "1,3-propanediol",
      product == 29012190                        ~ "Ethylene",
      product == 29012290                        ~ "Propylene",
      product == 29091910                      ~ "ETBE",
      product %in% c(39121100, 39121200)         ~ "Cellulose acetate"
    )
  ) %>%
  group_by(exporter_iso3, importer_iso3, product, FLOW, year) %>%
  summarize(value = sum(value, na.rm = TRUE), unit = first(unit), source = "Eurostat",
            .groups = "drop") %>%
  filter(!is.na(value), !is.na(product))




###########################################################
#3. Trade data from own collection #########
###########################################################

clean_own <- function(df) {
  df %<>%
    select(-c(`2023`, `2024`)) %>%
    mutate(
      across(years, ~ as.numeric(as.character(.x))),
      across(years, ~ case_when(country_iso3 %in% c("USA","DEU") & is.na(.x) ~ 0, TRUE ~ .x))
    ) %>%
    pivot_longer(cols = all_of(years), names_to = "year",
                 names_transform = list(year = as.integer), values_to = "value") %>%
    mutate(
      value = case_when(unit == "kl" ~ value / 1000, unit == "1000 barrels" ~ value * 0.1589873, TRUE ~ value),
      unit  = case_when(unit %in% c("kl","1000 barrels") ~ "Ml", TRUE ~ unit)
    ) %>%
    rename(source = `Source(s)`)
}

im_own <- clean_own(im_own) %>%
  rename(importer_iso3 = country_iso3, exporter_iso3 = origin_country_iso3)
ex_own <- clean_own(ex_own) %>%
  rename(exporter_iso3 = country_iso3, importer_iso3 = destination_country_iso3)

bilateral_im <- subset(im_own, exporter_iso3 != "Total")
bilateral_ex <- subset(ex_own, importer_iso3 != "Total")

# Keep both Imports and Exports — no prioritization yet
bilateral_trade_own <- bind_rows(bilateral_im, bilateral_ex) %>%
  rename(FLOW = data_type) %>%
  filter(!is.na(value)) %>%
  select(-subtype)

total_im    <- subset(im_own, exporter_iso3 == "Total")
total_ex    <- subset(ex_own, importer_iso3 == "Total")
total_trade <- bind_rows(total_im, total_ex) %>% bind_rows(SWE_cleaned_df)


###########################################################
#4. ComTrade data #########
###########################################################

# --- Step 1: shared cleaning (ISO mapping, ROW recoding, filtering) ---
bilateral_comtrade_base <- bilateral_comtrade %>%
  select(-c(type_code:ref_month, reporter_desc, flow_code, partner_desc,
            partner2code:is_original_classification, cmd_desc:qty_unit_code,
            is_qty_estimated:gross_wgt, is_gross_wgt_estimated,
            cifvalue, fobvalue, legacy_estimation_flag:is_aggregate)) %>%
  mutate(
    importer_iso3 = case_when(
      flow_desc == "Export" ~ partner_iso, flow_desc == "Import" ~ reporter_iso, TRUE ~ reporter_iso),
    exporter_iso3 = case_when(
      flow_desc == "Export" ~ reporter_iso, flow_desc == "Import" ~ partner_iso, TRUE ~ partner_iso),
    importer_iso3 = case_when(
      importer_iso3 %in% c("AND","BMU","BTN","GRL","COM","PLW","TON","PSE","SYC","CYM","MSR") ~ "ROW",
      importer_iso3 == "ABW" ~ "ANT", TRUE ~ importer_iso3),
    exporter_iso3 = case_when(
      exporter_iso3 %in% c("AND","BMU","BTN","GRL","COM","PLW","TON","PSE","SYC","CYM","MSR") ~ "ROW",
      exporter_iso3 == "ABW" ~ "ANT", TRUE ~ exporter_iso3)
  ) %>%
  rename(year = period, product = cmd_code) %>%
  mutate(product = as.integer(product), year = as.integer(year)) %>%
  filter(if_all(c(importer_iso3, exporter_iso3), ~ .x %in% c(iso_regions, "ROW")), year <= 2022)

# --- Step 2a: prices subset (both monetary and physical, no rescaling) ---
prices_comtrade <- bilateral_comtrade_base %>%
  rename(qty = qty, value_usd = primary_value, unit = qty_unit_abbr, FLOW = flow_desc) %>%
  mutate(source = "ComTrade") %>%
  select(-partner_code, -reporter_code, -reporter_iso, -partner_iso)

# --- Step 2b: quantity-only pipeline (existing behaviour) ---
bilateral_comtrade_clean <- bilateral_comtrade_base %>%
  select(-primary_value) %>%
  rename(value = qty, unit = qty_unit_abbr) %>%
  mutate(
    value     = value / 1000000,
    unit      = "kt",
    est_blend = ifelse(product == 271020, 0.05, NA),
    source    = "ComTrade",
    FLOW      = flow_desc
  ) %>%
  select(-partner_code, -reporter_code, -reporter_iso, -partner_iso, -flow_desc)




###########################################################
#5. BACI trade data #########
###########################################################

regions_temp <- regions %>%
  mutate(baci = case_when(
    code == 999    ~ 999, 
    baci == 380 ~ 381, # correcting Italy
    baci == 710 ~ 711, # correcting South Africa
    TRUE ~ baci))

clean_baci <- function(df) {
  df %>%
    filter(k %in% c(220710,220720,271019,271020,290919,382490,382600,230330) & t %in% 2010:2022) %>%
    mutate(across(c(i, j), ~ case_when(
      .x == 380 ~ 381, .x == 710 ~ 711,
      .x %in% c(531,533,534,535) ~ 530,
      !(.x %in% baci_regions) ~ 999, TRUE ~ .x))) %>%
    group_by(t, i, j, k) %>%
    summarize(q = sum(q) / 1000, v = sum(v) / 1000, unit = "kt", .groups = "drop")
}

# --- Biofuels: HS07 ---
# baci_hs07_base <- clean_baci(baci_hs07) %>%
#   filter(k %in% c(220710,220720,290919,382490) & t %in% 2010:2011)
# 
# baci_hs07_base %<>%
#   left_join(regions_temp %>% select(iso3c, baci), by = c("i" = "baci")) %>% rename(exporter_iso3 = iso3c) %>%
#   left_join(regions_temp %>% select(iso3c, baci), by = c("j" = "baci")) %>% rename(importer_iso3 = iso3c)
# 
# prices_baci_hs07 <- baci_hs07_base %>%
#   rename(year = t, product = k, exporter = i, importer = j, qty = q, value_Musd = v)
# 
# baci_hs07_clean <- baci_hs07_base %>%
#   select(-v) %>%
#   rename(year = t, product = k, exporter = i, importer = j, value = q)
# 
# baci_hs07_clean <- baci_hs07_clean %>% 
#   mutate(est_blend = case_when(product == 382490 ~ est_blend_avg$avg[est_blend_avg$year==2012],
#                                                                     TRUE ~ NA),
#                                               source = "BACI")

# --- Biofuels: HS12 ---

baci_hs12_base <- clean_baci(baci_hs12) %>%
  filter(k %in% c(220710,220720,271019,271020,290919,382600,230330) & t %in% 2012:2022)

baci_hs12_base %<>%
  left_join(regions_temp %>% select(iso3c, baci), by = c("i" = "baci")) %>% rename(exporter_iso3 = iso3c) %>%
  left_join(regions_temp %>% select(iso3c, baci), by = c("j" = "baci")) %>% rename(importer_iso3 = iso3c)

prices_baci_hs12 <- baci_hs12_base %>%
  rename(year = t, product = k, exporter = i, importer = j, qty = q, value_Musd = v)

baci_hs12_clean <- baci_hs12_base %>%
  select(-v) %>%
  rename(year = t, product = k, exporter = i, importer = j, value = q)

baci_hs12_clean %<>%
  left_join(estimated_blend_by_exporter %>% select(year, exporter_iso3, est_blend), by = c("exporter_iso3","year")) %>%
  mutate(est_blend = ifelse(product == 382600, est_blend, NA_real_)) %>%
  left_join(est_blend_avg %>% select(year, avg_blend = avg), by = "year") %>%
  mutate(
    est_blend = case_when(
      product == 382600 & is.na(est_blend) ~ avg_blend,
      product == 271020                    ~ 0.05,
      TRUE                                 ~ est_blend),
    source = "BACI") %>%
  select(-avg_blend)




###########################################################
########### MAKING FULL BILATERAL TRADE FLOWS DATASET #########
###########################################################

###########################################################
#1. Bringing BACI & ComTrade together (complementary for Biodiesel product) #########
###########################################################

baci_comtrade_named <- bind_rows(baci_hs12_clean, bilateral_comtrade_clean) %>%
  mutate(
    est_blend    = replace(est_blend, is.na(est_blend) & product == 382600L, 0),
    product_code = product,
    product = case_when(
      product_code == 230330                      ~ "Dried distillers grains with solubles",
      product_code == 152000                      ~ "Glycerol, crude",
      product_code %in% c(271020, 382600, 382490) ~ "Biodiesel",
      product_code %in% c(220710, 220720)         ~ "Bioethanol",
      product_code == 290531                      ~ "MEG",
      product_code == 290919                      ~ "ETBE",
      product_code == 291030                      ~ "Epichlorohydrin",
      product_code == 291811                      ~ "Lactic acid",
      product_code == 390770                      ~ "Polylactic acid",
      product_code %in% c(391211, 391212)         ~ "Cellulose acetate"
    ),
    value = case_when(
      product == "Biodiesel" ~ est_blend * value,
      product == "ETBE"      ~ 0.45 * value,
      TRUE                   ~ value
    ),
    FLOW = coalesce(FLOW, "BACI")
  ) %>%
  group_by(product, year, importer_iso3, exporter_iso3, FLOW, source) %>%
  summarise(value = sum(value, na.rm = TRUE), unit = "kt", .groups = "drop") %>%
  filter(!is.na(value), !is.na(product))


###########################################################
#2. Binding all sources together  #########
###########################################################

# source_priority encodes the global hierarchy used in the final slice:
#   1 = Own collection Imports
#   2 = Eurostat IMPORT
#   3 = Own collection Exports
#   4 = Eurostat EXPORT
#   5 = ComTrade Import
#   6 = BACI
#   7 = ComTrade Export

trade_all <- bind_rows(
  bilateral_bf_bp_eu %>%
    mutate(source_priority = case_when(FLOW == "IMPORT" ~ 2L, TRUE ~ 4L)),
  bilateral_trade_own %>%
    mutate(source_priority = case_when(FLOW == "Imports" ~ 1L, TRUE ~ 3L)),
  baci_comtrade_named %>%
    mutate(source_priority = case_when(
      source == "ComTrade" & FLOW == "Import" ~ 5L,
      source == "BACI"                        ~ 6L,
      source == "ComTrade" & FLOW == "Export" ~ 7L,
      TRUE                                    ~ 6L)),
  us_imports_clean %>%
    mutate(source_priority = 0)
) %>%
  filter(year <= 2022) %>%
  mutate(across(c(exporter_iso3, importer_iso3),
                ~ ifelse(is.na(.x) | !(.x %in% regions$iso3c), "ROW", .x)))

###########################################################
#3. Slicing of duplicates following the hierarchy of sources  #########
###########################################################

btd <- trade_all %>%
  group_by(exporter_iso3, importer_iso3, product, year) %>%
  slice_min(order_by = source_priority, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-source_priority) %>%
  arrange(importer_iso3, exporter_iso3, product, year)

btd_excluded <- trade_all %>%
  anti_join(btd,
            by = c("exporter_iso3","importer_iso3","product","year","FLOW","source","value")) %>%
  arrange(importer_iso3, exporter_iso3, product, year)





###########################################################
########### DERIVING PRICES #########
###########################################################

###########################################################
#1. Collecting yearly exchange rate EUR-USD to convert Eurostat prices to USD #########
###########################################################

eur_usd <- request("https://data-api.ecb.europa.eu/service/data/EXR/A.USD.EUR.SP00.A?format=csvdata") %>%
  req_perform() %>%
  resp_body_string() %>%
  read_csv(show_col_types = FALSE) %>%
  transmute(year = as.integer(TIME_PERIOD), eur_usd = OBS_VALUE) %>%
  filter(year %in% 2010:2022)


###########################################################
#2. Cleaning #########
###########################################################

bf_bp_eu_prices$product <- as.integer(bf_bp_eu_prices$product)
bf_bp_eu_prices <- bf_bp_eu_prices %>%
  mutate(product = case_when(
    product %in% c(38260010, 38260090)         ~ "Biodiesel",
    product == 2207                            ~ "Bioethanol",
    product == 29171310                        ~ "Sebacic acid",
    product == 29171920                        ~ "Succinic acid",
    product == 29053926                        ~ "1,4-butanediol",
    product == 29053995                        ~ "1,3-propanediol",
    product == 29012190                        ~ "Ethylene",
    product == 29012290                        ~ "Propylene",
    product == 29091910                        ~ "ETBE",
    product %in% c(39121100, 39121200)         ~ "Cellulose acetate"
  ))

str(prices_comtrade)
prices_comtrade <- prices_comtrade %>% 
  mutate(product = case_when(
    product == 152000                      ~ "Glycerol, crude",
    product %in% c(382600)                 ~ "Biodiesel",
    product %in% c(220710, 220720)         ~ "Bioethanol",
    product == 290531                      ~ "MEG",
    product == 290919                      ~ "ETBE",
    product == 291030                      ~ "Epichlorohydrin",
    product == 291811                      ~ "Lactic acid",
    product == 390770                      ~ "Polylactic acid",
    product %in% c(391211, 391212)         ~ "Cellulose acetate"
  ))

prices_baci_hs12 <- prices_baci_hs12 %>% 
  mutate(product = case_when(
    product == 230330                      ~ "Dried distillers grains with solubles",
    product %in% c(382600) ~ "Biodiesel",
    product %in% c(220710, 220720)         ~ "Bioethanol",
    product == 290919                      ~ "ETBE",
  ))

str(bf_bp_eu_prices)
str(prices_comtrade)
str(prices_baci_hs12)
str(prices_sebacic_us)

###########################################################
#3. Harmonizing datasets for bind #########
###########################################################

# --- Eurostat ---
bf_bp_eu_prices <- bf_bp_eu_prices %>%
  rename(qty = qty_100kg, value = value_eur) %>%
  mutate(qty = qty / 10, unit_qty = "t", source = "Eurostat") %>%
  left_join(eur_usd, by = "year") %>%
  mutate(value = value * eur_usd, unit_value = "USD") %>%
  select(year, product, unit_qty, unit_value, qty, value,
         importer_iso3, exporter_iso3, source)

# --- BACI HS12 ---
prices_baci_hs12 <- prices_baci_hs12 %>%
  rename(value = value_Musd) %>%
  mutate(qty        = qty*1000,
         value      = value * 1000000,
         unit_qty   = "t",
         unit_value = "USD",
         source     = "BACI") %>%
  select(year, product, unit_qty, unit_value, qty, value,
         importer_iso3, exporter_iso3, source)

# --- ComTrade ---
prices_comtrade <- prices_comtrade %>%
  rename(value = value_usd, unit_qty = unit) %>%
  mutate(qty        = qty / 1000,
         unit_qty   = "t",
         unit_value = "USD") %>%
  select(year, product, unit_qty, unit_value, qty, value,
         importer_iso3, exporter_iso3, source)

# --- US Census (Sebacic acid) ---
prices_sebacic_us <- prices_sebacic_us %>%
  rename(qty = value, value = monetary_value, unit_qty = unit) %>%
  mutate(qty        = qty * 1000,
         unit_qty   = "t",
         unit_value = "USD",
         source     = "US Census") %>%
  select(year, product, unit_qty, unit_value, qty, value,
         importer_iso3, exporter_iso3, source)

###########################################################
#4. Binding and calculating average exporter prices by product-year #########
###########################################################

prices <- bind_rows(bf_bp_eu_prices, prices_baci_hs12, prices_comtrade, prices_sebacic_us) %>%
  filter(! product %in% c("MEG", "1,3-propanediol"),
         ! is.na(value),
         ! is.na(qty),
         qty > 10, 
         value > 100,
         ! is.na(product)) %>%
  mutate(unit_price = value / qty) %>%
  filter(is.finite(unit_price))

# prices <- bind_rows(bf_bp_eu_prices, prices_baci_hs12, prices_comtrade, prices_sebacic_us) %>%
#   filter(! product %in% c("MEG", "1,3-propanediol"),
#          ! is.na(value),
#          ! is.na(qty),
#          qty > 10,
#          value > 100,
#          year %in% 2012:2022) %>%
# mutate(unit_price = value / qty) %>%
#   group_by(year, product) %>%
#   mutate(mean_price = mean(unit_price, na.rm = TRUE),
#          sd_price   = sd(unit_price, na.rm = TRUE)) %>%
#   filter(unit_price >= mean_price - sd_price,
#          unit_price <= mean_price + sd_price) %>%
#   ungroup() %>%
#   mutate(product = ifelse(product %in% c("Bioethanol", "ETBE"), "Biogasoline", product)) %>%
#   group_by(year, product, exporter_iso3) %>%
#   summarise(qty   = sum(qty),
#             value = sum(value),
#             .groups = "drop") %>%
#   mutate(price = value / qty)
# 
# caps <- prices %>%
#   group_by(product) %>%
#   summarise(
#     price_max = max(price, na.rm = TRUE),
#     price_q95 = quantile(price, .95, na.rm = TRUE),
#     price_q90 = quantile(price, .90, na.rm = TRUE),
#     price_q80 = quantile(price, .80, na.rm = TRUE),
#     price_q50 = quantile(price, .50, na.rm = TRUE),
#     price_q20 = quantile(price, .20, na.rm = TRUE),
#     price_q10 = quantile(price, .10, na.rm = TRUE),
#     price_q05 = quantile(price, .05, na.rm = TRUE),
#     price_min = min(price, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# prices <- left_join(prices, caps, by = "product")


# --- DDGS name alias ------------------------------------------------------------
# The trade data calls it "Dried distillers grains with solubles"; items_full_bcp calls
# item 654 "Brewing or distilling dregs and waste" (-> c171). The name join therefore
# MISSES, and 148.8 Mt of DDGS trade across 5,196 rows ends up with item_code = NA and
# comm_code = NA — a bilateral flow of *nothing*, which 11's ghost check then reports as
# 18 Mt of countries shipping an unnamed commodity.
BCP_ITEM_ALIAS <- data.table(
  item_alias = c("Dried distillers grains with solubles"),
  item_code  = 654L,
  comm_code  = "c171")

setDT(btd_final_bcp)
btd_final_bcp[BCP_ITEM_ALIAS, on = .(item = item_alias),
              `:=`(item_code = fcoalesce(item_code, i.item_code),
                   comm_code = fcoalesce(comm_code, i.comm_code))]

miss <- btd_final_bcp[is.na(comm_code) | comm_code == "",
                      .(t = sum(value), n = .N), by = .(item_code, item)]
fabio_assert(nrow(miss) == 0,
             "07_04: %d btd_final_bcp item(s) still have no comm_code — they become bilateral flows of nothing.",
             nrow(miss), data = miss)


###########################################################
########### WRITING DATA TABLES #########
###########################################################


setwd(fabio_root)

# Trade 
saveRDS(total_trade,   "intermediate_data/btd_total.rds")
saveRDS(btd,           "intermediate_data/btd_intermediate.rds")
saveRDS(btd_excluded,  "intermediate_data/btd_excluded_flows.rds")

# Prices
saveRDS(prices, "intermediate_data/prices_bcp.rds")

# Removing temporary objects

rm(list = ls())