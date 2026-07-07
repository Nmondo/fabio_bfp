###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(data.table)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(janitor)
library(zoo)
library(httr2)
library(comtradr)
library(censusapi)
library(purrr)

veg_oil_code <- 2571:2582

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
setwd(fabio_root)


regions <- fread("inst/regions.csv")

###########################################################
# Collecting yearly exchange rate EUR-USD to convert Eurostat prices to USD #########
###########################################################

eur_usd <- request("https://data-api.ecb.europa.eu/service/data/EXR/A.USD.EUR.SP00.A?format=csvdata") %>%
  req_perform() %>%
  resp_body_string() %>%
  read_csv(show_col_types = FALSE) %>%
  transmute(year = as.integer(TIME_PERIOD), eur_usd = OBS_VALUE) %>%
  filter(year %in% 2010:2022)



###########################################################
########### BTD FOR THE FAOSTAT PRODUCTS #########
###########################################################

btd <- readRDS("data/tidy/btd_tidy.rds")
btd <- btd[item_code %in% c(veg_oil_code, "2737"), ]
prices <- dcast(
  btd[unit %in% c("usd", "tonnes", "An", "1000 An")],
  from + from_code + to + to_code + item + item_code + year ~ unit,
  value.var = "value", fun.aggregate = sum, na.rm = TRUE, fill = 0
)

# Keep only bilateral flows with positive USD and at least one positive qty
prices <- prices[usd > 0 & tonnes > 0]
prices <- prices[, cat := fcase(
  item_code == 2737, "Animal fats (non-waste)",
  default = "Vegetable oil (non-waste)"
)]

prices_exporter <- prices[, .(
  usd       = sum(usd,       na.rm = TRUE),
  tonnes    = sum(tonnes,    na.rm = TRUE)
), by = .(from_code, cat, year)]

prices_exporter[, price := fcase(
  tonnes    > 0, usd / tonnes,
  default = NA_real_
)]

prices_exporter <- prices_exporter[
  is.finite(price) & price > 0,
  .(from_code, cat, year, price)
]




###########################################################
########### EUROSTAT QUERY FOR WASTE #########
###########################################################

url_data <- paste0(
  "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/3.0/",
  "data/dataflow/ESTAT/ds-045409/1.0/*.*.*.*.*.*",
  "?c[freq]=A",
  "&c[reporter]=AT,BE,BG,CY,CZ,DE,DK,EE,ES,FI,FR,GR,HR,HU,IE,IT,LT,LU,LV,MT,NL,PL,PT,RO,SE,SI,SK",
  "&c[partner]=AD,AE,AL,AM,AN,AR,AT,AU,AW,BA,BD,BE,BG,BH,BR,BY,BZ,CA,CH,CL,CN,CY,CZ,DE,DK,EE,EG,",
  "ES,FI,FR,GB,GI,GR,HK,HR,HU,ID,IE,IL,IN,IS,IT,JP,KR,KW,LA,LK,LT,LU,LV,MO,MT,MY,NL,NO,OM,PH,",
  "PK,PL,PT,QA,RO,RU,SA,SE,SG,SI,SK,SX,TH,TR,TW,UA,US,VN,XS,ZA",
  "&c[product]=15180031,15180039,15180095",
  "&c[flow]=1,2",
  "&c[indicators]=QUANTITY_IN_100KG,VALUE_IN_EUROS",
  "&c[TIME_PERIOD]=2022,2021,2020,2019,2018,2017,2016,2015,2014,2013,2012,2011,2010",
  "&compress=false&format=csvdata&formatVersion=1.0&lang=en&labels=both")

trade_waste_oils <- request(url_data) %>%
  req_timeout(120) %>%
  req_retry(max_tries = 100) %>%
  req_perform() %>%
  resp_body_string() %>%
  read_csv(show_col_types = FALSE)

trade_waste_oils %<>%
  mutate(across(c(reporter, partner, product, indicators), ~ sub(":.*", "", .x)),
         across(c(freq, flow),                             ~ sub(".*:", "", .x))) %>%
  rename(FLOW = flow)

# --- Shared cleaning (both indicators pass through) ---
trade_waste_oils <- trade_waste_oils %>%
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
trade_waste_oils <- trade_waste_oils %>%
  pivot_wider(names_from = indicators, values_from = OBS_VALUE) %>%
  mutate(qty_ton = QUANTITY_IN_100KG/10, 
         cat = ifelse(product %in% c("15180031", "15180039"), "Vegetable oil (waste)", "Animal fats (waste)")) %>%
  rename(value_eur = VALUE_IN_EUROS) %>%
  select(-DATAFLOW, -`LAST UPDATE`,-QUANTITY_IN_100KG, -exporter_iso2, -importer_iso2) 

setDT(trade_waste_oils)

prices_exporter_waste <- trade_waste_oils[, .(
  value_eur = sum(value_eur,       na.rm = TRUE),
  qty_ton   = sum(qty_ton,    na.rm = TRUE)
), by = .(exporter_iso3, cat, year)]

prices_exporter_waste[, price := value_eur/qty_ton]

prices_exporter_waste <- prices_exporter_waste[
  is.finite(price) & price > 0,
  .(exporter_iso3, cat, year, price)
]

prices_exporter_waste <- 
  prices_exporter_waste %>%
  left_join(eur_usd, by = "year") %>%
  mutate(price = price * eur_usd) %>% 
  select(-eur_usd)


# Replace country codes by iso3
prices_exporter_waste <- prices_exporter_waste %>%
  left_join(regions %>% select(from_code = code, iso3c), by = c("exporter_iso3" = "iso3c")) %>%
  select(-exporter_iso3)




###########################################################
########### MERGE #########
###########################################################

prices_oils <- rbind(prices_exporter, prices_exporter_waste)

setDT(prices_oils)

make_density_plot <- function(dt, cats, title) {
  d <- dt[cat %chin% cats & is.finite(price)]
  ggplot(d, aes(x = price, fill = cat, colour = cat)) +
    geom_density(alpha = 0.4, na.rm = TRUE) +
    labs(title = title, x = "price", y = "density", fill = NULL, colour = NULL) +
    theme_minimal() +
    scale_x_log10() +
    theme(legend.position = "bottom")
}

p_fats <- make_density_plot(
  prices_oils,
  c("Animal fats (waste)", "Animal fats (non-waste)"),
  "Animal fats"
)

p_veg <- make_density_plot(
  prices_oils,
  c("Vegetable oil (waste)", "Vegetable oil (non-waste)"),
  "Vegetable oil"
)

p_fats | p_veg



