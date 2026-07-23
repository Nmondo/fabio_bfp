###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)

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

########### FABIO items (initial) #########

items <- read.csv("inst/items_full_123.csv")
items_supply <- read.csv("inst/items_supply.csv")
items_use <- read.csv("inst/items_use.csv")

print(items[, c("comm_code", "item_code", "item")])


###########################################################
########### MAKING NEW ITEMS #########
###########################################################

items_extension <- data.frame(
  comm_code  = c("c141","c142","c143","c144","c145","c146","c147","c148","c149","c150",
                 "c151","c152","c153","c154","c155","c156","c157","c158","c159","c160",
                 "c161","c162","c163","c164","c165","c166","c167","c168","c169","c170",
                 "c171","c901","c999"),
  item_code  = c(97, 165, 265, 266, 1274, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 654, NA, NA),
  item       = c("Triticale","Molasses","Castor oil seeds","Oil of castor beans","Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils","Biogasoline",
                 "Biodiesel","Glycerol, crude","Renewable diesel","Biopropane","Bionaphtha",
                 "L-LA","Sebacic acid","Succinic acid","11-AA","Ethylene",
                 "Propylene","1,3-PDO","1,4-BDO","MEG","MPG",
                 "ECH","PLA","PE","PP",
                 "PTT","PET","PBAT","PBS","Epoxy resins",
                 "Brewing or distilling dregs and waste","Other, Waste","Other, Unknown"),
  unit       = c("tonnes","tonnes","tonnes","tonnes","tonnes","1000 liters",
                 "1000 liters","tonnes","1000 liters","1000 liters","1000 liters",
                 rep("tonnes", 22)),
  comm_group = c("Cereals","Sugar, sweeteners","Oil crops","Vegetable oils","Waste",
                 "Biofuels","Biofuels","Building blocks","Biofuels","Biofuels","Biofuels",
                 "Building blocks","Building blocks","Building blocks","Building blocks","Building blocks",
                 "Building blocks","Building blocks","Building blocks","Building blocks","Building blocks",
                 "Building blocks","Biopolymers","Biopolymers","Biopolymers",
                 "Biopolymers","Biopolymers","Biopolymers","Biopolymers","Biopolymers",
                 NA,"Waste","Unknown"),
  stringsAsFactors = FALSE
)

items_supply_extension <- data.frame(
  comm_code = c("c141","c142","c143","c144","c145","c146","c147","c148","c149","c150",
                "c151","c152","c153","c154","c155","c156","c157","c158","c159","c160",
                "c161","c162","c163","c164","c165","c166","c167","c168","c169","c170",
                "c171","c901","c999"),
  item_code = c(97, 165, 265, 266, 1274, rep(NA, 25), 654, NA, NA),
  item      = c("Triticale","Molasses","Castor oil seeds","Oil of castor beans","Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils","Biogasoline",
                "Biodiesel","Glycerol, crude","Renewable diesel","Biopropane","Bionaphtha",
                "L-LA","Sebacic acid","Succinic acid","11-AA","Ethylene",
                "Propylene","1,3-PDO","1,4-BDO","MEG","MPG",
                "ECH","PLA","PE","PP",
                "PTT","PET","PBAT","PBS","Epoxy resins",
                "Brewing or distilling dregs and waste","Other, Waste","Other, Unknown"),
  proc_code = c("p121","p065","p122","p123","p124","p125","p126","p126","p127","p127","p127",
                "p128","p129","p130","p131","p132","p133","p134","p135","p136","p137",
                "p138","p139","p140","p141","p142","p143","p144","p145","p146",
                "p125","p901","p999"),
  proc      = c("Triticale production","Sugar production","Castor oil seeds production",
                "Castor oil production","Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils collection","Biogasoline production",
                "Biodiesel production","Biodiesel production",
                "Renewable diesel production","Renewable diesel production","Renewable diesel production",
                "Lactic acid production","Sebacic acid production","Succinic acid production",
                "11-AA production","Ethylene production","Propylene production",
                "1,3-PDO production","1,4-BDO production","MEG production","MPG production",
                "ECH production","PLA production","PE production","PP production",
                "PTT production","PET production","PBAT production","PBS production","Epoxy resins production",
                "Biogasoline production","Other, Waste collection","Other, Unknown production"),
  stringsAsFactors = FALSE
)
items_use_extension <- data.frame(
  proc_code = c("p123",
                rep("p125", 13),
                rep("p126", 12),
                rep("p127", 8),
                rep("p128", 8),
                "p129",
                rep("p130", 3),
                "p131",
                rep("p132", 2),
                rep("p133", 2),
                rep("p134", 2),
                rep("p135", 6),
                "p136","p137","p138","p139","p140","p141","p142","p143","p144","p145","p146"),
  proc      = c("Castor oil production",
                rep("Biogasoline production", 13),
                rep("Biodiesel production", 12),
                rep("Renewable diesel production", 8),
                rep("Lactic acid production", 8),
                "Sebacic acid production",
                rep("Succinic acid production", 3),
                "11-AA production",
                rep("Ethylene production", 2),
                rep("Propylene production", 2),
                rep("1,3-PDO production", 2),
                rep("1,4-BDO production", 6),
                "MEG production","MPG production","ECH production","PLA production",
                "PE production","PP production","PTT production","PET production",
                "PBAT production","PBS production","Epoxy resins production"),
  comm_code = c("c143",
                "c001","c002","c003","c004","c005","c008","c011","c015","c016","c141","c142","c901","c999",
                "c068","c070","c071","c072","c073","c074","c075","c079","c119","c145","c901","c999",
                "c068","c071","c074","c079","c119","c145","c901","c999",
                "c001","c002","c004","c010","c011","c015","c016","c148",
                "c144",
                "c002","c004","c010",
                "c144",
                "c146","c151",
                "c150","c151",
                "c004","c148",
                "c001","c002","c004","c010","c011","c154",
                "c156","c148","c148","c152","c156","c157","c158","c160","c159","c154","c162"),
  item_code = c(265,
                2807,2511,2513,2514,2515,2518,2532,2536,2537,97,165,NA,NA,
                2571,2573,2574,2575,2576,2577,2578,2582,2737,1274,NA,NA,
                2571,2574,2577,2582,2737,1274,NA,NA,
                2807,2511,2514,2531,2532,2536,2537,NA,
                266,
                2511,2514,2531,
                266,
                NA,NA,
                NA,NA,
                2514,NA,
                2807,2511,2514,2531,2532,NA,
                NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
  item      = c("Castor oil seeds",
                "Rice and products","Wheat and products","Barley and products","Maize and products",
                "Rye and products","Sorghum and products","Cassava and products","Sugar cane","Sugar beet",
                "Triticale","Molasses","Other, Waste","Other, Unknown",
                "Soyabean Oil","Sunflowerseed Oil","Rape and Mustard Oil","Cottonseed Oil",
                "Palmkernel Oil","Palm Oil","Coconut Oil","Maize Germ Oil","Fats, Animals, Raw",
                "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils","Other, Waste","Other, Unknown",
                "Soyabean Oil","Rape and Mustard Oil","Palm Oil","Maize Germ Oil","Fats, Animals, Raw",
                "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils","Other, Waste","Other, Unknown",
                "Rice and products","Wheat and products","Maize and products","Potatoes and products",
                "Cassava and products","Sugar cane","Sugar beet","Glycerol, crude",
                "Oil of castor beans",
                "Wheat and products","Maize and products","Potatoes and products",
                "Oil of castor beans",
                "Biogasoline","Bionaphtha",
                "Biopropane","Bionaphtha",
                "Maize and products","Glycerol, crude",
                "Rice and products","Wheat and products","Maize and products","Potatoes and products",
                "Cassava and products","Succinic acid",
                "Ethylene","Glycerol, crude","Glycerol, crude","L-LA","Ethylene","Propylene",
                "1,3-PDO","MEG","1,4-BDO","Succinic acid","ECH"),
  stringsAsFactors = FALSE
)


###########################################################
########### JOINING #########
###########################################################

# List of items
items_full_bcp <- bind_rows(items, items_extension) %>%
  filter(comm_code != "c094",
         ! item %in% c("Oilcrops, Other", "Oilcrops Oil, Other", "Sweeteners, Other", "Cereals, Other")) %>%
  mutate(comm_group = ifelse(item == "Fats, Animals, Raw", "Waste", comm_group)) %>% # change units for Biopropane and Bionaphtha
  mutate(moisture = case_when(item == "Triticale" ~ 0.140,
                              item == "Molasses" ~ 0.200,
                              item == "Castor oil seeds" ~ 0.080,
                              item == "Brewing or distilling dregs and waste" ~ 0.100,
                              TRUE ~ moisture),
         feedtype = case_when(item %in% c("Triticale","Molasses","Castor oil seeds") ~ "crops",
                              item == "Brewing or distilling dregs and waste" ~ "byproducts", 
                              TRUE ~ feedtype),
         # ISIC Rev.4 SECTION: A = Agriculture, forestry & fishing (raw crops);
         # C = Manufacturing (processed products); "" = none. The HS 1518 item is a
         # used-cooking-oil / waste stream -> ISIC section E, outside A/C -> none;
         # set it to "C" instead if reading it as the manufactured chem-modified oil.
         ISIC = case_when(
           !is.na(ISIC)                  ~ ISIC,
           item == "Triticale"           ~ "A",   # crop production
           item == "Castor oil seeds"    ~ "A",   # crop production
           item == "Molasses"            ~ "C",   # manufacture of sugar
           item == "Oil of castor beans" ~ "C",   # manufacture of vegetable oils
           item == "Brewing or distilling dregs and waste" ~ "C",   # manufacture of vegetable oils
           item == "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils" ~ "C",
           TRUE                          ~ ""      # incl. HS 1518 fats/oils (see note)
         ))

# List of items supplied by process
items_supply_bcp <- bind_rows(items_supply, items_supply_extension) %>%
  filter(! proc_code %in% c("p079","p084"),
         ! item %in% c("Oilcrops, Other", "Oilcrops Oil, Other", "Sweeteners, Other", "Cereals, Other"))

# List of items used by process
triticale_codes <- bind_rows(items_use, items_use_extension) %>%
  filter(item == "Triticale") %>%
  distinct(item, item_code, comm_code)

molasses_codes <- bind_rows(items_use, items_use_extension) %>%
  filter(item == "Molasses") %>%
  distinct(item, item_code, comm_code)

castor_oil_codes <- bind_rows(items_use, items_use_extension) %>%
  filter(item == "Oil of castor beans") %>%
  distinct(item, item_code, comm_code)

chemmod_oil_codes <- bind_rows(items_use, items_use_extension) %>%
  filter(item == "Animal or vegetable fats and oils and their fractions, chemically modified, except those hydrogenated, inter-esterified, re-esterified or elaidinized; inedible mixtures or preparations of animal or vegetable fats or oils") %>%
  distinct(item, item_code, comm_code)

castor_seed_codes <- bind_rows(items_use, items_use_extension) %>%
  filter(item == "Castor oil seeds") %>%
  distinct(item, item_code, comm_code)

items_use_bcp <- bind_rows(items_use, items_use_extension) %>%
  bind_rows(
    filter(., item == "Cereals, Other") %>%
      select(-item, -item_code, -comm_code) %>%
      bind_cols(triticale_codes),
    filter(., item == "Sweeteners, Other") %>%
      select(-item, -item_code, -comm_code) %>%
      bind_cols(molasses_codes),
    filter(., item == "Oilcrops Oil, Other") %>%
      select(-item, -item_code, -comm_code) %>%
      bind_cols(castor_oil_codes),
    filter(., item == "Oilcrops Oil, Other") %>%
      select(-item, -item_code, -comm_code) %>%
      bind_cols(chemmod_oil_codes),
    filter(., item == "Oilcrops, Other") %>%
      select(-item, -item_code, -comm_code) %>%
      bind_cols(castor_seed_codes)
  ) %>%
  filter(! proc_code %in% c("p066","p079","p084"),
         ! item %in% c("Oilcrops, Other", "Oilcrops Oil, Other", "Sweeteners, Other", "Cereals, Other"))

# Adding vegetable oil use in the Used cooking oil collection process
proc_p124 <- items_supply_bcp %>%
  filter(proc_code == "p124") %>%
  pull(proc) %>%
  unique()

new_rows <- items_full_bcp %>%
  filter(comm_group == "Vegetable oils" | item == "Fats, Animals, Raw",
         item != "Oil of castor beans") %>%
  distinct(item, item_code, comm_code) %>%
  mutate(proc_code = "p124",
         proc = proc_p124)

items_use_bcp <- bind_rows(items_use_bcp, new_rows) %>%
  select(-item_code.1) %>%
  arrange(proc_code, item_code)

# Castor oil crush (p123) was left with type = NA, so 10_1a never routed castor seed (c143)
# into it and c144 emerged from the supply table with an empty input column. It is an ordinary
# single-feedstock oil crush like soy (p067) / sunflower (p068) / rape (p070), all of which are
# "100%", so type it the same. The p124+ BCP-extension rows deliberately stay NA — their
# feedstock is allocated by the biofuel/biopolymer pipeline (07/08/11), not by 10_1a.
items_use_bcp <- items_use_bcp %>%
  mutate(type = ifelse(proc_code == "p123" & comm_code == "c143", "100%", type))

###########################################################
########### SAVING #########
###########################################################

setwd(fabio_root)

write_csv(items_full_bcp, "inst/items_full_bcp.csv")
write_csv(items_supply_bcp, "inst/items_supply_bcp.csv")
write_csv(items_use_bcp, "inst/items_use_bcp.csv")

rm(list = ls())