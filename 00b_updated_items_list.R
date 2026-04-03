###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

########### FABIO items (initial) #########

items <- read.csv("inst/items_full_123.csv")
items_supply <- read.csv("inst/items_supply.csv")
items_use <- read.csv("inst/items_use.csv")




###########################################################
########### MAKING NEW ITEMS #########
###########################################################

items_extension <- data.frame(
  comm_code  = c("c141","c142","c143","c144","c145","c146","c147","c148","c149","c150",
                 "c151","c152","c153","c154","c155","c156","c157","c158","c159","c160",
                 "c161","c162","c163","c164","c165","c166","c167","c168","c169","c170",
                 "c901","c999"),
  item_code  = c(97, 165, 265, 266, 1274, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA),
  item       = c("Triticale","Molasses","Castor oil seeds","Castor oil","Used cooking oil","Biogasoline",
                 "Biodiesel","Glycerol, crude","Renewable diesel","Biopropane","Bionaphtha",
                 "L-LA","Sebacic acid","Succinic acid","11-AA","Ethylene",
                 "Propylene","1,3-PDO","1,4-BDO","MEG","MPG",
                 "ECH","PLA","PE","PP",
                 "PTT","PET","PBAT","PBS","Epoxy resins",
                 "Other, Waste","Other, Unknown"),
  unit       = c("tonnes","tonnes","tonnes","tonnes","tonnes","1000 liters",
                 "1000 liters","tonnes","1000 liters","1000 liters","1000 liters",
                 rep("tonnes", 21)),
  comm_group = c("Cereals","Sugar, sweeteners","Oil crops","Vegetable oils","Waste",
                 "Biofuels","Biofuels","Building blocks","Biofuels","Biofuels","Biofuels",
                 "Building blocks","Building blocks","Building blocks","Building blocks","Building blocks",
                 "Building blocks","Building blocks","Building blocks","Building blocks","Building blocks",
                 "Building blocks","Biopolymers","Biopolymers","Biopolymers",
                 "Biopolymers","Biopolymers","Biopolymers","Biopolymers","Biopolymers",
                 "Waste","Unknown"),
  stringsAsFactors = FALSE
)

items_supply_extension <- data.frame(
  comm_code = c("c141","c142","c143","c144","c145","c146","c147","c148","c149","c150",
                "c151","c152","c153","c154","c155","c156","c157","c158","c159","c160",
                "c161","c162","c163","c164","c165","c166","c167","c168","c169","c170",
                "c901","c999"),
  item_code = c(97, 165, 265, 266, 1274, rep(NA, 27)),
  item      = c("Triticale","Molasses","Castor oil seeds","Castor oil","Used cooking oil","Biogasoline",
                "Biodiesel","Glycerol, crude","Renewable diesel","Biopropane","Bionaphtha",
                "L-LA","Sebacic acid","Succinic acid","11-AA","Ethylene",
                "Propylene","1,3-PDO","1,4-BDO","MEG","MPG",
                "ECH","PLA","PE","PP",
                "PTT","PET","PBAT","PBS","Epoxy resins",
                "Other, Waste","Other, Unknown"),
  proc_code = c("p121","p065","p122","p123","p124","p125","p126","p126","p127","p127","p127",
                "p128","p129","p130","p131","p132","p133","p134","p135","p136","p137",
                "p138","p139","p140","p141","p142","p143","p144","p145","p146",
                "p901","p999"),
  proc      = c("Triticale production","Sugar production","Castor oil seeds production",
                "Castor oil production","Used cooking oil collection","Biogasoline production",
                "Biodiesel production","Biodiesel production",
                "Renewable diesel production","Renewable diesel production","Renewable diesel production",
                "Lactic acid production","Sebacic acid production","Succinic acid production",
                "11-AA production","Ethylene production","Propylene production",
                "1,3-PDO production","1,4-BDO production","MEG production","MPG production",
                "ECH production","PLA production","PE production","PP production",
                "PTT production","PET production","PBAT production","PBS production","Epoxy resins production",
                "Other, Waste collection","Other, Unknown production"),
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
                "c069","c071","c072","c073","c074","c075","c076","c080","c120","c145","c901","c999",
                "c069","c072","c075","c080","c120","c145","c901","c999",
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
                "Used cooking oil","Other, Waste","Other, Unknown",
                "Soyabean Oil","Rape and Mustard Oil","Palm Oil","Maize Germ Oil","Fats, Animals, Raw",
                "Used cooking oil","Other, Waste","Other, Unknown",
                "Rice and products","Wheat and products","Maize and products","Potatoes and products",
                "Cassava and products","Sugar cane","Sugar beet","Glycerol, crude",
                "Castor oil",
                "Wheat and products","Maize and products","Potatoes and products",
                "Castor oil",
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

items_full_bcp <- bind_rows(items, items_extension) %>%
  filter(comm_code != "c095") %>%
  mutate(comm_group = ifelse(item == "Fats, Animals, Raw", "Waste", comm_group)) # change units for Biopropane and Bionaphtha

items_supply_bcp <- bind_rows(items_supply, items_supply_extension) %>%
  filter(proc_code != "p084")

items_use_bcp <- bind_rows(items_use, items_use_extension) %>%
  filter(proc_code != "c084")


###########################################################
########### SAVING #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

write_csv(items_full_bcp, "inst/items_full_bcp.csv")
write_csv(items_supply_bcp, "inst/items_supply_bcp.csv")
write_csv(items_use_bcp, "inst/items_use_bcp.csv")

rm(list = ls())