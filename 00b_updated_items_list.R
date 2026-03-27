###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)

###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/bruckner2/fabio/")

########### FABIO items (initial) #########

items <- read.csv("inst/items_full_123.csv") ## this has 123 products 
items_supply <- read.csv("inst/items_supply.csv") ## this has 125 products with non-matching codes for items.




###########################################################
########### MAKING NEW ITEMS #########
###########################################################

items_extension <- data.frame(
  comm_code  = c("c141","c142","c143","c144","c145","c146","c147","c148","c149","c150",
                 "c151","c152","c153","c154","c155","c156","c157","c158","c159","c160",
                 "c161","c162","c163","c164","c165","c166","c167","c168","c169","c170","c171"),
  item_code  = c(97, 165, 266, 1274, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  item       = c("Triticale","Molasses","Castor oil","Used cooking oil","Biogasoline",
                 "Biodiesel","Glycerol, crude","Renewable diesel","Biopropane","Bionaphtha",
                 "L-LA","Sebacic acid","11-AA","Ethylene","Propylene",
                 "1,3-PDO","1,4-BDO","MEG","MPG","ECH",
                 "PLA","PA","PE","PP","PTT","PET","PBAT","PBS",
                 "Epoxy resins","Cellulose acetate","PUR"),
  unit       = c("tonnes","tonnes","tonnes","tonnes","1000 liters",
                 "1000 liters","tonnes","1000 liters","tonnes","tonnes",
                 rep("tonnes", 21)),
  comm_group = c("Cereals","Sugar, sweeteners","Oil crops","Waste",
                 "Biofuels","Biofuels","Building blocks","Biofuels","Biofuels","Biofuels",
                 "Building blocks","Building blocks","Building blocks","Building blocks","Building blocks",
                 "Building blocks","Building blocks","Building blocks","Building blocks","Building blocks",
                 "Biopolymers","Biopolymers","Biopolymers","Biopolymers","Biopolymers",
                 "Biopolymers","Biopolymers","Biopolymers","Biopolymers","Biopolymers","Biopolymers"),
  stringsAsFactors = FALSE
)

processes_extension <- data.frame(
  comm_code = c("c141","c142","c143","c144","c145","c146","c147","c148","c149","c150",
                "c151","c152","c153","c154","c155","c156","c157","c158","c159","c160",
                "c161","c162","c163","c164","c165","c166","c167","c168"),
  item_code = c(97, 165, 266, 1274, rep(NA, 24)),
  item      = c("Triticale","Molasses","Castor oil","Used cooking oil","Biogasoline",
                "Biodiesel","Glycerol, crude","Renewable diesel","Biopropane","Bionaphtha",
                "L-LA","Sebacic acid","11-AA","Ethylene","Propylene",
                "1,3-PDO","1,4-BDO","MEG","MPG","ECH",
                "PLA","PA","PE","PP","PTT","PET","PBAT","PBS"),
  proc_code = c("p121","p065","p122","p123","p124","p125","p125","p126","p126","p126",
                "p127","p128","p129","p130","p131","p132","p133","p134","p135","p136",
                "p137","p138","p139","p140","p141","p142","p143","p144"),
  proc      = c("Triticale production","Sugar production","Castor oil production",
                "Used cooking oil collection","Biogasoline production",
                "Biodiesel production","Biodiesel production",
                "Renewable diesel production","Renewable diesel production","Renewable diesel production",
                "Lactic acid production","Sebacic acid production","11-AA production",
                "Ethylene production","Propylene production","1,3-PDO production",
                "1,4-BDO production","MEG production","MPG production","ECH production",
                "PLA production","PA production","PE production","PP production",
                "PTT production","PET production","PBAT production","PBS production"),
  stringsAsFactors = FALSE
)

items_full_nonfood_ext <- bind_rows(items, items_extension) %>%
  filter(comm_code != "c095")
items_supply_nonfood_ext <- bind_rows(items_supply, processes_extension) %>%
  filter(proc_code != "p084",
         comm_code != "c095")




###########################################################
########### SAVING #########
###########################################################

setwd("/home/mmondolfo/")

saveRDS(items_full_nonfood_ext, "items_full_nonfood.rds")
saveRDS(items_supply_nonfood_ext, "items_supply_nonfood.rds")

