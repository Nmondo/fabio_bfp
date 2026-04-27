# 18 - Calculate footprints from FABIO MRIO

# Setup ------------------------------------------------------------------------
library(data.table)
library(Matrix)
library(tidyverse)
source("R/00_system_variables.R")

setwd("/home/mmondolfo/fabio_bfp/")

# Read labels ------------------------------------------------------------------
input_path <- "/mnt/nfs_fineprint/tmp/fabio/v2_bcp/"
regions <- fread(file="inst/regions_full.csv") %>% filter(current==TRUE)
io <- fread(paste0(input_path,"io_labels.csv"))
items <- fread(file="inst/items_full_bcp.csv") %>% filter(comm_code %in% unique(io$comm_code))
fd <- fread(file=paste0(input_path,"losses/fd_labels.csv"))
ex <- fread(file=paste0(input_path,"ex_labels.csv"))

# Set parameters ---------------------------------------------------------------
allocation <- "mass"
year <- 2021
country <- "CHN"
consumption_categories <- unique(fd$fd)
consumption <- consumption_categories[2] # 2 == fuel; 4 == other industrial
extension <- "land_harv"

# Read data --------------------------------------------------------------------
X <- readRDS(file=paste0(input_path,"losses/X.rds"))
Y <- readRDS(file=paste0(input_path,"losses/Y.rds"))
Z <- readRDS(file=paste0(input_path,"losses/Z_mass.rds"))
E <- readRDS(file=paste0(input_path,"E.rds"))
L <- readRDS(file=paste0(input_path,"losses/",year,"_L_",allocation,".rds"))
Xi <- X[, as.character(year)]
Yi <- Y[[as.character(year)]]
Zi <- Z[[as.character(year)]]
Ei <- E[[as.character(year)]]

# Prepare calculations ---------------------------------------------------------
# Prepare extension
ext <- as.numeric(Ei[ex$Stressor == extension, ]) / as.vector(Xi)
ext[!is.finite(ext)] <- 0

# Prepare final demand
if(country=="EU27"){
  Y_country <- Yi[, (fd$continent == "EU")]
  colnames(Y_country) <- fd$fd[fd$continent == "EU"]
  Y_country <- agg(Y_country)
} else {
  Y_country <- Yi[, fd$iso3c == country]
  colnames(Y_country) <- fd$fd[fd$iso3c == country]
}


# Calculate footprints ---------------------------------------------------------
MP <- ext * L
FP <- t(t(MP) * as.vector(as.matrix(Y_country[,consumption])))


# Make results data.table ------------------------------------------------------
# Convert from sparse matrix to data.table
colnames(FP) <- rownames(FP) <- paste0(io$iso3c, "_", io$item)
FP <- as(FP, "TsparseMatrix")
results <- data.table(origin=rownames(FP)[FP@i + 1], 
                      target=colnames(FP)[FP@j + 1], 
                      value =FP@x)

# Add auxiliary information
results[,`:=`(
  country_consumer = country,
  year = year,
  indicator = extension,
  country_origin = substr(origin,1,3),
  item_origin = substr(origin,5,100),
  country_target = substr(target,1,3),
  item_target = substr(target,5,100)
)]

results[,`:=`(
  group_origin = items$comm_group[match(results$item_origin,items$item)],
  group_target = items$comm_group[match(results$item_target,items$item)],
  continent_origin = regions$continent[match(results$country_origin, regions$iso3c)]
)]



# Aggregate results ------------------------------------------------------------
# by continent
# data_continent <- results %>%
#   mutate(group = case_when(
#     group_origin == "Grazing" ~ "Grazing",
#     grepl("Livestock", group_origin) ~ "Livestock",
#     TRUE ~ "Crops"
#   )) %>%
#   mutate(group = paste(group, continent_origin, sep = "_")) %>%
#   group_by(item_target, group) %>%
#   summarise(value = round(sum(value)), .groups = "drop") %>%
#   filter(value != 0) %>%
#   spread(group, value, fill = 0) %>%
#   mutate(Total = rowSums(across(where(is.numeric))))
# 


data_continent_bcp <- results %>%
  group_by(item_origin, continent_origin) %>%
  summarise(value = round(sum(value)), .groups = "drop") %>%
  filter(value != 0) %>%
  spread(continent_origin, value, fill = 0) %>%
  mutate(Total = rowSums(across(where(is.numeric))))


# data_uco <- round(as.matrix(MP)[, io$comm_code=="c145"] * 10000)
# colnames(data_uco) <- regions$iso3c
# data_uco <- cbind(io[,.(iso3c,continent,comm_code,item)], data_uco[, colSums(data_uco)>0])[rowSums(data_uco[,5:ncol(data_uco)])>0]

# fwrite(data_continent, 
#        file.path("output", paste0("FABIO_", country, "_", year, "_", 
#                                   extension, "_", consumption, "_", 
#                                   allocation, "-alloc_continent.csv")))


results$continent_origin[results$country_origin==country] <- country
results$continent_origin[results$country_origin!=country] <- "REST"

# by domestic vs. ROW
data_domestic <- results %>%
  mutate(group = case_when(
    group_origin == "Grazing" ~ "Grazing",
    grepl("Livestock", group_origin) ~ "Livestock",
    TRUE ~ "Crops"
  )) %>%
  mutate(group = paste(group, 
                       if_else(continent_origin == country, country, "ROW"), 
                       sep = "_")) %>%
  group_by(item_target, group) %>%
  summarise(value = round(sum(value)), .groups = "drop") %>%
  filter(value != 0) %>%
  spread(group, value, fill = 0)

# fwrite(data_domestic, 
#        file.path("output", paste0("FABIO_", country, "_", year, "_", 
#                                   extension, "_", consumption, "_", 
#                                   allocation, "-alloc.csv")))



################## CHECKS (TEMPORARY) ###################

# 
# Z <- readRDS(file = paste0(input_path, "Z_mass.rds"))
# Z2021 <- Z[[as.character(2021)]]
# rm(Z); gc()                        # free the rest of the years
# 
# sel_rows <- grepl("c028$|c074$", rownames(Z2021))
# sel_cols <- grepl("c074$|c028$", colnames(Z2021))
# 
# Zsub     <- Z2021[sel_rows, sel_cols , drop = FALSE]
# 
# View(as.matrix(Zsub))


