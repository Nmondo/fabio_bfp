###########################################################
########### GENERAL OPTIONS #########
###########################################################

options(scipen = 999)
options(digits = 5)

###########################################################
########### LOADING PACKAGES #########
###########################################################

library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(magrittr)
library(Matrix) 
library(logmult)
library(purrr)


###########################################################
########### LOADING DATA #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/intermediate_data/")

########### Pre-cleaned supply data (to collect consumption from the same source) #########

supply_fame_hvo <- readRDS("sup_fame_hvo_full.rds")
supply_biogasoline <- readRDS("sup_biogasoline_full.rds")

########### Pre-cleaned consumption and trade data -- for now only for 2012 onwards #########

y_table <- readRDS("y_table_initial.rds")
btd_intermediate <- readRDS("btd_intermediate.rds")
btd_excluded <- readRDS("btd_excluded_flows.rds")

btd_total <- readRDS("btd_total.rds")
neste_clean <- readRDS("neste_data_clean.rds")
neste_eu <- neste_clean$neste_eu
neste_regional <- neste_clean$neste_regional




###########################################################
########### MAKING FUNCTIONS #########
###########################################################

###########################################################
#1. RD Trade balancing (Iterative proportional fitting with initial constraints) #########
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

rd_trade_balancing <- function(year) {
    df <- read_xlsx("own_data/rd_trade_balancing.xlsx", 
                    sheet = paste0("rd_btd_", year),
                    range = "A35:AD46", col_names = FALSE)
    
    # iso3 + structure
    row_iso3 <- as.character(df[-c(1,nrow(df)), 1][[1]])
    col_iso3 <- as.character(df[1, -c(1, ncol(df))])
    n_row <- nrow(df) - 2
    n_col <- ncol(df) - 2
    
    mat <- apply(df[-c(1, nrow(df)), -c(1, ncol(df))], 2, as.numeric)
    
    row_margins <- as.numeric(df[-c(1, nrow(df)), ncol(df)][[1]])
    col_margins <- as.numeric(df[nrow(df), -c(1, ncol(df))])
    keep_rows <- row_margins > 0
    keep_cols <- col_margins > 0
    
    mat_filt <- mat[keep_rows, keep_cols, drop = FALSE]
    r_marg_filt <- row_margins[keep_rows]
    c_marg_filt <- col_margins[keep_cols]
    
    if (nrow(mat_filt) == 0 || ncol(mat_filt) == 0) {
      mat_ras_filt <- mat_filt
    } else if (nrow(mat_filt) == 1 && ncol(mat_filt) > 1) {
      mat_ras_filt <- matrix(c_marg_filt, nrow = 1)
    } else if (ncol(mat_filt) == 1 && nrow(mat_filt) > 1) {
      mat_ras_filt <- matrix(r_marg_filt, ncol = 1)
    } else {
      mat_ras_filt <- ras(as.matrix(mat_filt), r_marg_filt, c_marg_filt, tolerance = 1e-6)
    }  
    mat_ras <- matrix(0, n_row, n_col)
    mat_ras[keep_rows, keep_cols] <- mat_ras_filt
    
    mat_df <- as_tibble(mat_ras)
    names(mat_df) <- col_iso3
    
    out <- tibble(row_iso3, mat_df) %>%
      bind_rows(tibble(row_iso3 = "TOTAL", mat_df[nrow(mat_df), ])) %>%
      mutate(across(-row_iso3, ~ round(.x, 2)))
    
    out_long <- out %>%
      pivot_longer(
        cols      = -row_iso3,
        names_to  = "importer_iso3",
        values_to = "value"
      ) %>%                                       
      rename(exporter_iso3 = row_iso3) %>%         
      mutate(year = year)  
    
    df_fix <- read_xlsx("own_data/rd_trade_balancing.xlsx", 
                        sheet = paste0("rd_btd_", year),
                        range = "A20:AD31", col_names = FALSE)
    
    row_iso3_fix <- as.character(df_fix[-1, 1][[1]])     
    col_iso3_fix <- as.character(df_fix[1, -1])          
    
    mat_fix <- apply(df_fix[-1, -1], 2, as.numeric)
    
    mat_fix_df <- as_tibble(mat_fix)
    names(mat_fix_df) <- col_iso3_fix
    
    fix_long <- tibble(exporter_iso3 = row_iso3_fix) %>%
      bind_cols(mat_fix_df) %>%
      pivot_longer(
        cols      = -exporter_iso3,
        names_to  = "importer_iso3",
        values_to = "value"
      ) %>%
      filter(!is.na(value), value > 0) %>% 
      mutate(year = year)
    
    out_final <- out_long %>%
      left_join(
        fix_long %>%
          rename(value_fix = value),
        by = c("exporter_iso3", "importer_iso3", "year")
      ) %>%
      mutate(
        value = if_else(!is.na(value_fix), value_fix, value)
      ) %>%
      select(-value_fix)
    
    assign(paste0("rd_btd_", year), as.data.frame(out_final), envir = .GlobalEnv)
}


###########################################################
#2. Resolving error duplicate values #########
###########################################################

resolve_list_col <- function(x) {
  sapply(x, function(vals) {
    non_na <- vals[!is.na(vals)]
    if (length(non_na) == 0) NA_real_
    else non_na[[1]]
  })
}


###########################################################
#3. Rebalancing of Biogasoline & Biodiesel bilateral trade flows using initially excluded flows via simulated annealing #########
###########################################################

run_sa_rebalancing <- function(
    product_name, btd_intermediate, btd_excluded, full_compile,
    n_passes = 100, T_scale = 0.5, T_end_fac = 0.0005, seed = 42) {
  
  set.seed(seed)
  
  disc <- full_compile %>%
    filter(product == product_name) %>%
    mutate(discrepancy = total_supply + imports - exports - y) %>%
    select(iso3c, year, discrepancy) %>%
    drop_na(discrepancy)
  
  disc_vec <- setNames(disc$discrepancy, paste(disc$iso3c, disc$year))
  
  btd_prod      <- btd_intermediate %>% filter(product == product_name)
  btd_excl_prod <- btd_excluded     %>% filter(product == product_name)
  
  candidates <- btd_excl_prod %>%
    inner_join(
      btd_prod %>% select(exporter_iso3, importer_iso3, year, value_orig = value),
      by = c("exporter_iso3", "importer_iso3", "year")
    ) %>%
    mutate(flow_id = as.integer(factor(paste(exporter_iso3, importer_iso3, year))),
           alt_id  = row_number())
  
  n_flows <- n_distinct(candidates$flow_id)
  
  if (n_flows == 0) {
    return(btd_intermediate)
  }
  
  flow_pool <- candidates %>%
    group_by(flow_id) %>% group_split() %>%
    lapply(function(df) list(
      exporter    = df$exporter_iso3[1], importer    = df$importer_iso3[1],
      year        = df$year[1],
      cy_exp_key  = paste(df$exporter_iso3[1], df$year[1]),
      cy_imp_key  = paste(df$importer_iso3[1], df$year[1]),
      value_orig  = df$value_orig[1],
      alt_values  = df$value, alt_sources = df$source, alt_flows = df$FLOW
    ))
  
  T_start <- median(abs(disc_vec[disc_vec != 0]), na.rm = TRUE) * T_scale
  T_end   <- T_start * T_end_fac
  n_iter  <- n_passes * n_flows
  alpha   <- (T_end / T_start)^(1 / (n_iter - 1))
  
  cat(sprintf("\n─── SA: %-35s ─────────────────\n", product_name))
  cat(sprintf("  Flows : %d | Iterations : %d (%d passes) | T: %.4f → %.6f\n\n",
              n_flows, n_iter, n_passes, T_start, T_end))
  
  current_choice_idx <- rep(0L, n_flows)
  current_obj        <- sum(abs(disc_vec), na.rm = TRUE)
  best_obj           <- current_obj
  best_choice_idx    <- current_choice_idx
  best_disc_vec      <- disc_vec
  iter               <- 0L
  
  for (pass in seq_len(n_passes)) {
    flow_order <- sample.int(n_flows)
    for (fi in flow_order) {
      iter <- iter + 1L
      T    <- T_start * alpha^(iter - 1L)
      
      pool_f  <- flow_pool[[fi]]
      k_f     <- length(pool_f$alt_values)
      cur_idx <- current_choice_idx[fi]
      cur_val <- if (cur_idx == 0L) pool_f$value_orig else pool_f$alt_values[cur_idx]
      
      new_idx <- sample.int(k_f + 1L, 1L) - 1L
      if (new_idx == cur_idx) next
      
      new_val <- if (new_idx == 0L) pool_f$value_orig else pool_f$alt_values[new_idx]
      delta   <- new_val - cur_val
      if (delta == 0) next
      
      exp_key  <- pool_f$cy_exp_key; imp_key <- pool_f$cy_imp_key
      disc_exp <- disc_vec[exp_key];  disc_imp <- disc_vec[imp_key]
      
      delta_obj <- 0
      if (!is.na(disc_exp)) delta_obj <- delta_obj + abs(disc_exp - delta) - abs(disc_exp)
      if (!is.na(disc_imp)) delta_obj <- delta_obj + abs(disc_imp + delta) - abs(disc_imp)
      
      if (delta_obj < 0 || runif(1L) < exp(-delta_obj / T)) {
        current_choice_idx[fi] <- new_idx
        if (!is.na(disc_exp)) disc_vec[exp_key] <- disc_exp - delta
        if (!is.na(disc_imp)) disc_vec[imp_key] <- disc_imp + delta
        current_obj <- current_obj + delta_obj
        if (current_obj < best_obj) {
          best_obj <- current_obj; best_choice_idx <- current_choice_idx
          best_disc_vec <- disc_vec
        }
      }
    }
    cat(sprintf("  Pass %2d/%d | T = %8.4f | current = %10.2f | best = %10.2f\n",
                pass, n_passes, T, current_obj, best_obj))
  }
  
  obj_before <- sum(abs(disc$discrepancy), na.rm = TRUE)
  cat(sprintf("\n  |disc| before : %.2f  →  after : %.2f  (reduction: %.1f%%)\n",
              obj_before, best_obj, 100 * (obj_before - best_obj) / obj_before))
  
  substitutions <- lapply(seq_len(n_flows), function(f) {
    idx <- best_choice_idx[f]
    if (idx == 0L) return(NULL)
    pool_f <- flow_pool[[f]]
    tibble(exporter_iso3 = pool_f$exporter, importer_iso3 = pool_f$importer,
           product = product_name, year = pool_f$year,
           value = pool_f$alt_values[idx], source = pool_f$alt_sources[idx],
           FLOW  = pool_f$alt_flows[idx])
  }) %>% bind_rows()
  
  cat(sprintf("  Substitutions applied : %d / %d\n", nrow(substitutions), n_flows))
  
  disc_report <- disc %>%
    mutate(cy_key = paste(iso3c, year), disc_before = discrepancy,
           disc_after  = best_disc_vec[cy_key],
           improvement = abs(disc_before) - abs(disc_after)) %>%
    filter(abs(improvement) > 1e-9) %>%
    select(iso3c, year, disc_before, disc_after, improvement) %>%
    arrange(desc(abs(improvement)))
  cat("\n  Top countries by improvement:\n"); print(head(disc_report, 10))
  
  if (nrow(substitutions) == 0) return(btd_intermediate)
  btd_intermediate %>%
    rows_update(substitutions, by = c("exporter_iso3","importer_iso3","product","year")) %>%
    arrange(importer_iso3, exporter_iso3, product, year)
}




#########################################################################
  ########### CALCULATING TOTAL BIOFUELS IMPORTS AND EXPORTS BY COUNTRY AND YEAR #########
#########################################################################

btd_intermediate %<>%
  mutate(value = case_when(unit == "Ml" & product == "Biodiesel" ~ value/1.136, # unit back to kt for the elicitation
                           unit == "Ml" & product == "Renewable diesel" ~ value/1.282,
                           unit == "Ml" & product == "Bioethanol" ~ value/1.267,
                           TRUE ~ value),
         unit = "kt")

exports <- btd_intermediate %>%
  group_by(iso3c = exporter_iso3, year, product) %>%
  summarise(exports = sum(value, na.rm = TRUE), .groups = "drop")

imports <- btd_intermediate %>%
  group_by(iso3c = importer_iso3, year, product) %>%
  summarise(imports = sum(value, na.rm = TRUE), .groups = "drop")

total_trade <- full_join(exports, imports,
                         by = c("iso3c", "year", "product"))



#########################################################################
########### DISAGGREGATING "BIODIESEL & RENEWABLE DIESEL" WHERE NECESSARY #########
#########################################################################

supply_fame_hvo %<>%
  mutate(value = case_when(unit == "Ml" & output == "Biodiesel" ~ value/1.136, # unit back to kt for the elicitation
                           unit == "Ml" & output == "Renewable diesel" ~ value/1.282,
                           TRUE ~ value),
         unit = "kt") 


#########################################################################
#1. Identifying rows to disaggregate #########
#########################################################################

y_biodiesels_to_solve <- subset(y_table, product == "Biodiesel & Renewable diesel") %>%
  rename(y_agg = value)


#########################################################################
#2. Joining production & trade (Biodiesel) or production (Renewable diesel) #########
#########################################################################

y_biodiesels_to_solve %<>% 
  left_join(supply_fame_hvo %>% 
              filter(output == "Biodiesel") %>% 
              select(year,iso3c,value) %>% 
              rename(supply_bd = value),
            by = c("year","iso3c")) %>%
  left_join(supply_fame_hvo %>% 
              filter(output == "Renewable diesel") %>% 
              select(year,iso3c,value) %>% 
              rename(supply_rd = value),
            by = c("year","iso3c")) %>%
  left_join(total_trade %>%
              filter(product == "Biodiesel") %>%
              select(-product) %>%
              rename(exports_bd = exports,
                     imports_bd = imports),
            by = c("year","iso3c"))


#########################################################################
#3. Computing residuals and estimating consumption of each product, under some assumptions #########
#########################################################################
#consumption BD = supply BD + imports BD - exports RD
#consumption RD = consumption BD&RD - consumption BD
#net trade RD = supply RD - consumption RD 
#if net trade > 0, exports = net trade and imports = 0
#if net trade < 0, exports = 0 and imports = net trade.

y_biodiesels_to_solve %<>% 
  mutate(residual = y_agg - (supply_bd + imports_bd - exports_bd),
         y_rd_constrained = case_when(residual > y_agg ~ y_agg, 
                                      residual < 0 ~ 0,
                                      TRUE ~ residual),
         y_bd_constrained = y_agg - y_rd_constrained,
         stat_discrepancy = case_when(residual > y_agg ~ y_agg - residual,
                                      residual < 0 ~ residual,
                                      TRUE ~ 0))

y_biodiesel_solved <- y_biodiesels_to_solve %>%
  select(use, iso3c, unit, year, y_bd_constrained, source) %>%
  mutate(product = "Biodiesel") %>%
  rename(value = y_bd_constrained)

y_rd_solved <- y_biodiesels_to_solve %>%
  select(use, iso3c, unit, year, y_rd_constrained, source) %>%
  mutate(product = "Renewable diesel") %>%
  rename(value = y_rd_constrained)

y_table_intermediate1 <- y_table %>%
  anti_join(y_biodiesels_to_solve, by = c("iso3c", "year", "product")) %>%
  rows_upsert(
    y_biodiesel_solved %>% mutate(source = ifelse(iso3c == "FIN",
                                                  "Estimate from IEA Renewables",
                                                  paste0("Estimate from ", source))),
    by = c("iso3c", "year", "product", "use")
  ) %>%
  rows_upsert(
    y_rd_solved %>% mutate(source = ifelse(iso3c == "FIN",
                                           "Estimate from IEA Renewables",
                                           paste0("Estimate from ", source))),
    by = c("iso3c", "year", "product", "use")
  )




###################################################################################################
########### RENEWABLE DIESEL TRADE #########
###################################################################################################

###################################################################################################
#1. Computation of net trade of RD  where this issue of aggregation does not appear #########
###################################################################################################

y_rd <- subset(y_table_intermediate1, product == "Renewable diesel") %>%
  rename(y_rd = value) %>%
  mutate(y_rd = case_when(unit == "Ml" ~ y_rd/1.282,
                          unit == "1000 barrels" ~ (y_rd*0.158987)/1.282,
                           TRUE ~ y_rd),
         unit = case_when(unit %in% c("Ml","1000 barrels") ~ "kt",
                          TRUE ~ unit))

rd_total_trade <- btd_total %>%
  filter(product == "Renewable diesel",
         !is.na(value)) %>%
  mutate(value = case_when(unit == "Ml" ~ value/1.282,
                           TRUE ~ value),
         imports_rd = ifelse(importer_iso3!="Total", value, NA_real_),
         exports_rd = ifelse(exporter_iso3!="Total", value, NA_real_),
         iso3c = ifelse(importer_iso3!="Total",importer_iso3,exporter_iso3)) %>%
  group_by(iso3c,year) %>%
  summarize(imports_rd = sum(imports_rd, na.rm=TRUE),
            exports_rd = sum(exports_rd, na.rm=TRUE))

y_rd %<>% mutate(exports_rd = NA_real_) %>%
  left_join(supply_fame_hvo %>% 
                    filter(output == "Renewable diesel") %>% 
                    select(year,iso3c,value) %>%
                    rename(supply_rd = value),
                  by = c("year","iso3c")) %>%
  left_join(total_trade %>%
              filter(product == "Renewable diesel") %>%
              select(-product,-exports) %>%
              rename(imports_rd = imports),
            by = c("year","iso3c")) %>% 
  rows_update(rd_total_trade %>% select(year,iso3c,exports_rd,imports_rd),
             by = c("year","iso3c"),
             unmatched = "ignore")

y_rd %<>% mutate(
  exports_nettrade_rd = ifelse(y_rd < supply_rd, supply_rd - y_rd, 0),
  imports_nettrade_rd = ifelse(y_rd > supply_rd, y_rd - supply_rd, 0)
)


###################################################################################################
#2. Collecting information to fill the renewable diesel trade balancing matrix #########
###################################################################################################

## This is used only as an input in manually filled constraints for the RD trade balancing, and will be further readjusted in the balancing. 

# Rescaling US BTD on total sales from Neste reports
# 
# usa_rd_totals <- btd_intermediate %>%
#   filter(product == "Renewable diesel", importer_iso3 == "USA") %>%
#   group_by(year) %>%
#   summarise(usa_rd_sum = sum(value, na.rm = TRUE), .groups = "drop")
# 
# # 2) Neste North America totals by year
# neste_na_totals <- neste_regional %>%
#   filter(region == "North America") %>%
#   group_by(year) %>%
#   summarise(neste_na_sum = sum(value/1.282, na.rm = TRUE), .groups = "drop")
# 
# # 3) Scaling factor per year (handle zero/NA carefully)
# scales <- usa_rd_totals %>%
#   inner_join(neste_na_totals, by = "year") %>%
#   mutate(scale_factor = if_else(usa_rd_sum > 0,
#                                 neste_na_sum / usa_rd_sum,
#                                 NA_real_))
# 
# # 4) Apply factor only to USA RD rows, keep others unchanged
# btd_intermediate_rescaled <- btd_intermediate %>%
#   left_join(scales %>% select(year, scale_factor), by = "year") %>%
#   mutate(
#     value_rescaled = if_else(
#       product == "Renewable diesel" &  importer_iso3 == "USA" & !is.na(scale_factor),
#       value * scale_factor,
#       value
#     )
#   )


###############################################################################################
#3. Renewable diesel trade balancing ###############
###############################################################################################

# Applying the RD trade balancing function and compiling it into a dataframe

rd_btd_final <- map_dfr(2012:2022, rd_trade_balancing) %>% filter(exporter_iso3!="TOTAL")
rd_btd_final %<>% mutate(product = "Renewable diesel",
                        unit = "kt",
                        source = "Estimate")

# Removing temporary objects
rm(list = ls(pattern = "^rd_btd_\\d{4}$"))


###############################################################################################
#4. Updating BTD with Renewable diesel trade ###############
###############################################################################################

btd_intermediate1 <- rows_upsert(
  btd_intermediate,
  rd_btd_final,
  by = c("exporter_iso3", "importer_iso3", "product", "year")
)




###############################################################################################
######################### CALCULATING TOTAL IMPORTS AND EXPORTS BY PRODUCT, COUNTRY, YEAR ###############
###############################################################################################

exports <- btd_intermediate1 %>%
  group_by(iso3c = exporter_iso3, year, product) %>%
  summarise(exports = sum(value, na.rm = TRUE), 
            unit_exports = first(unit), 
            .groups = "drop")

imports <- btd_intermediate1 %>%
  group_by(iso3c = importer_iso3, year, product) %>%
  summarise(imports = sum(value, na.rm = TRUE),
            unit_imports = first(unit), 
            .groups = "drop")

total_trade <- full_join(exports, imports,
                         by = c("iso3c", "year", "product"))

y_total <- y_table_intermediate1 %>% 
  group_by(iso3c,year,product) %>%
  summarize(y = sum(value),
            unit = first(unit))




###############################################################################################
######################### CHECKING BALANCES BY COMPILING SUPPLY, CONSUMPTION AND TRADE ###############
###############################################################################################


###############################################################################################
#1. Pivotting to wide format ###############
###############################################################################################

y_wide <- y_table_intermediate1 %>%
  filter(use %in% c("Fuel", "Non-fuel", "Total")) %>%
  mutate(y_val = case_when(
    unit == "kt" & product == "Biodiesel"                       ~ 1.136 * value,
    unit == "kt" & product %in% c("Bioethanol", "Biogasoline")  ~ 1.267 * value,
    unit == "kt" & product == "Renewable diesel"                ~ 1.282 * value,
    unit == "kt" & product == "ETBE"                            ~ 1.351 * value,
    TRUE                                                        ~ value
  )) %>%
  pivot_wider(
    id_cols     = c(product, iso3c, year),
    names_from  = use,
    names_prefix = "y_",
    values_from = y_val,
    values_fn   = ~ first(na.omit(.x))   # take first non-NA among duplicates
  ) %>%
  rename(y_Non_fuel = `y_Non-fuel`)

y_wide <- y_wide %>%
  mutate(across(c(y_Fuel, y_Non_fuel, y_Total),
                ~ if (is.list(.x)) as.double(unlist(.x)) else .x))


###############################################################################################
#2. Joining consumption, production and trade data to check balances ###############
###############################################################################################

full_compile <- bind_rows(
  supply_fame_hvo %>% rename(total_supply = value),
  supply_biogasoline)  %>%
  rename(
    product     = output,
    unit_supply = unit
  ) %>%
  full_join(total_trade, by = c("year", "iso3c", "product")) %>%
  full_join(y_total %>% rename(unit_y = unit),
            by = c("year", "iso3c", "product")) %>%
  left_join(y_wide, by = c("product", "iso3c", "year")) %>%   # NEW
  
  mutate(
    total_supply = case_when(
      unit_supply == "kt" & product == "Biodiesel"        ~ 1.136 * total_supply,
      unit_supply == "kt" & product == "Bioethanol"       ~ 1.267 * total_supply,
      unit_supply == "kt" & product == "Renewable diesel" ~ 1.282 * total_supply,
      unit_supply == "kt" & product == "ETBE"             ~ 1.351 * total_supply,
      TRUE                                                ~ total_supply
    ),
    exports = case_when(
      unit_exports == "kt" & product == "Biodiesel"        ~ 1.136 * exports,
      unit_exports == "kt" & product == "Bioethanol"       ~ 1.267 * exports,
      unit_exports == "kt" & product == "Renewable diesel" ~ 1.282 * exports,
      unit_exports == "kt" & product == "ETBE"             ~ 1.351 * exports,
      TRUE                                                 ~ exports
    ),
    imports = case_when(
      unit_imports == "kt" & product == "Biodiesel"        ~ 1.136 * imports,
      unit_imports == "kt" & product == "Bioethanol"       ~ 1.267 * imports,
      unit_imports == "kt" & product == "Renewable diesel" ~ 1.282 * imports,
      unit_imports == "kt" & product == "ETBE"             ~ 1.351 * imports,
      TRUE                                                 ~ imports
    ),
    y = case_when(
      unit_y == "kt" & product == "Biodiesel"                      ~ 1.136 * y,
      unit_y == "kt" & product %in% c("Bioethanol", "Biogasoline") ~ 1.267 * y,
      unit_y == "kt" & product == "Renewable diesel"               ~ 1.282 * y,
      unit_y == "kt" & product == "ETBE"                           ~ 1.351 * y,
      TRUE                                                         ~ y
    ),
    unit_supply  = if_else(unit_supply  == "kt" & product %in% c("Biodiesel","Bioethanol","Biogasoline","Renewable diesel","ETBE"), "Ml", unit_supply),
    unit_exports = if_else(unit_exports == "kt" & product %in% c("Biodiesel","Bioethanol","Biogasoline","Renewable diesel","ETBE"), "Ml", unit_exports),
    unit_imports = if_else(unit_imports == "kt" & product %in% c("Biodiesel","Bioethanol","Biogasoline","Renewable diesel","ETBE"), "Ml", unit_imports),
    unit_y       = if_else(unit_y       == "kt" & product %in% c("Biodiesel","Bioethanol","Biogasoline","Renewable diesel","ETBE"), "Ml", unit_y)
  ) %>%
  
  filter(year >= 2012,
         iso3c %in% regions$iso3c) %>%
  
  mutate(across(c(imports, exports), ~ if_else(is.na(.x), 0, .x)),
         across(c(unit_imports, unit_exports, unit_y, unit_supply), ~ if_else(is.na(.x), "Ml", .x)),
         process = ifelse(product == "ETBE", "ETBE production", process),
         y       = ifelse(product == "Renewable diesel" & is.na(y), 0, y)) %>%
  
  arrange(iso3c, year, product)

## Manual correction for PER-Bioethanol ##
full_compile <- full_compile %>%
  mutate(
    y = case_when(
      product == "Bioethanol" & iso3c == "PER" & !is.na(y_Total) ~ y_Total,
      TRUE                                                         ~ y
    ),
    y_Non_fuel = case_when(
      product == "Bioethanol" & iso3c == "PER" & !is.na(y_Total) ~ y_Total - y_Fuel,
      TRUE                                                         ~ y_Non_fuel
    ),
    y_Fuel = case_when(
      product == "Bioethanol" & iso3c %in% c("AUS","BOL") & !is.na(y_Total) ~ y_Total,
      TRUE                                                         ~ y_Fuel
    )
  )




###########################################################################
########### REBALANCING OF BILATERAL TRADE FLOWS VIA SIMULATED ANNEALING #################
###########################################################################

products_to_rebalance <- list(
  list(product = "Biodiesel",  n_passes = 100),
  list(product = "Bioethanol", n_passes = 100)
)

btd_intermediate_balanced <- reduce(
  products_to_rebalance,
  function(btd, p) run_sa_rebalancing(
    product_name     = p$product,
    btd_intermediate = btd,
    btd_excluded     = btd_excluded,
    full_compile     = full_compile,
    n_passes         = p$n_passes
  ),
  .init = btd_intermediate
)

# Changes summary

btd_changes <- btd_intermediate %>%
  inner_join(btd_intermediate_balanced,
             by = c("exporter_iso3","importer_iso3","product","year"),
             suffix = c("_orig","_balanced")) %>%
  filter(abs(value_orig - value_balanced) > 1e-9) %>%
  mutate(delta = value_balanced - value_orig) %>%
  select(exporter_iso3, importer_iso3, product, year,
         value_orig, value_balanced, delta,
         source_orig, source_balanced, FLOW_orig, FLOW_balanced) %>%
  arrange(product, desc(abs(delta)))


###########################################################
#1. Rebuild btd_intermediate_balanced1 #################
###########################################################

btd_intermediate_balanced1 <- rows_upsert(
  btd_intermediate_balanced,
  rd_btd_final,
  by = c("exporter_iso3", "importer_iso3", "product", "year")
) %>%
  mutate(value = case_when(
    unit == "kt" & product == "Biodiesel"        ~ 1.136 * value,
    unit == "kt" & product == "Bioethanol"       ~ 1.267 * value,
    unit == "kt" & product == "Renewable diesel" ~ 1.282 * value,
    unit == "kt" & product == "ETBE"             ~ 1.351 * value,
    TRUE                                                 ~ value),
    unit = case_when(unit == "kt" & product %in% c("Biodiesel","Bioethanol","Renewable diesel", "ETBE") ~ "Ml",
    TRUE ~ unit)) 

###########################################################
#2. Recompute total trade from updated bilateral flows #################
###########################################################

exports_bal <- btd_intermediate_balanced1 %>%
  group_by(iso3c = exporter_iso3, year, product) %>%
  summarise(exports      = sum(value, na.rm = TRUE),
            unit_exports = first(unit), .groups = "drop")

imports_bal <- btd_intermediate_balanced1 %>%
  group_by(iso3c = importer_iso3, year, product) %>%
  summarise(imports      = sum(value, na.rm = TRUE),
            unit_imports = first(unit), .groups = "drop")

total_trade_balanced <- full_join(
  exports_bal, imports_bal,
  by = c("iso3c", "year", "product")
)


###########################################################
# 3. Patch full_compile #################
###########################################################

full_compile_balanced <- full_compile %>%
  select(-exports, -imports, -unit_exports, -unit_imports) %>%
  left_join(total_trade_balanced, by = c("iso3c", "year", "product")) %>%
  mutate(
    across(c(imports, exports),       ~ if_else(is.na(.x), 0,    .x)),
    across(c(unit_imports, unit_exports), ~ if_else(is.na(.x), "Ml", .x)))




###############################################################################################
######################### READJUSTING SUPPLY AND CONSUMPTION ###############
###############################################################################################

###############################################################################################
#1. Restricting possible uses for Biodiesel & Renewable diesel to fuel only ###############
###############################################################################################

full_compile_balanced <- full_compile_balanced %>%
  mutate(y_Fuel = case_when(product == "Biodiesel" & is.na(y_Fuel) ~ 0,
                            product == "Renewable diesel" ~ y,
                            TRUE ~ y_Fuel),
         y_Non_fuel = case_when(product %in% c("Biodiesel","Renewable diesel") ~ 0,
                                TRUE ~ y_Non_fuel))


###############################################################################################
#2. Readjusting supply and consumption for Renewable diesel ###############
###############################################################################################

full_compile_balanced <- full_compile_balanced %>%
  mutate(
    residual_hvo = case_when(
      product == "Renewable diesel" ~ total_supply + imports - exports - y_Fuel,
      TRUE                   ~ NA_real_
    ),
    
    y_Fuel = case_when(
      product == "Renewable diesel" & !is.na(residual_hvo) & residual_hvo > 0
      ~ y_Fuel + residual_hvo,
      TRUE ~ y_Fuel
    ),
    
    total_supply = case_when(
      product == "Renewable diesel" & !is.na(residual_hvo) & residual_hvo < 0
      ~ total_supply - residual_hvo,    
      TRUE ~ total_supply
    )
  ) %>%
  select(-residual_hvo)


###############################################################################################
#3. Readjusting supply and consumption for Biodiesel ###############
###############################################################################################

full_compile_balanced <- full_compile_balanced %>%
  mutate(
    residual_bd = case_when(
      product == "Biodiesel" ~ total_supply + imports - exports - y_Fuel,
      TRUE                   ~ NA_real_
    ),
    
    # Apparent > reported → y_Fuel absorbs the excess
    y_Fuel = case_when(
      product == "Biodiesel" & !is.na(residual_bd) & residual_bd > 0
      ~ y_Fuel + residual_bd,
      TRUE ~ y_Fuel
    ),
    
    # Apparent < reported → total_supply filled up to match y_Fuel
    total_supply = case_when(
      product == "Biodiesel" & !is.na(residual_bd) & residual_bd < 0
      ~ total_supply - residual_bd,     # residual_bd is negative, so subtracting it adds
      TRUE ~ total_supply
    )
  ) %>%
  select(-residual_bd)


###############################################################################################
#4. Readjusting supply and consumption for Bioethanol ###############
###############################################################################################

full_compile_balanced <- full_compile_balanced %>%
  
  mutate(
    total_supply = case_when(
      product == "Bioethanol" &
        total_supply < y + exports - imports   ~ y + exports - imports,
      TRUE                                     ~ total_supply
    )
  ) %>%
  
  mutate(
    y_Non_fuel = case_when(
      product == "Bioethanol" &
        source == "OECD-FAO Agricultural outlook" &
        is.na(y_Fuel)                              ~ y_Total,
      
      product == "Bioethanol" &
        total_supply > y + exports - imports &
        !is.na(y_Non_fuel)                         ~ y_Non_fuel + total_supply - (y + exports - imports),
      
      product == "Bioethanol" &
        total_supply > y + exports - imports &
        is.na(y_Non_fuel)                          ~ total_supply - (y + exports - imports),
      
      product == "Bioethanol" &
        total_supply <= y + exports - imports      ~ 0, 
      
      TRUE                                         ~ y_Non_fuel
    ),
    y_Fuel = case_when(
      product == "Bioethanol" &
        source == "OECD-FAO Agricultural outlook" &
        is.na(y_Fuel)                              ~ 0,
      product %in% c("Bioethanol","Biogasoline") & is.na(y_Fuel) ~ y_Total,
      TRUE ~ y_Fuel
  )
  )
 
full_compile_balanced <- full_compile_balanced %>%
  mutate(
    y_Fuel = case_when(
      product == "Bioethanol" & iso3c == "NOR" & is.na(y_Fuel)              ~ pmax(0, total_supply + imports - exports),
      product == "Bioethanol" & iso3c %in% c("PRK","CPV","ANT","PYF","SSD","TLS") ~ 0,
      TRUE                                                                   ~ y_Fuel
    ),
    y_Non_fuel = case_when(
      product == "Bioethanol" & iso3c == "NOR"              ~ 0,
      product == "Bioethanol" & iso3c %in% c("PRK","CPV","ANT","PYF","SSD","TLS") ~ pmax(0, total_supply + imports - exports),
      TRUE                                                                   ~ y_Non_fuel
    )
  )




###############################################################################################
######################### ESTIMATING ETBE PRODUCTION & CONSUMPTION FROM IMPORTS AND EXPORTS ###############
###############################################################################################

full_compile_balanced <- full_compile_balanced %>%
  mutate(
    total_supply = case_when(
      # JPN/ARE: purely import-and-consume, no domestic production
      product == "ETBE" & iso3c %in% c("JPN", "ARE")
      ~ 0,
      product == "ETBE" & y_Fuel > 0
      ~ y_Fuel + exports - imports,
      product == "ETBE" & (y_Fuel == 0 | is.na(y_Fuel)) & exports > imports
      ~ exports - imports,
      product == "ETBE" & (y_Fuel == 0 | is.na(y_Fuel)) & exports <= imports
      ~ 0,
      TRUE ~ total_supply
    ),
    y_Fuel = case_when(
      # JPN/ARE: consumption = all imports (exports are separate re-export flows)
      product == "ETBE" & iso3c %in% c("JPN", "ARE")
      ~ imports,
      product == "ETBE" & y_Fuel > 0
      ~ y_Fuel,
      product == "ETBE" & (y_Fuel == 0 | is.na(y_Fuel)) & imports > exports
      ~ imports - exports,
      product == "ETBE" & (y_Fuel == 0 | is.na(y_Fuel)) & imports <= exports
      ~ 0,
      TRUE ~ y_Fuel
    ),
    y_Non_fuel = ifelse(product == "ETBE", 0, y_Non_fuel),
    y          = ifelse(product == "ETBE", y_Fuel, y)
  )




###############################################################################################
# NET OUT BIOETHANOL IMPORTS USED AS ETBE FEEDSTOCK FROM ETBE PRODUCTION, TO AVOID DOUBLE COUNTING ##
###############################################################################################

eth_imports_etbe_countries <- full_compile_balanced %>%
  filter(product == "Bioethanol", iso3c %in% c("JPN", "ARE")) %>%
  select(iso3c, year, eth_imports = imports)

full_compile_balanced <- full_compile_balanced %>%
  left_join(eth_imports_etbe_countries, by = c("iso3c", "year")) %>%
  mutate(
    # (c) Reduce ETBE total_supply by Bioethanol imports absorbed as feedstock
    total_supply = case_when(
      product == "ETBE" & iso3c %in% c("JPN", "ARE")
      ~ pmax(0, total_supply - coalesce(eth_imports, 0)),
      TRUE ~ total_supply
    ),
    # (c) JPN Bioethanol: all intermediate → y_Fuel = 0, y_Non_fuel = residual apparent consumption
    y_Fuel = case_when(
      product == "Bioethanol" & iso3c == "JPN" ~ 0,
      TRUE                                     ~ y_Fuel
    ),
    # (c) JPN + ARE Bioethanol: remove the portion of imports absorbed into ETBE from apparent consumption
    y_Non_fuel = case_when(
      product == "Bioethanol" & iso3c %in% c("JPN", "ARE") ~
        pmax(0, total_supply + imports - exports - coalesce(eth_imports, 0)),
      TRUE ~ y_Non_fuel
    )
  ) %>%
  select(-eth_imports)




###############################################################################################
######################### AGGREGATING BIOETHANOL AND ETBE TO BIOGASOLINE ###############
###############################################################################################

biogasoline_keys <- full_compile_balanced %>%
  filter(product == "Biogasoline") %>%
  distinct(iso3c, year)

biogasoline_context_full <- full_compile_balanced %>%
  filter(product %in% c("Biogasoline", "Bioethanol", "ETBE")) %>%
  semi_join(biogasoline_keys, by = c("iso3c", "year"))

# Countries with a Biogasoline row AND Bioethanol/ETBE rows to aggregate
biogasoline_keys_with_eth <- biogasoline_context_full %>%
  filter(product %in% c("Bioethanol", "ETBE")) %>%
  distinct(iso3c, year)

# Countries with a Biogasoline row but NO Bioethanol/ETBE — leave untouched
biogasoline_keys_standalone <- biogasoline_keys %>%
  anti_join(biogasoline_keys_with_eth, by = c("iso3c", "year"))

# Pass 1: countries with both Biogasoline + Bioethanol/ETBE rows
biogasoline_agg_p1 <- biogasoline_context_full %>%
  semi_join(biogasoline_keys_with_eth, by = c("iso3c", "year")) %>%
  filter(product %in% c("Bioethanol", "ETBE")) %>%
  group_by(iso3c, year) %>%
  summarise(
    total_supply = sum(total_supply, na.rm = TRUE),
    exports      = sum(exports,      na.rm = TRUE),
    imports      = sum(imports,      na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  left_join(
    biogasoline_context_full %>%
      filter(product == "Biogasoline") %>%
      select(iso3c, year, y_Fuel, y_Non_fuel, y_Total),
    by = c("iso3c", "year")
  ) %>%
  mutate(
    product      = "Biogasoline",
    y_Fuel       = coalesce(y_Fuel, 0),
    y_Non_fuel   = coalesce(y_Non_fuel, 0),
    .apparent    = total_supply + imports - exports,
    .total_y     = y_Fuel + y_Non_fuel,
    y_Non_fuel   = ifelse(.apparent > .total_y, y_Non_fuel + (.apparent - .total_y), y_Non_fuel),
    total_supply = ifelse(.apparent < .total_y, .total_y + exports - imports, total_supply),
    y            = y_Fuel + y_Non_fuel
  ) %>%
  select(-.apparent, -.total_y)

full_compile_balanced <- full_compile_balanced %>%
  rows_update(biogasoline_agg_p1, by = c("iso3c", "year", "product")) %>%
  anti_join(biogasoline_keys_with_eth %>% mutate(product = "Bioethanol"),
            by = c("iso3c", "year", "product")) %>%
  anti_join(biogasoline_keys_with_eth %>% mutate(product = "ETBE"),
            by = c("iso3c", "year", "product")) %>%
  arrange(iso3c, year, product)


# Pass 2: countries with no Biogasoline row at all
biogasoline_agg_p2 <- full_compile_balanced %>%
  filter(product %in% c("Bioethanol", "ETBE")) %>%
  group_by(iso3c, year) %>%
  summarise(
    total_supply = sum(total_supply, na.rm = TRUE),
    imports      = sum(imports,      na.rm = TRUE),
    exports      = sum(exports,      na.rm = TRUE),
    y_Fuel       = sum(y_Fuel,       na.rm = TRUE),
    y_Non_fuel   = sum(y_Non_fuel,   na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    product      = "Biogasoline",
    y_Fuel       = ifelse(iso3c == "KOR", y_Non_fuel, y_Fuel),
    y_Non_fuel   = ifelse(iso3c == "KOR", 0, y_Non_fuel),
    .apparent    = total_supply + imports - exports,
    .total_y     = y_Fuel + y_Non_fuel,
    y_Non_fuel   = ifelse(.apparent > .total_y, y_Non_fuel + (.apparent - .total_y), y_Non_fuel),
    total_supply = ifelse(.apparent < .total_y, .total_y + exports - imports, total_supply),
    y            = y_Fuel + y_Non_fuel
  ) %>%
  select(-.apparent, -.total_y)

full_compile_balanced <- full_compile_balanced %>%
  filter(!product %in% c("Bioethanol", "ETBE")) %>%
  bind_rows(biogasoline_agg_p2) %>%
  mutate(
    process = ifelse(product == "Biogasoline", "Biogasoline production", process),
    across(starts_with("unit"), ~ ifelse(product == "Biogasoline", "Ml", .x))
  ) %>%
  arrange(iso3c, year, product)


# Checking balances globally by product-year (sum of imports = sum of exports; sum of production = sum of use)

full_compile_balanced %>%
  filter(product %in% c("Biogasoline", "Biodiesel", "Renewable diesel")) %>%
  group_by(year, product) %>%
  summarise(
    total_supply = sum(total_supply,           na.rm = TRUE),
    imports      = sum(imports,                na.rm = TRUE),
    exports      = sum(exports,                na.rm = TRUE),
    y            = sum(y_Fuel + y_Non_fuel,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(product, year) %>%
  print(n = Inf)




###########################################################
########### MAKING FINAL SUPPLY TABLES WITH BY-PRODUCTS ########### 
###########################################################

###########################################################
#1. Final supply table without by-products ########### 
###########################################################

supply_final_bf <- full_compile_balanced %>%
  filter(product %in% c("Biodiesel","Renewable diesel","Biogasoline")) %>%
  select(iso3c,product,process,year,total_supply,unit_supply) %>%
  rename(supply = total_supply,
         unit = unit_supply,
         proc = process)


###########################################################
#2. Modeling glycerol production as a by-product of FAME (10% of biodiesel output weight) ########### 
###########################################################

supply_final_bf %<>%
  dplyr::bind_rows(
    supply_final_bf %>%
      dplyr::filter(product == "Biodiesel") %>%
      dplyr::mutate(product = "Glycerol, crude",
                    supply  = 0.1 * (supply/1.136),
                    unit = "kt")
  )


###########################################################
#3. Modeling naphtha production as a by-product of HVO (8% of Renewable diesel output weight) ########### 
###########################################################

supply_final_bf %<>%
  dplyr::bind_rows(
    supply_final_bf %>%
      dplyr::filter(product == "Renewable diesel") %>%
      dplyr::mutate(product = "Bionaphtha",
                    supply  = 0.08 * (supply/1.282),
                    unit = "kt")
  )


###########################################################
#4. Modeling propane production as a by-product of HVO (4% of Renewable diesel output weight) ########### 
###########################################################

supply_final_bf %<>%
  dplyr::bind_rows(
    supply_final_bf %>%
      dplyr::filter(product == "Renewable diesel") %>%
      dplyr::mutate(product = "Biopropane",
                    supply  = 0.1 * (supply/1.282),
                    unit = "kt")
  )




###########################################################
########### MAKING FINAL TABLES FOR USE IN FINAL DEMAND AND BILATERAL TRADE ########### 
###########################################################

y_final_bf <- full_compile_balanced %>%
  filter(product %in% c("Biodiesel","Renewable diesel","Biogasoline")) %>%
  select(iso3c,product,process,year,y_Fuel,y_Non_fuel,unit_y) %>%
  rename(fuel = y_Fuel,
         non_fuel = y_Non_fuel,
         unit = unit_y)

btd_final_bf <- btd_intermediate_balanced1 %>%
  filter(year >= 2012,
         product %in% c("Biodiesel","Renewable diesel","Bioethanol","ETBE")) %>%
  mutate(product = case_when(product %in% c("Bioethanol", "ETBE") ~ "Biogasoline",
                             TRUE ~ product)) %>%
  group_by(importer_iso3,exporter_iso3,year,product) %>%
  summarize(value=sum(value, na.rm=TRUE),
            unit = first(unit))

btd_intermediate_other <- btd_intermediate_balanced1 %>%
  filter(year >= 2012, 
         !(product %in% c("Biodiesel", "Renewable diesel", "Bioethanol", "ETBE")),
         !is.na(product))



###########################################################
########### SAVING FINAL DATASETS ########### 
###########################################################

setwd("/home/mmondolfo/fabio_bfp/")

dir.create("inputs_for_final_data", recursive = TRUE, showWarnings = FALSE)

saveRDS(supply_final_bf, "inputs_for_final_data/supply_final_bf.rds")
saveRDS(btd_final_bf, "inputs_for_final_data/btd_final_bf.rds")
saveRDS(y_final_bf, "inputs_for_final_data/y_final_bf.rds")
saveRDS(btd_intermediate_other, "intermediate_data/btd_intermediate_other.rds")


