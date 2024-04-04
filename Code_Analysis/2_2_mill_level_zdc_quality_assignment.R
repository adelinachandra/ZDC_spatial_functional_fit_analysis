# Tue Nov 15 13:28:37 2022 ------------------------------
## ---------------------------
## Title: "Mill-level score aggregation for ZDC qualities" 
## Author: "Adelina Chandra"
## Notes: 
##  This script aims to determine ZDC quality at the mill-level 
##  for 2018-2020, using different score aggregation methods.
##  
## ---------------------------

# The input data consists of the following components:
# a) Refined Refinery Groups' links to Mills and ZDC scores as 'Buyer's score'
# b) Refined Mill Groups' links to Mills and ZDC scores as 'Owner's score'
# c) Refined mill-level scores with the mill's Buyer and Owner's scores
# d) Mill locations

# In this script, the score details for input data and aggregation processes are as follows: 3 = high, 2 = low, 1 = zero, 0 = unmatched with any refineries or mills.
# Statistics included in the aggregation functions: min, max, mean, wmean (weighted mean), wmode (weighted mode)
# The weight is based on TRASE's volume flow.
# Detailed functions for aggregation:
# a) all_includeZero: The unmatched mills were included, resulting in many 0 values.
# b) all_nonZero: The unmatched mills were excluded.
# c) mode_nonZero: The unmatched mills were excluded only in the weighted mode calculation.
# For a robustness check, we aggregated the scores using various methods. However, for the study, we specifically utilized 'all_nonZero'."

# The output of this script presents the results using the 'all_nonZero' method with recoded scale, as follows:
# 2, 1, 0 represent 'high-quality', 'low-quality', 'zero' ZDCs, respectively. NA indicates unmatched/unknown.
# However, there is a dedicated script section where users can export the results from other score aggregations for mill-level scores in 2018-2020.

library(ggplot2)
library(tidyverse)

#---- Set up directory -----------------------------------------------------------------------------------------------------------------
# Specify your working directory
dir_path <- "path/to/your/working/directory"

# Specify the folder names
folders <- c("Data_output", "Data_input/In_process")

# Create the folders within the working directory if they don't exist
for (folder in folders) {
  folder_path <- file.path(dir_path, folder)
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
}

#set working directory
setwd(dir_path)

#---- Read data  -----------------------------------------------------------------------------------------------------------------
# Refined list for Refinery, Mill Groups, and Mills
TRASE_list_by_REF <- read_rds("Data_input/In_process/TRASE_list_by_REF_GROUP.rds") %>% select(-smh_ref)
TRASE_list_by_MILL_GROUP <- read_rds("Data_input/In_process/TRASE_list_by_MILL_GROUP.rds") %>% select(-smh_mill)
TRASE_list_Ref_Mill <- read_rds("Data_input/In_process/TRASE_list_by_REF_MILL_GROUP.rds") %>% select(-smh_ref, -smh_mill)

# Mill locations
# Pubicly accessible at https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/SMPITC
mills_locs <- read_rds("Supporting_files/mills_locations.rds") 

#-------- Define functions for score aggregation -------------------------------------------------------------------------------------------------------------
# Weighted mode, weight: 'volume'
weighted_mode <- function(x, volume){
  df <- aggregate(volume, list(group=x), sum)
  df[which.max(df[,2]), "group"]
}

# The unmatched mills were excluded only in weighted mode calculation.
score_mode_non_zero <- function(df) {
  df_grp <- df %>% 
    group_by(mill_id, year) %>% 
    summarise(across(starts_with("NDPE"), list(mean = mean, wmean = ~weighted.mean(., w = volume), max = max, min= min))) %>%  
    ungroup()
  
  df_mod_exclude <- df %>% 
    filter(if_any(contains("NDPE"), ~. > 0)) %>% 
    group_by(mill_id, year) %>% 
    summarise(across(starts_with("NDPE"), list(wmode = ~weighted_mode(., volume)))) %>% 
    ungroup()
  
  df_comb <- df_grp %>% 
    left_join(df_mod_exclude, by = c("mill_id", "year"))

  return(df_comb)
}

# The unmatched mills were excluded.
score_all_non_zero <- function(df) {
  df_grp <- df %>% 
    filter(if_any(contains("NDPE"), ~. > 0)) %>% 
    group_by(mill_id, year) %>% 
    summarise(across(starts_with("NDPE"), list(mean = mean, wmean = ~weighted.mean(., w = volume), max = max, min= min, wmode = ~weighted_mode(., volume)))) %>%  
    ungroup()
  return(df_grp) 
}

# The unmatched mills were included, therefore many 0 values.
score_all_include_zero <- function(df) {
  df_grp <- df %>% 
    group_by(mill_id, year) %>% 
    summarise(across(starts_with("NDPE"), list(mean = mean, wmean = ~weighted.mean(., w = volume), max = max, min= min, wmode = ~weighted_mode(., volume)))) %>%  
    ungroup()
  return(df_grp) 
}

#---- Score aggregation -----------------------------------------------------------------------------------------------------------------
# Function: mode_non_zero
smry_by_ref_mode_nonZero <- score_mode_non_zero(TRASE_list_by_REF)
smry_by_mill_mode_nonZero <- score_mode_non_zero(TRASE_list_by_MILL_GROUP)
  
smr_NDPE_mode_nonZero <- smry_by_ref_mode_nonZero %>% 
  full_join(smry_by_mill_mode_nonZero, by = c("mill_id", "year")) 

# Function: all_non_zero
smry_by_ref_all_nonZero <- score_all_non_zero(TRASE_list_by_REF)
smry_by_mill_all_nonZero <- score_all_non_zero(TRASE_list_by_MILL_GROUP)

smr_NDPE_all_nonZero <- smry_by_ref_all_nonZero %>%  
  full_join(smry_by_mill_all_nonZero, by = c("mill_id", "year")) 

# Function: all_include_zero
smry_by_ref_all_includeZero <- score_all_include_zero(TRASE_list_by_REF)
smry_by_mill_all_includeZero <- score_all_include_zero(TRASE_list_by_MILL_GROUP)

smr_NDPE_all_includeZero <- smry_by_ref_all_includeZero %>% 
  full_join(smry_by_mill_all_includeZero, by = c("mill_id", "year")) 

#---- Checks -----------------------------------------------------------------------------------------------------------------
# 1) Check values 
sapply(smr_NDPE_mode_nonZero %>% filter(year == 2020)%>% 
         select(starts_with("NDPE_score")) %>% 
         summarise(across(everything(), ~sum(. == 0, na.rm = TRUE))), sum)

sapply(smr_NDPE_all_nonZero %>% filter(year == 2020)%>% 
         select(starts_with("NDPE_score")) %>% 
         summarise(across(everything(), ~sum(. == 0, na.rm = TRUE))), sum)

sapply(smr_NDPE_all_includeZero %>% filter(year == 2020)%>% 
         select(starts_with("NDPE_score")) %>% 
         summarise(across(everything(), ~sum(. == 0, na.rm = TRUE))), sum)

# 2) Check: the 'wmode' in smr_NDPE_mode_nonZero and smr_NDPE_all_nonZero should be the same.
check_wmode <- smr_NDPE_mode_nonZero %>% select(mill_id, year, NDPE_score_ref_wmode) %>% rename(modenonZero = NDPE_score_ref_wmode) %>% 
  left_join(smr_NDPE_all_nonZero %>% select(mill_id, year, NDPE_score_ref_wmode) %>% rename(allnonZero = NDPE_score_ref_wmode), by = c("mill_id", "year"))

# Are they the same?
all.equal(check_wmode$modenonZero, check_wmode$allnonZero, check.attributes = FALSE) 
# great!

#---- Correlation -----------------------------------------------------------------------------------------------------------------
cor_cal <- function(x){
  cor_M <- x %>% 
    select(starts_with("NDPE")) %>% 
    rename_with(~str_remove(., "NDPE_score_") %>% 
                  str_replace("_", " ") %>% 
                  str_replace("ref", "ref") %>% 
                  str_replace("wmean", "w-mean") %>% 
                  str_replace("wmode", "w-mode")) %>% 
    
    cor(use = "pairwise.complete.obs") #any pairs with a NA match gets removed
  
  ## cor for printing
  cor_M_c <- round(cor_M, 2)
  cor_M_c[lower.tri(cor_M_c)] <- ""
  cor_M_c
  
  knitr::kable(cor_M_c)
}

cor_cal(smr_NDPE_mode_nonZero)
cor_cal(smr_NDPE_all_nonZero)
cor_cal(smr_NDPE_all_includeZero)

#---- Visualization and export data -----------------------------------------------------------------------------------------------------------------
# Function to convert data to long format 
to_long <- function(x) {
  smry_NDPE <- x %>% 
    pivot_longer(starts_with("NDPE"),
                 values_to  ="NDPE_score",
                 names_to = "variable") %>% 
    mutate(group = str_extract(variable, "ref|mill") %>% 
             recode(ref = "By refinery",
                    mill = "By mill"),
           stat =  str_extract(variable, "wmean|mean|max|min|wmode") %>% 
             recode("wmean" = "w-mean", "wmode" = "w-mode"))
  return(smry_NDPE) 
}

smr_NDPE_mode_nonZero_long <- to_long(smr_NDPE_mode_nonZero)
smr_NDPE_all_nonZero_long <- to_long(smr_NDPE_all_nonZero)
smr_NDPE_all_includeZero_long <- to_long(smr_NDPE_all_includeZero)

visu_by_refi_2020 <- function(x_long) {
  mill_dens_ref <- x_long %>% 
    rename(stats = stat) %>% 
    filter(group == "By refinery" & year == 2020) %>% 
    ggplot(aes(x = NDPE_score)) +
    geom_histogram() +
    facet_wrap(~stats) +
    labs(x = "NDPE score")+
    ggtitle("Mill level score by refinery in 2020")
  
  mill_dens_ref
}

visu_by_mill <- function(x_long) {
  mill_dens_mill <- x_long %>% 
    #rename(stats = stat) %>% 
    group_by(mill_id, year, group) %>% 
    summarise(NDPE_score_mean= mean(NDPE_score)) %>% 
    filter(group == "By mill") %>% 
    ggplot(aes(x = NDPE_score_mean)) +
    geom_histogram() +
    facet_wrap(~year) +
    labs(x = "NDPE score")+
    ggtitle("Mill level score by mill group in 2018-2020")
  
  mill_dens_mill
}

# Check visualization for all_nonZero method 
smr_NDPE_all_nonZero_ref20 <- visu_by_refi_2020(smr_NDPE_all_nonZero_long)
smr_NDPE_all_nonZero_ref20
smr_NDPE_all_nonZero_mill <- visu_by_mill(smr_NDPE_all_nonZero_long)
smr_NDPE_all_nonZero_mill

#---- Exporting mill-level ZDCs using various score aggregation methods -----------------------------------------------------------------------------------------------------------------
# Please note that the score used in these exported files are 3, 2, 1, and 0, for 'high', 'low', 'zero', and 'unmatched/unknown', respectively
# Please uncomment this section if you would like to export the data.
# # Determine one or multiple data you would like to export (below, for example "smr_NDPE_all_nonZero")
# file_names <- c("smr_NDPE_all_nonZero") 
# 
# # Specify the output path
# dir_path <- "path/to/the/output/folder"
# 
# for (file_name in file_names) {
#   file_path <- paste0(dir_path, file_name, ".csv")
#   write_csv(get(file_name), file = file_path)
# }

#---- Mill-level score for grid_level analysis ----------------------------------------------------------------------------------------------------------------- 
# Prepare data for Spatial Fit analysis 
# For robustness check, we aggregated the score using different methods. However, for the study, we used "smr_NDPE_all_nonZero". 
all_nonZero_prep <- smr_NDPE_all_nonZero %>% 
  select(mill_id, year, NDPE_score_ref_max, NDPE_score_ref_min, NDPE_score_ref_wmode, NDPE_score_mill_wmode) %>% 
  mutate(year = as.integer(year)) 

# Recode 321 to 210, where 2, 1, 0 represent 'high-quality', 'low-quality', 'zero' ZDCs, respectively. NA indicates unmatched/unknown.
all_nonZero_prep_recode <- all_nonZero_prep %>% 
  mutate(across(starts_with("NDPE_score"), ~case_when(. == 3 ~ 2, . == 2 ~ 1, . == 1 ~ 0, TRUE ~ .)))

all_nonZero_df <- mills_locs %>% 
  right_join(all_nonZero_prep_recode, by = "mill_id") %>% 
  rename("max_buyers_ZDC_score" = "NDPE_score_ref_max", 
         "min_buyers_ZDC_score" = "NDPE_score_ref_min",
         "weighted_mode_buyers_ZDC_score" = "NDPE_score_ref_wmode",
         "mill_owners_ZDC_score" = "NDPE_score_mill_wmode")

#---- Export data -----------------------------------------------------------------------------------------------------------------
# Mill-level ZDC quality score 
# The score details: 2, 1, 0 represent 'high-quality', 'low-quality', 'zero' ZDCs, respectively. NA indicates unmatched/unknown.
write_csv(all_nonZero_df, "Chandraetal2024_mill_level_ZDC_quality_2018-2020.csv")
