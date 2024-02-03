# Tue Nov 15 13:28:37 2022 ------------------------------
## ---------------------------
## Title: "Mill-level score aggregation for ZDC qualities" 
## Author: "Adelina Chandra"
## Notes: 
##  This script aims to determine ZDC quality via smallholder inclusion at the mill-level 
##  for 2020, for only known mills.
##
## ---------------------------

# The input data consists of the following components:
# a) Refined Refinery Groups' links to Mills and ZDC smallholder inclusion scores as 'Buyer's score'
# b) Refined Mill Groups' links to Mills and ZDC smallholder inclusion scores as 'Owner's score'
# c) Refined mill-level scores with the mill's Buyer and Owner's smallholder inclusion scores
# d) Mill locations

# In this script, the score details for input data and aggregation processes are as follows: 3 = high, 2 = low, 1 = zero, 0 = unmatched with any refineries or mills.
# Statistics included in the aggregation functions: min, max, mean, wmean (weighted mean), wmode (weighted mode)
# The weight is based on TRASE's volume flow.
# Here, we focus on using one aggregation method for efficiency. Detailed functions for aggregation:
# a) all_nonZero: The unmatched mills were excluded.

# The output of this script presents the results using the 'all_nonZero' method with recoded scale, as follows:
# 2, 1, 0 represent 'high-quality', 'low-quality', 'zero' ZDCs, respectively. NA indicates unmatched/unknown.

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
#---- Load data  -----------------------------------------------------------------------------------------------------------------
# Refined list for Refinery, Mill Groups, and Mills, keep only 2020 and rename column
TRASE_list_by_REF <- read_rds("Data_input/In_process/TRASE_list_by_REF_GROUP.rds") %>% select(-NDPE_score_ref) %>% 
  rename(NDPE_score_ref = smh_ref) %>%  filter(year == 2020)
TRASE_list_by_MILL_GROUP <- read_rds("Data_input/In_process/TRASE_list_by_MILL_GROUP.rds") %>% select(-NDPE_score_mill) %>% 
  rename(NDPE_score_mill= smh_mill) %>% filter(year == 2020)
TRASE_list_Ref_Mill <- read_rds("Data_input/In_process/TRASE_list_by_REF_MILL_GROUP.rds") %>% select(-NDPE_score_ref, -NDPE_score_mill) %>% 
  rename(NDPE_score_ref = smh_ref, NDPE_score_mill = smh_mill) %>% 
  filter(year == 2020)

# Mill locations
mills_locs <- read_rds("Supporting_files/mills_locations.rds") 

#-------- Define functions for score aggregation -------------------------------------------------------------------------------------------------------------
# Weighted mode, weight: 'volume'
weighted_mode <- function(x, volume){
  df <- aggregate(volume, list(group=x), sum)
  df[which.max(df[,2]), "group"]
}

# The unmatched mills were excluded.
score_all_non_zero <- function(df) {
  df_grp <- df %>% 
    filter(if_any(starts_with("NDPE"), ~. > 0)) %>% 
    group_by(mill_id) %>% 
    summarise(across(starts_with("NDPE"), list(mean = mean, wmean = ~weighted.mean(., w = volume), max = max, min= min, wmode = ~weighted_mode(., volume)))) %>%  
    ungroup()
  return(df_grp) 
}

#---- Score aggregation -----------------------------------------------------------------------------------------------------------------
# Function: all_non_zero
smry_by_ref_all_nonZero <- score_all_non_zero(TRASE_list_by_REF)
smry_by_mill_all_nonZero <- score_all_non_zero(TRASE_list_by_MILL_GROUP)

smr_NDPE_all_nonZero <- smry_by_ref_all_nonZero %>% 
  full_join(smry_by_mill_all_nonZero , by = c("mill_id")) 

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

smr_NDPE_all_nonZero_long <- to_long(smr_NDPE_all_nonZero)

sapply(smr_NDPE_all_nonZero, unique)

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

cor_cal(smr_NDPE_all_nonZero)

#---- Visualization and export data -----------------------------------------------------------------------------------------------------------------
visu_by_ref <- function(x_long) {
  mill_dens_ref <- x_long %>% 
    rename(stats = stat) %>% 
    filter(group == "By refinery") %>% 
    ggplot(aes(x = NDPE_score)) +
    geom_histogram() +
    facet_wrap(~stats) +
    labs(x = "NDPE score")+
    ggtitle("Mill level NDPE score via smallholder inclusion by refinery in 2020")
  
  mill_dens_ref
}


visu_by_mill <- function(x_long) {
  mill_dens_mill <- x_long %>% 
    #rename(stats = stat) %>% 
    group_by(mill_id, group) %>% 
    summarise(NDPE_score_mean= mean(NDPE_score)) %>% 
    filter(group == "By mill") %>% 
    ggplot(aes(x = NDPE_score_mean)) +
    geom_histogram() +
    labs(x = "NDPE score")+
    ggtitle("Mill level NDPE score via smallholder inclusion score by mill group in 2020")
  
  mill_dens_mill
}


smr_NDPE_all_nonZero_ref_20 <- visu_by_ref(smr_NDPE_all_nonZero_long)
smr_NDPE_all_nonZero_ref_20
smr_NDPE_all_nonZero_mill_20 <- visu_by_mill(smr_NDPE_all_nonZero_long)
smr_NDPE_all_nonZero_mill_20

#---- Mill-level score for grid_level analysis ----------------------------------------------------------------------------------------------------------------- 
# Prepare data for Functional Fit analysis 
all_nonZero_prep <- smr_NDPE_all_nonZero %>% 
  select(mill_id, NDPE_score_ref_max, NDPE_score_ref_min, NDPE_score_ref_wmode, NDPE_score_mill_wmode) 

# Recode 321 to 210, where 2, 1, 0 represent 'high-quality', 'low-quality', 'zero' ZDCs, respectively. NA indicates unmatched/unknown.
all_nonZero_prep_recode <- all_nonZero_prep %>% 
  mutate(across(starts_with("NDPE_score"), ~case_when(. == 3 ~ 2, . == 2 ~ 1, . == 1 ~ 0, TRUE ~ .)))

all_nonZero_df <- mills_locs %>% 
  right_join(all_nonZero_prep_recode, by = "mill_id") %>% 
  rename("max_buyers_ZDC_score_smh_inc" = "NDPE_score_ref_max", 
         "min_buyers_ZDC_score_smh_inc" = "NDPE_score_ref_min",
         "weighted_mode_buyers_ZDC_score_smh_inc" = "NDPE_score_ref_wmode",
         "mill_owners_ZDC_score_smh_inc" = "NDPE_score_mill_wmode")

#---- Export data -----------------------------------------------------------------------------------------------------------------
# Mill-level ZDC quality via smallholder inclusion 
# The score details: 2, 1, 0 represent 'high-quality', 'low-quality', 'zero' ZDCs, respectively. NA indicates unmatched/unknown.
write_csv(all_nonZero_df, "Chandraetal2024_mill_level_ZDC_quality_via_smh_inclusion_2020.csv")
