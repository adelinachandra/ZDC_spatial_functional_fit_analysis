# Thu Aug 24 10:32:22 2023 ------------------------------
## ---------------------------
## Title: "Matching TRASE and SPOTT data and ZDC score assignment" 
## Author: "Adelina Chandra"
## Notes: 
##  This script aims to link SPOTT companies to TRASE companies, 
##  to assign ZDC score at the mill-level,
##  and prepare the input for score aggregation for the next step.
##  
## ---------------------------

# The input data used in this script consist of 'zdc_quality' data, the output SPOTT score data produced in previous script. 
# 'matching_id', the master list used to link TRASE company names with the SPOTT company names. 
# 'ts_list', the master TRASE list showing linkage between a mill and refinery group sourcing from them and mill group. 

# The output of this script is a) ZDC quality with rescaled score (3,2,1 for 'high', 'low', and 'zero', respectively), 
# b) Refined Refinery Groups' links to Mills and ZDC score as 'Buyer's score', 
# c) Refined Mill Groups' links to Mills and ZDC score as 'Owner's score', and 
# d) Refined mill-level score with mill's Buyer and Owner's score

library(sf)
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
# ZDC quality score for 2018, 2019, and 2020
zdc_quality <- read_csv("Chandraetal2024_SPOTT_ZDC_quality_2018-2020.csv")

# Matching list for TRASE and SPOTT companies
matching_id <- read_rds("Supporting_files/spott_trase_matching_company_list.rds") 

# TRASE cleaned list 
ts_list <- read_rds("Supporting_files/TRASE_list.rds") 

#---- Prepare data -----------------------------------------------------------------------------------------------------------------
# Rescale score 
spott_score <- zdc_quality %>% 
  rename(final_class = zdc_quality_category) %>% 
  mutate(NDPE_score = case_when(final_class == "zero" ~ 1,
                                final_class == "low" ~2,
                                final_class == "high" ~3),
         smh_inclusion = case_when(smh_inclusion == "zero" ~ 1,
                                   smh_inclusion == "low" ~2,
                                   smh_inclusion == "high" ~3)) 

# Separate matching list for different type of company and rename a) TRASE company based on the type and b) SPOTT_company into 'company'
ts_refine <- matching_id %>% filter(TRASE_group_type == "refinery_group") %>% select (-TRASE_group_type) %>% rename("REFINERY_GROUP" = "TRASE_company", "company" = "SPOTT_company")
ts_mill <- matching_id %>% filter(TRASE_group_type == "mill_group") %>% select (-TRASE_group_type) %>% rename("MILL_GROUP" = "TRASE_company", "company" = "SPOTT_company")
ts_mill_own <- matching_id %>% filter(TRASE_group_type == "mill_id") %>%select (-TRASE_group_type) %>% rename("MILL_ID" = "TRASE_company", "company" = "SPOTT_company")

#---- Preparation for matching -----------------------------------------------------------------------------------------------------------------
# Assign score to different type of company groups
# Refinery group, assign score as Buyers' score (suffix 'ref)
ts_ref_score <- ts_refine %>% 
  left_join(spott_score,by = "company") %>% 
  rename(NDPE_score_ref = NDPE_score, smh_ref = smh_inclusion, ZDC_commit = final_class)%>%
  select(-company, -ZDC_commit)

# Mill Group, assign score as Owner's score (suffix 'mill)
ts_mill_g_score <- ts_mill %>%  
  left_join(spott_score, by = "company", relationship = "many-to-many") %>% 
  rename(NDPE_score_mill = NDPE_score, smh_mill = smh_inclusion, ZDC_commit = final_class)%>%
  select(-company, -ZDC_commit)

# Mill Ownership, assign score as Direct Owner's score (suffix 'mill_o) - this is temporary since it will be merged with 'ts_mill_g_score' in the next step.
ts_mill_own_score <- ts_mill_own %>%  #mill owned 
  left_join(spott_score,by = "company", relationship = "many-to-many") %>% 
  rename(NDPE_score_mill_o = NDPE_score, smh_mill_o = smh_inclusion, ZDC_commit = final_class, mill_id = MILL_ID)%>%
  select(-company, -ZDC_commit)

# Combine mill_own and mill_group to add additional links
ts_mill_merge <- ts_list %>%
  filter(mill_id!="M-X") %>% #exclude 3 entries
  left_join(ts_mill_g_score %>%
              mutate(mill_group_matched = TRUE), by = c("MILL_GROUP", "year")) %>%
  left_join(ts_mill_own_score %>%
              mutate(mill_o_matched = TRUE), by = c("mill_id", "year")) %>%
  within(., NDPE_score_mill <- ifelse(!is.na(NDPE_score_mill_o), NDPE_score_mill_o, NDPE_score_mill)) %>% #overwrite column score to add mill own - zero mill score in overwritten elements
  select(-NDPE_score_mill_o) %>%
  within(., mill_group_matched <- ifelse(!is.na(mill_o_matched), mill_o_matched, mill_group_matched)) %>% #overwrite checking column
  select(-mill_o_matched) %>%
  within(., smh_mill <- ifelse(!is.na(smh_mill_o), smh_mill_o, smh_mill)) %>% #overwrite column score to add mill own - zero mill score in overwritten elements
  select(-smh_mill_o) %>%
  mutate(across(ends_with("_matched"), ~replace_na(., FALSE)))

## Merge Refinery score and Mill score
ts_ref_mill_merge <- ts_mill_merge %>%
  left_join(ts_ref_score %>%
              mutate(refinery_matched = TRUE), by = c("REFINERY_GROUP", "year")) %>%
  mutate(across(ends_with("_matched"), ~replace_na(., FALSE)))

head(ts_ref_mill_merge)
summary(ts_ref_mill_merge) # A lot of NAs in score columns

## Assign zero (0) score if the company did not match
TRASE_list_Ref_Mill <- ts_ref_mill_merge%>%
  mutate(across(starts_with("NDPE"), ~replace_na(., 0))) %>%
  mutate(is_owner_known = MILL_GROUP!="UNKNOWN",
         is_ref_known = REFINERY_GROUP!="UNKNOWN",
         owner_status = case_when(MILL_GROUP == "UNKNOWN" ~ "unknown",
                                  MILL_GROUP != "UNKNOWN" & mill_group_matched~ "known_matched",
                                  MILL_GROUP != "UNKNOWN" & !mill_group_matched~ "known_unmatched",
                                  TRUE ~ "PROBLEM"),
         refiner_status = case_when(REFINERY_GROUP == "UNKNOWN" ~ "unknown",
                                    REFINERY_GROUP != "UNKNOWN" & refinery_matched~ "known_matched",
                                    REFINERY_GROUP != "UNKNOWN" & !refinery_matched~ "known_unmatched",
                                    TRUE ~ "PROBLEM"))

#---- Prepare score by TRASE company group type for aggregation -----------------------------------------------------------------------------------------------------------------
# List for Refinery Group 
TRASE_list_by_REF <- ts_list %>%
  distinct(REFINERY_GROUP, mill_id, year, volume) %>%
  left_join(ts_ref_score, by = c("REFINERY_GROUP", "year"), multiple="all") %>%
  mutate(across(starts_with("NDPE"), ~replace_na(., 0)))

# List for both Mill Group and additional Mill ID from matching
TRASE_list_by_MILL_GROUP <- ts_list %>%
  distinct(MILL_GROUP, mill_id, year, volume) %>%
  left_join(ts_mill_g_score, by = c("MILL_GROUP", "year")) %>%
  left_join(ts_mill_own_score, by = c("mill_id", "year")) %>% 
  within(., NDPE_score_mill <- ifelse(!is.na(NDPE_score_mill_o), NDPE_score_mill_o, NDPE_score_mill)) %>% #overwrite column score to add mill own - zero mill score in overwriting elements 
  select(-NDPE_score_mill_o) %>%
  within(., smh_mill <- ifelse(!is.na(smh_mill_o), smh_mill_o, smh_mill)) %>% #overwrite column score to add mill own - zero mill score in overwritten elements 
  select(-smh_mill_o) %>%
  mutate(across(starts_with("NDPE"), ~replace_na(., 0)))

#---- Export data  -----------------------------------------------------------------------------------------------------------------
# ZDC quality with rescaled score (3,2,1 for 'high', 'low', and 'zero', respectively)
write_rds(spott_score,"Data_input/In_process/SPOTT_ZDC_quality_score_2018-2020.csv")

# Refined Refinery Groups' links to Mills and ZDC score as 'Buyer's score'
write_rds(TRASE_list_by_REF, "Data_input/In_process/TRASE_list_by_REF_GROUP.rds")

# Refined Mill Groups' links to Mills and ZDC score as 'Owner's score'
write_rds(TRASE_list_by_MILL_GROUP, "Data_input/In_process/TRASE_list_by_MILL_GROUP.rds")

# Refined mill-level score with mill's Buyer and Owner's score
write_rds(TRASE_list_Ref_Mill, "Data_input/In_process/TRASE_list_by_REF_MILL_GROUP.rds")

