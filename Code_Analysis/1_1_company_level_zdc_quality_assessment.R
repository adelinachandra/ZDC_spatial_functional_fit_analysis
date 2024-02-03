##  Mon Nov  7 15:22:59 2022 
## ---------------------------
## Title: "ZDC quality assessment for SPOTT companies in 2018-2020" 
## Author: "Adelina Chandra" 
## Notes: 
##   This script aims to determine ZDC quality for Spatial and Functional fit analysis.
##   We provide score calculations not only for 2020 but also for 2018 and 2019, as explained in the Manuscript.
##   However, the output for 2020 is the one used for this Manuscript.
##   Additionally, we also provide input data for assessing ZDC 'Policy' comprehensiveness, which does not include implementation criteria.
##   Please note that the scripts should be executed in the described sequence (see the the GitHub repository) for the proper flow of the analysis.
## ---------------------------

# The input data contains pre-processed data from SPOTT (https://www.spott.org/), which includes the following steps:
# 1. We compiled SPOTT data for 2018, 2019, and 2020. The source data for 2018 was SPOTT data assessed in Oct-19, for 2019 in Nov-20, and for 2020 in Nov-21.
# 2. Due to indicator inconsistency in the SPOTT data across these years, we matched the indicators and adjusted them based on the SPOTT indicator for 2019 (see technical note for SPOTT assessment in 2020).
# 3. We fixed encoding issues on company names.
# 4. We selected the indicators of interest as explained in the manuscript.
# 5. We calculated the indicator score (see variable: 'perc') by dividing the value of 'points' by the maximum possible points (max.pt).
#   a. When max.point is 0, this means the indicator is not applicable for the company; see further details in the SPOTT's technical note.
#   b. In SPOTT, a company may be given an extra 1 point under certain conditions (e.g., if externally verified by RSPO); we exclude this additional point to handle inconsistency across indicators.
# 6. We divided the indicators into 'policy' or 'implementation' indicators, as outlined in the 'class' variable in the data.

# The output of this script is company-level ZDC quality in 2018-2020 and ZDC quality via smallholder inclusion in 2020

library(ggplot2)
library(tidyverse)

#---- Set up directory-----------------------------------------------------------------------------------------------------------------
# Specify your working directory
dir_path <- "path/to/your/working/directory"

# Specify the folder names
folders <- c("Data_output")

# Create the folders within the working directory if they don't exist
for (folder in folders) {
  folder_path <- file.path(dir_path, folder)
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
}

#set working directory
setwd(dir_path)

#---- Read data -----------------------------------------------------------------------------------------------------------------
# Please make sure that the data has been unzipped
# Set the data directory
spott <- read_csv("Data_input/Chandraetal2024_spott_input_2018-2020.csv")
class <- read_csv("Data_input/Chandraetal2024_spott_classification.csv")

#---- Prepare conditions -----------------------------------------------------------------------------------------------------------------
# Set up threshold (1, 0.75 or 0.5) for the necessary criteria, see further details in the Supplementary Material.
nec_1 <- c(39,31)
nec_075 <- c(160,162,32) 
nec_05 <- c(68,33,167,180,181,168,41,161,163,34,149,152)

# Filter which companies are eligible to be assigned as having 'high-quality' ZDCs based on whether they have ZDC applies up to the supplier level (indicator #39).
eligible <- spott %>% filter(number == 39 & perc == 1) 
eligible_2018 <- eligible %>% filter(year ==2018) %>% pull(company)
eligible_2019 <- eligible %>% filter(year ==2019) %>% pull(company)
eligible_2020 <- eligible %>% filter(year ==2020) %>% pull(company)

# Second classification conditions, we allowed for multiple avenues to meet a threshold (via either-or criteria) 
# select indicators for smallholder criteria (smh_num), compliance support (hi_risk_mill), and traceability (trace_own_mill). 
smh_num <- c(163,161,149,152)
hi_risk_mill <- c(167,168)
trace_own_mill <- c(31,32)

#---- Incorporate conditions -----------------------------------------------------------------------------------------------------------------
# Assign score for assessment based on threshold
spott_nec <- spott %>% 
  mutate(assess = case_when(number %in% nec_1 & perc == 1 ~ "necessary",
                            number %in% nec_075 & perc >= 0.75 ~ "necessary",
                            number %in% nec_05 & perc >= 0.5 ~ "necessary",
                            is.na(perc) ~ NA,
                            TRUE ~ "below necessary"))

# Custom function to incorporate second classification conditions, keep only eligible companies
either_fun <- function(main_data, list_either) {
  nec_ei<- main_data %>% 
    filter(number %in% list_either) %>% 
    group_by(company, year) %>% 
    mutate(class_ei = ifelse(any(assess == "necessary"), 1,0)) %>% 
    filter(class_ei == 1) %>% 
    ungroup() %>% 
    mutate(class_ei = ifelse(is.na(assess), NA, class_ei)) %>% 
    select(company, year, number, class_ei)
  return(nec_ei)
}

nec_ei_smh <- either_fun(spott_nec, smh_num)
nec_ei_risk <- either_fun(spott_nec, hi_risk_mill)
nec_ei_mill <- either_fun(spott_nec, trace_own_mill)

nec_ei_all <- rbind(nec_ei_risk, nec_ei_smh, nec_ei_mill)

# Combine data
zdc_score_assessment <- spott_nec %>% 
  left_join(nec_ei_all, by= c("company", "year", "number")) %>% 
  mutate(either = ifelse(is.na(class_ei), assess, "necessary"))

#---- Determine ZDC quality based on Policy and Implementation criteria for 2018, 2019, and 2020 -----------------------------------------------------------------------------------------------------------------
# Both policy and implementation for Spatial Fit analysis 
# Score scale used is 0,1,2 for 'zero', 'low' and 'high', respectively.
zdc_quality <- zdc_score_assessment %>% 
  group_by(company, year, either) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = either, values_from = count) %>% 
  ungroup() %>% 
  mutate(eligible = case_when(company %in% eligible_2018 & year == 2018 ~ "eligible",
                              company %in% eligible_2019 & year == 2019 ~ "eligible",
                              company %in% eligible_2020 & year == 2020 ~ "eligible",
                              TRUE ~ "ineligible"),
         policy_imp_class = case_when(is.na(`below necessary`) & eligible == "eligible" ~ "high",
                                  `below necessary` > 0 & eligible == "eligible" ~ "low",
                                  eligible != "eligible" ~ "zero"),
         policy_imp_score = case_when(policy_imp_class == "high" ~2,
                                      policy_imp_class == "low" ~ 1,
                                      policy_imp_class == "zero" ~0)) %>% 
  select(company, year, policy_imp_class) %>% 
  rename("zdc_quality_category" = "policy_imp_class")

#---- Determine only ZDC 'Policy' score by category -----------------------------------------------------------------------------------------------------------------
zdc_policy <- zdc_score_assessment %>% 
  filter(class == "policy")%>% 
  filter(year == 2020) %>% 
  left_join(class, by = "number") %>% 
  group_by(company, year, type, either) %>% 
  summarise(count = n()) %>%  
  pivot_wider(names_from = either, values_from = count) %>% 
  ungroup() %>% 
  mutate(eligible = case_when(company %in% eligible_2020 & year == 2020 ~ "eligible",
                              TRUE ~ "ineligible"),
         by_class = case_when(is.na(`below necessary`) & eligible == "eligible" ~ "high",
                                      `below necessary` > 0 & eligible == "eligible" ~ "low",
                                      eligible != "eligible" ~ "zero")) %>% 
  mutate(by_class = case_when(is.na(`below necessary`)&is.na(`necessary`) ~ NA,
                              TRUE ~ by_class)) %>%
  select(company, year, type, by_class) %>% 
  pivot_wider(names_from = type, values_from= by_class) %>% 
  left_join(zdc_quality %>% filter(year == 2020)) 
  
#---- Determine ZDC quality via Independent Smallholder inclusion for 2020 -----------------------------------------------------------------------------------------------------------------
# Here we focus on Independent Smallholders criteria (indicator number 162,163,152) for Functional Fit analysis
zdc_smalholder_inclusion <- zdc_score_assessment %>% 
  filter(number %in% c(162,163,152)) %>% 
  left_join(class, by = "number") %>% 
  group_by(company, year, type, either) %>% 
  summarise(count = n()) %>%  
  pivot_wider(names_from = either, values_from = count) %>% 
  ungroup() %>% 
  mutate(eligible = case_when(company %in% eligible_2018 & year == 2018 ~ "eligible",
                              company %in% eligible_2019 & year == 2019 ~ "eligible",
                              company %in% eligible_2020 & year == 2020 ~ "eligible",
                              TRUE ~ "ineligible"),
         by_class = case_when(is.na(`below necessary`) & eligible == "eligible" ~ "high",
                              `below necessary` > 0 & eligible == "eligible" ~ "low",
                              eligible != "eligible" ~ "zero")) %>% 
  mutate(by_class = case_when(is.na(`below necessary`)&is.na(`necessary`) ~ NA,
                              TRUE ~ by_class)) %>% 
  select(company, year, type, by_class) %>% 
  pivot_wider(names_from = type, values_from= by_class) 

#---- Compile final data for export -----------------------------------------------------------------------------------------------------------------
zdc_data <- zdc_quality %>% 
  left_join(zdc_smalholder_inclusion, by = c("company", "year"))

#---- Export data -----------------------------------------------------------------------------------------------------------------
# ZDC quality score and via smallholder inclusion for 2018, 2019, and 2020
write_csv(zdc_data, "Chandraetal2024_SPOTT_ZDC_quality_2018-2020.csv")
