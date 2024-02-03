# Tue Jan 10 16:13:36 2023 ------------------------------
## ---------------------------
## Title: "Visualization of the results" 
## Author: "Adelina Chandra" 
## Notes: 
##  This steps aim to visualize the results,  
##  producing graph on forest and smallholder palm details.
##  Please note that this code includes visualization for Figure 2Sb in Supplementary Material
## ---------------------------
 
# The input data is a grid-level spatial data, covering both inside and outside mill sheds.
# Please note, that only grid cells located inside mill sheds contain values for each variables.
# Details of dataframe (unit for area is hectare, a pixel area is 400 ha)
# ID: grid ID
# island: Island 
# smh_palm: Total area of smallholder palm in a given pixel
# for20: Total area of remaining forest in a given pixel
# prob: The probability of a given pixel to be converted into oil palm in 2020

# ZDC coverage in the dataframe
# main_mill: ZDC coverage by mill owners
# main_ref: ZDC coverage by buyers
# smh_inc_mill_coverage: ZDC coverage by mill owners via smallholder inclusion
# smh_inc_ref_coverage: ZDC coverage by buyers via smallholder inclusion

# The output of this script is a figure of remaining forests and probability conversion by Mill Owners' ZDCs
# and Smallholder palm area by Mill Owners' ZDCs via smallholder Inclusion
 
library(sf)
library(ggplot2)
library(ggpubr)
library(viridis)
library(tidyverse)

#---- Set up directory -----------------------------------------------------------------------------------------------------------------
# Specify your working directory
dir_path <- "path/to/your/working/directory"

# Specify the folder names
folders <- c("Figure_output")

# Create the folders within the working directory if they don't exist
for (folder in folders) {
  folder_path <- file.path(dir_path, folder)
  if (!file.exists(folder_path)) {
    dir.create(folder_path, recursive = TRUE)
  }
}

#set working directory
setwd(dir_path)
#---- Prepare custom palette -----------------------------------------------------------------------------------------------------------------
# Prepare color-blind friendly palette
terrain5_palette<- viridis_pal(option = "viridis")(3)

#---- Read data -----------------------------------------------------------------------------------------------------------------
# Prepare input data
indo_inside_millshed <- st_read("Data_input/Chandraetal2024_indo_all_variables_grid_inside_millshed.geojson") %>% as_tibble() %>% select(-geometry)

summary(indo_inside_millshed)

#---- Prepare data for visualization -----------------------------------------------------------------------------------------------------------------
#prepare data for visualization of ZDC quality
indo_select <- indo_inside_millshed %>% 
  select(for20, prob, main_mill, main_ref, island) %>% 
  filter(for20!= 0) #exclude non-forest areas

indo_select$main_mill <- factor(indo_select$main_mill, levels = c(0,1,2), labels = c("Zero", "Low", "High"))
indo_select$main_ref <- factor(indo_select$main_ref, levels = c(0,1,2), labels = c("Zero", "Low", "High"))
indo_select$island <- factor(indo_select$island, levels = c("Sumatra", "Kalimantan", "Papua"))

#prepare data for visualization of ZDCs via Smallholder Inclusion 
indo_smh <- indo_inside_millshed %>% 
  select(smh_palm, island, smh_inc_mill_coverage, smh_inc_ref_coverage) %>% 
  filter(smh_palm >0)

indo_smh$smh_inc_mill_coverage <- factor(indo_smh$smh_inc_mill_coverage, levels = c("zero coverage", "low coverage", "high coverage"), labels = c("Zero", "Low", "High"))
indo_smh$smh_inc_ref_coverage <- factor(indo_smh$smh_inc_ref_coverage, levels = c("zero coverage", "low coverage", "high coverage"), labels = c("Zero", "Low", "High"))
indo_smh$island <- factor(indo_smh$island, levels = c("Sumatra", "Kalimantan", "Papua"))

#---- Visualization for ZDC qualities -----------------------------------------------------------------------------------------------------------------
# ZDCs by Mill Owners
# Remaining forest, Mill Owners
indo_mill_forest <- indo_select %>% 
  group_by(main_mill, island) %>% 
  summarise(sum_for20 = sum(for20, na.rm=T)*1e-6) %>% 
  ggplot(aes(x= sum_for20, y = factor(island, levels = c("Papua", "Kalimantan", "Sumatra"), labels = c("Papua", "Kalimantan", "Sumatra")), fill = main_mill)) +
  geom_bar(color = "black", stat= "identity", position= "stack", width = 0.7) +                                                                                            
  scale_fill_manual(values = terrain5_palette) +
  labs(x ="Remaining forest 2020 (Mha)", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    panel.grid = element_blank() # Remove grid lines
  )

indo_mill_forest 

# Average probability of conversion, Mill Owners
means_mill <- indo_select %>%
  group_by(island, main_mill) %>%
  summarize(mean_prob = mean(prob, na.rm = TRUE),
            median_prob = median(prob, na.rm=TRUE),
            sd_prob = sd(prob, na.rm=TRUE))

indo_mill_prob <- ggplot(means_mill, aes(y = factor(island, levels = c("Papua", "Kalimantan", "Sumatra"), labels = c("Papua", "Kalimantan", "Sumatra")), color = main_mill)) +
  geom_point(data = means_mill, aes(x = mean_prob, color = main_mill),  size = 2) +
  geom_text(data = means_mill, aes(x = mean_prob, label = round(mean_prob, 2), color = main_mill),  vjust = -0.5, hjust = 0.2,size = 3) +
  geom_errorbar(aes(xmin=mean_prob-sd_prob, xmax=mean_prob+sd_prob), width=.8,
                position=position_dodge(0.05))+
  geom_hline(data = means_mill, aes(yintercept = factor(island, levels = c("Papua", "Kalimantan", "Sumatra"), labels = c("Papua", "Kalimantan", "Sumatra"))), color = "grey", linetype = "solid", alpha = 0.2) +
  scale_color_manual(values = terrain5_palette) +
  labs(x = "Average of probability of conversion", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    panel.grid = element_blank(), 
  )

indo_mill_prob

#arrange both figures, Mill Owners
combine_mill <- ggarrange(indo_mill_forest, indo_mill_prob, ncol = 2)
annotate_figure(combine_mill, top = text_grob("Remaining forests and probability of conversion in 2020 by Mill Owners' ZDCs", 
                                      color = "black", size = 11))

ggsave("Figure_output/Figure_Remaining_forest_and_probability_of_conversion_2020_by_Mill_Owner_ZDCs.jpeg", dpi=300)

#---- Visualization for ZDC qualities via Smallholder Inclusion -----------------------------------------------------------------------------------------------------------------
# ZDCs via smallholder inclusion by Mill Owners
indo_smh_mill_all_island <- indo_smh %>% 
  group_by(smh_inc_mill_coverage, island) %>% 
  summarise(sum_palm = sum(smh_palm, na.rm=T)*1e-6) %>% 
  ggplot(aes(x= sum_palm, y = factor(island, level = c("Papua", "Kalimantan", "Sumatra")) , fill = smh_inc_mill_coverage )) +
  geom_bar(color = "black", stat= "identity", position= "stack",width = 0.7) +                                                                                            
  scale_fill_manual(values = terrain5_palette) +
  labs(x ="Smallholder palm area 2019 (Mha)", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  #facet_wrap(~island, nrow=3)+
  theme(
    panel.grid = element_blank()  
  )

indo_smh_mill_all_island

indo_smh_mill_zoom_papua <- indo_smh %>% 
  group_by(smh_inc_mill_coverage, island) %>% 
  summarise(sum_palm = sum(smh_palm, na.rm=T)*1e-6) %>% 
  filter(island == "Papua") %>% 
  mutate(sum_palm_kha = sum_palm*1000) %>% 
  ggplot(aes(x= sum_palm_kha, y = island , fill = smh_inc_mill_coverage )) +
  geom_bar(color = "black", stat= "identity", position= "stack",width = 0.7) +                                                                                            
  scale_fill_manual(values = terrain5_palette) +
  labs(x ="Smallholder palm area 2019 (kha)", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    panel.grid = element_blank()  
  )

indo_smh_mill_zoom_papua

mill_arrange_smh <- ggarrange(indo_smh_mill_all_island, indo_smh_mill_zoom_papua, nrow=2, heights = c(1, 0.5),align = "hv")
annotate_figure(mill_arrange_smh, top = text_grob("Independent smallholder palm area by Mill Owners' ZDCs", 
                                             color = "black", size = 11))
ggsave("Figure_output/Figure_Independent_Smallholder_Palm_by_Mill_Owners_ZDCs.jpeg", dpi=300)

#---- Additional visualization for added values: the overlaps between Mill Owners' "Low" & "Zero" ZDCs in Buyers' "High" ZDCs -----------------------------------------------------------------------------------------------------------------
# We present this figure in the Supplementary materials
# Prepare data 
indo_forest_mill_ref_add <- indo_select %>% 
  filter(main_mill != "High" & main_ref == "High") %>% 
  group_by(island, main_mill) %>% 
  summarise(sum_for20 = sum(for20, na.rm= T)*1e-6,
            mean_prob = mean(prob, na.rm = T),
            sd_prob = sd(prob, na.rm = T)) 

indo_forest_mill_ref_add

indo_mill_ref_add_forest <- indo_forest_mill_ref_add %>% 
  ggplot(aes(x= sum_for20, y = factor(island, levels = c("Papua", "Kalimantan", "Sumatra"), labels = c("Papua", "Kalimantan", "Sumatra")), fill = main_mill)) +
  geom_bar(color = "black", stat= "identity", position= "stack", width = 0.7) +
  scale_fill_manual(values = terrain5_palette) +
  labs(x ="Remaining forest 2020 (Mha)", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    panel.grid = element_blank() 
  )

indo_mill_ref_add_forest 

indo_mill_ref_add_prob <- ggplot(indo_forest_mill_ref_add, aes(y = factor(island, levels = c("Papua", "Kalimantan", "Sumatra"), labels = c("Papua", "Kalimantan", "Sumatra")), color = main_mill)) +
  geom_point(data = indo_forest_mill_ref_add, aes(x = mean_prob, color =main_mill ),  size = 2) +
  geom_errorbar(aes(xmin=mean_prob-sd_prob, xmax=mean_prob+sd_prob), width=.8,
               position=position_dodge(0.05))+
  geom_text(data = indo_forest_mill_ref_add, aes(x = mean_prob, label = round(mean_prob, 2), color = main_mill),  vjust = -0.4, hjust = 0.1,size = 3) +
  geom_hline(data = indo_forest_mill_ref_add, aes(yintercept = factor(island, levels = c("Papua", "Kalimantan", "Sumatra"), labels = c("Papua", "Kalimantan", "Sumatra"))), color = "grey", linetype = "solid", alpha = 0.2) +
  scale_color_manual(values = terrain5_palette) +
  labs(x = "Average of probability of conversion", y = "")+
  theme_minimal()+
  theme(legend.position = "none")+
  theme(
    panel.grid = element_blank(), 
  )

indo_mill_ref_add_prob
combine_fig_add <- ggarrange(indo_mill_ref_add_forest, indo_mill_ref_add_prob, ncol = 2)
annotate_figure(combine_fig_add, top = text_grob("Forest at risk is covered by low-quality and zero ZDCs by Mill Owners but covered by high-quality ZDCs by Buyers", 
                                             color = "black", size = 11))
ggsave("Figure_output/Figure_Remaining_forest_and_probability_of_conversion_2020_added_coverage_ZDCs.jpeg", dpi=300)
