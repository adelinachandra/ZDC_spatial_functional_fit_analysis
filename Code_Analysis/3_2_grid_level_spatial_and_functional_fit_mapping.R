# Fri Apr 28 09:07:30 2022 ------------------------------
## ---------------------------
## Title: "Generating Maps"
## Author: "Adelina Chandra"
## Date: 2023-04-28
##  This step aims to create maps including:
##  a) ZDC coverage, b) Spatial fit, c) Functional fit (w/o inset)
##  by Mill Owners and Buyers 
##  showing the results as presented in the manuscript.
##  note: currently the tmap_save function is commented out. Please uncomment it if needed.
## ---------------------------

# The main input data is a grid-level spatial data, covering both inside and outside mill sheds.
# Please note, that the data consists only grid cells located inside mill sheds for each variables.
# Details of the data (unit for area is hectare, a pixel area is 400 ha)
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

# The administrative boundary is a subset of data from BPS, 2020. 
# This data can be downloaded via https://data.humdata.org/dataset/cod-ab-idn? 
# Please note that the data presents provinces as they were when it was published.

# The output of this script are 1) the maps presented in the paper, including: 
# a) ZDC coverage, b) Spatial fit, c) Functional fit (w/o inset) by Mill Owners and Buyers 
# 2) Grid-level spatial and functional fit output data

# This code uses 'tmap' package v4 (https://github.com/r-tmap/tmap)
# library(remotes)
# remotes::install_github('r-tmap/tmap') #installing tmap v4 
library(tmap)
library(sf)
library(viridis)
library(ggplot2)
library(tidyverse)

#---- Set up directory -----------------------------------------------------------------------------------------------------------------
# Specify your working directory
dir_path <- "path/to/your/working/directory"

# Specify the folder names
folders <- c("Figure_output", "Data_output")

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
terrain5_palette <- c("#440154FF","#21908CFF","#FDE725FF") #based on viridis_pal(option = "viridis")(3)

#---- Read data ----------------------------------------------------------------------------------------------------------------- 
# Adm boundaries 
# Specify the path to the downloaded administrative boundary data (the data can be downloaded here https://data.humdata.org/dataset/cod-ab-idn?)
# Unzip the data before import
adm_indo <- st_read("specify/your/data/path/idn_admbnda_adm1_bps_20200401.shp")  #calling the shapefile: idn_admbnda_adm1_bps_20200401.shp

# Select provinces that are located in Sumatra, Kalimantan and Papua
indo_select <- adm_indo %>% 
  filter(str_detect(ADM1_PCODE, "ID1|ID7|ID6|ID9")) %>% 
  mutate(Island = case_when(str_detect(ADM1_PCODE, "ID1") ~ "Sumatra",
                            str_detect(ADM1_PCODE, "ID6") ~ "Kalimantan",
                            str_detect(ADM1_PCODE, "ID7") ~ "Sulawesi",
                            str_detect(ADM1_PCODE, "ID9") ~ "Papua")) %>% 
  select(ADM1_EN, ADM1_PCODE, Island)
  
# Project the admin data for consistency
indo_project <- st_transform(indo_select, 3395)

# Select provinces that are located in Sulawesi Island
sula <- indo_project %>% filter(Island == "Sulawesi")

# Grid-level data - only inside palm oil mill sheds 
all_map <- st_read("Data_input/Chandraetal2024_indo_all_variables_grid_inside_millshed.geojson")

#---- Prepare spatial data -----------------------------------------------------------------------------------------------------------------
all_island <- indo_project %>%
  mutate(adm_sort = case_when(Island== "Sumatra" ~1,
                              Island== "Kalimantan" ~2,
                              Island== "Papua" ~3,
                              Island== "Sulawesi" ~4))

#---- ZDC coverage visualization  -----------------------------------------------------------------------------------------------------------------
# ZDC coverage by mill owners
inside_zdc_mill <- all_map %>% 
  filter(main_mill_coverage != "outside mill shed") %>% 
  mutate(main_mill_label = case_when(main_mill_coverage == "zero coverage" ~ "Zero",
                                     main_mill_coverage == "low coverage" ~ "Low-quality",
                                     main_mill_coverage == "high coverage" ~ "High-quality"))

inside_zdc_mill$main_mill_label <- factor(inside_zdc_mill$main_mill_label, levels = c("Zero", "Low-quality", "High-quality"))

all_cov_mill <- tm_shape(all_island)+
  tm_polygons(fill = "white", col = NA) +
  tm_shape(inside_zdc_mill)+
  tm_polygons(fill="main_mill_label",fill.scale = tm_scale_categorical(values = terrain5_palette, value.na = "white"), 
              col_alpha=0, fill.legend = tm_legend(title = "ZDC coverage by Mill Owners",
                                                   position = tm_pos_in("right", "top"))) +
  tm_shape(all_island) + tm_polygons(fill = NA) +
  tm_shape(sula) + tm_polygons(fill = "white", col = NA)
all_cov_mill
#tmap_save(all_cov_mill, "Figure_output/Map_ZDC_coverage_by_Mill_Owners.png") #uncomment to save the map

# ZDC coverage by buyers
inside_zdc_ref <- all_map %>% 
  filter(main_ref_coverage != "outside mill shed") %>% 
  mutate(main_ref_label = case_when(main_ref_coverage == "zero coverage" ~ "Zero",
                                    main_ref_coverage == "low coverage" ~ "Low-quality",
                                    main_ref_coverage == "high coverage" ~ "High-quality"))

inside_zdc_ref$main_ref_label <- factor(inside_zdc_ref$main_ref_label, levels = c("Zero", "Low-quality", "High-quality"))

all_cov_ref <- tm_shape(all_island)+
  tm_polygons(fill = "white", col = NA) +
  tm_shape(inside_zdc_ref)+
  tm_polygons(fill="main_ref_label",fill.scale = tm_scale_categorical(values = terrain5_palette, value.na = "white"), 
              col_alpha=0, fill.legend = tm_legend(title = "ZDC coverage by Buyers",
                                                   position = tm_pos_in("right", "top"))) +
  tm_shape(all_island) + tm_polygons(fill = NA) +
  tm_shape(sula) + tm_polygons(fill = "white", col = NA)
all_cov_ref
#tmap_save(all_cov_ref, "Figure_output/Map_ZDC_coverage_by_Buyers.png") #uncomment to save the map

#---- Data preparation for spatial fit -----------------------------------------------------------------------------------------------------------------
covered_data_mill_all <- all_map %>%
  filter(main_mill_coverage != "outside mill shed") %>% 
  mutate(spatial_fit = case_when(for20 > 0.01 & main_mill_coverage == "zero coverage" ~ "Forest at risk not covered by ZDCs",
                                 for20 > 0.01 & main_mill_coverage == "low coverage" ~ "Forest at risk covered by low-quality ZDCs",
                                 for20 > 0.01 & main_mill_coverage == "high coverage" ~ "Forest at risk covered by high-quality ZDCs",
                                 for20 <=0.01 ~ "Non-forest"))

covered_data_mill <- covered_data_mill_all %>% 
  filter(spatial_fit != "Non-forest")

covered_data_mill$spatial_fit <- factor(covered_data_mill$spatial_fit, levels = c("Forest at risk not covered by ZDCs", "Forest at risk covered by low-quality ZDCs", "Forest at risk covered by high-quality ZDCs"))
levels(covered_data_mill$spatial_fit)

######ref
covered_data_ref_all <- all_map %>%
  filter(main_ref_coverage != "outside ref shed") %>% 
  mutate(spatial_fit = case_when(for20 > 0.01 & main_ref_coverage == "zero coverage" ~ "Forest at risk not covered by ZDCs",
                          for20 > 0.01 & main_ref_coverage == "low coverage" ~ "Forest at risk covered by low-quality ZDCs",
                          for20 > 0.01 & main_ref_coverage == "high coverage" ~ "Forest at risk covered by high-quality ZDCs",
                          for20 <=0.01 ~ "Non-forest"))

covered_data_ref <- covered_data_ref_all %>% 
  filter(spatial_fit != "Non-forest")

covered_data_ref$spatial_fit <- factor(covered_data_ref$spatial_fit, levels = c("Forest at risk not covered by ZDCs", "Forest at risk covered by low-quality ZDCs", "Forest at risk covered by high-quality ZDCs"))
levels(covered_data_ref$spatial_fit)

#---- Visualization of spatial fit -----------------------------------------------------------------------------------------------------------------
# Spatial fit by mill owners 
all_zdc_mill <- tm_shape(all_island)+
  tm_polygons(fill = "white", col = NA) +
  tm_shape(covered_data_mill)+
  tm_polygons(fill="spatial_fit",fill.scale = tm_scale_categorical(values = terrain5_palette, value.na = "white"), 
              col_alpha=0, fill.legend = tm_legend(title = "Spatial fit of ZDCs by Mill Owners",
                                                   position = tm_pos_in("right", "top"))) +
  tm_shape(all_island) + tm_polygons(fill = NA)+
  tm_shape(sula) + tm_polygons(fill = "white", col = NA)
all_zdc_mill
#tmap_save(all_zdc_mill, "Figure_output/Map_Spatial_fit_within_mill_shed_by_Mill_Owners_ZDCs.png") #uncomment to save the map

# Spatial fit by buyers
all_zdc_ref <- tm_shape(all_island)+
  tm_polygons(fill = "white", col = NA) +
  tm_shape(covered_data_ref)+
  tm_polygons(fill="spatial_fit",fill.scale = tm_scale_categorical(values = terrain5_palette, value.na = "white"), 
              col_alpha=0, fill.legend = tm_legend(title = "Spatial fit of ZDCs by Buyers",
                                                   position = tm_pos_in("right", "top"))) +
  tm_shape(all_island) + tm_polygons(fill = NA) +
  tm_shape(sula) + tm_polygons(fill = "white", col = NA)
all_zdc_ref
#tmap_save(all_zdc_ref, "Figure_output/Map_Spatial_fit_within_mill_shed_by_Buyers_ZDCs.png") #uncomment to save the map

#---- Data preparation for functional fit -----------------------------------------------------------------------------------------------------------------
all_map_smh <- all_map %>% 
  select(ID, smh_palm, smh_inc_mill_coverage, smh_inc_ref_coverage,island) %>% 
  filter(!is.na(smh_inc_mill_coverage)) %>% 
  mutate(fun_fit_smh_mill = case_when(smh_palm == 0 ~ "Non-smallholder area",
                                      smh_palm > 0 & smh_inc_mill_coverage == "low coverage" ~ "Smallholder area in low-quality ZDCs",
                                      smh_palm > 0 & smh_inc_mill_coverage == "high coverage" ~ "Smallholder area in high-quality ZDCs",
                                      smh_palm > 0 & smh_inc_mill_coverage == "zero coverage" ~ "Smallholder area not covered by ZDCs"),
         fun_fit_smh_ref = case_when(smh_palm == 0 ~ "Non-smallholder area",
                                     smh_palm > 0 & smh_inc_ref_coverage == "low coverage" ~ "Smallholder area in low-quality ZDCs",
                                     smh_palm > 0 & smh_inc_ref_coverage == "high coverage" ~ "Smallholder area in high-quality ZDCs",
                                     smh_palm > 0 & smh_inc_ref_coverage == "zero coverage" ~ "Smallholder area not covered by ZDCs"))


all_map_smh_mill_include <- all_map_smh %>% filter(fun_fit_smh_mill != "Non-smallholder area")
all_map_smh_ref_include <- all_map_smh %>% filter(fun_fit_smh_ref != "Non-smallholder area")

all_map_smh_mill_include$fun_fit_smh_mill <- factor(all_map_smh_mill_include$fun_fit_smh_mill, levels = c("Smallholder area not covered by ZDCs","Smallholder area in low-quality ZDCs","Smallholder area in high-quality ZDCs"))
all_map_smh_ref_include$fun_fit_smh_ref <- factor(all_map_smh_ref_include$fun_fit_smh_ref, levels = c("Smallholder area not covered by ZDCs","Smallholder area in low-quality ZDCs","Smallholder area in high-quality ZDCs"))

#---- Visualization of functional fit -----------------------------------------------------------------------------------------------------------------
# Functional fit by Mill Owners, without inset
all_smh_mill <- tm_shape(all_island)+
  tm_polygons(fill = "white", col = NA) +
  tm_shape(all_map_smh_mill_include)+
  tm_polygons(fill="fun_fit_smh_mill",fill.scale = tm_scale_categorical(values = terrain5_palette, value.na = "white"), 
              col_alpha=0, fill.legend = tm_legend(title = "Functional fit of ZDCs by Mill Owners",
                                                   position = tm_pos_in("right", "top"))) +
  tm_shape(all_island) + tm_polygons(fill = NA)+
  tm_shape(sula) + tm_polygons(fill = "white", col = NA)
all_smh_mill
#tmap_save(all_smh_mill, "Figure_output/Map_Functional_fit_within_mill_shed_by_Mill_Owners_ZDCs.png") #uncomment to save the map

# Functional fit by Buyers
all_smh_ref <- tm_shape(all_island)+
  tm_polygons(fill = "white", col = NA) +
  tm_shape(all_map_smh_ref_include)+
  tm_polygons(fill="fun_fit_smh_ref",fill.scale = tm_scale_categorical(values = terrain5_palette, value.na = "white"), 
              col_alpha=0, fill.legend = tm_legend(title = "Functional fit of ZDCs by Buyers",
                                                   position = tm_pos_in("right", "top"))) +
  tm_shape(all_island) + tm_polygons(fill = NA)+
  tm_shape(sula) + tm_polygons(fill = "white", col = NA)
all_smh_ref
#tmap_save(all_smh_ref, "Figure_output/Map_Functional_fit_within_mill_shed_by_Buyers_ZDCs.png") #uncomment to save the map

#---- Prepare output data  -----------------------------------------------------------------------------------------------------------------
output_data <- all_map %>% 
  filter(if_all(contains("coverage"), ~!is.na(.))) %>% #filter outside coverage
  select(ID) %>%
  left_join(covered_data_mill_all %>% as_tibble() %>% rename("spatial_fit_by_mill_owners" = "spatial_fit")) %>% 
  left_join(covered_data_ref_all %>% as_tibble() %>% select(ID, spatial_fit, geometry) %>% rename("spatial_fit_by_buyers" = "spatial_fit")) %>% 
  left_join(all_map_smh %>% as_tibble() %>% select(ID, contains('fun_fit'), geometry) %>% rename("functional_fit_by_mill_owners" = "fun_fit_smh_mill", "functional_fit_by_buyers" = "fun_fit_smh_ref")) %>% 
  select(ID, island, area_ha, contains("coverage"), contains("_fit_"))

# Check, validity
all(st_is_valid(output_data))
all(!st_is_empty(output_data))

#---- Export data -----------------------------------------------------------------------------------------------------------------  
# Grid_level spatial and functional fit output 
st_write(output_data, "Chandraetal2024_grid_level_spatial_functional_fit.geojson", delete_dsn = TRUE)
