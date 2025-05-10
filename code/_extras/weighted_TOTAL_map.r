setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(scales)
library(tidyr)

ices_shape <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")

# Ensure CRS is EPSG:3857
ices_shape <- st_transform(ices_shape, crs = 3857)
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

species_data <- read_csv("sag_complete2.csv")
area <- read_csv("StocksPerArea2024.csv")
species_data_MEDBS <- read_csv("MEDandBSEA_stockdata.csv")

# Define bounding box for Europe
x_limits <- c(-4898058, 7625385)
y_limits <- c(3000000, 17500000)

# Convert columns to character format
ices_shape <- ices_shape %>% mutate(Area_Full = as.character(Area_Full))
area <- area %>% mutate(ICES_Area = as.character(ICES_Area))
gfcm_shape <- gfcm_shape %>% mutate(F_GSA_LIB = gsub("GSA", "", as.character(F_GSA_LIB)))
species_data_MEDBS <- species_data_MEDBS %>% mutate(Area = gsub("\\s", "", as.character(Area)))

# Clean-up MEDBS species data:
gfcm_shape$F_GSA_LIB <- gsub("\\s", "", gfcm_shape$F_GSA_LIB)

species_data_MEDBS <- species_data_MEDBS %>% 
  mutate(Area = strsplit(Area, "_")) %>% 
  unnest(Area) %>% 
  mutate(Area = as.character(Area))

# Split "11" into "11.1" and "11.2"
species_data_MEDBS <- species_data_MEDBS %>%
  bind_rows(
    species_data_MEDBS %>% 
      filter(Area == "11") %>%
      mutate(Area = "11.1") 
  ) %>%
  bind_rows(
    species_data_MEDBS %>% 
      filter(Area == "11") %>%
      mutate(Area = "11.2")
  ) %>%
  filter(Area != "11")

# Remove all rows where Area is "18"
species_data_MEDBS <- species_data_MEDBS %>%
  filter(Area != "18")

# Ensure total catch column exists
species_data <- species_data %>%
  filter(!is.na(Year) & !is.na(F.FMSy) & !is.na(Catches))

species_data_MEDBS <- species_data_MEDBS %>%
  filter(!is.na(Year) & !is.na(F.FMSy) & !is.na(Catches))

# Normalize F/FMSy values using weighted method
species_data <- species_data %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1))) %>%
  ungroup()

species_data_MEDBS <- species_data_MEDBS %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1))) %>%
  ungroup()

# Compute weighted average F/FMSy_norm per area
avg_per_area_weighted <- species_data %>%
  filter(Year %in% tail(sort(unique(Year)), 6)) %>%
  inner_join(area, by = "StockKeyLabel", relationship = "many-to-many") %>%
  group_by(ICES_Area) %>%
  summarise(avg_F_FMSy_norm = weighted.mean(F.FMSy_norm, Catches, na.rm = TRUE)) %>%
  mutate(color_category = cut(avg_F_FMSy_norm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                              labels = c("blue", "green", "yellow", "orange", "red"), 
                              include.lowest = TRUE))

avg_per_area_MEDBS_weighted <- species_data_MEDBS %>%
  filter(Year %in% tail(sort(unique(Year)), 6)) %>%
  group_by(Area) %>%
  summarise(avg_F_FMSy_norm = weighted.mean(F.FMSy_norm, Catches, na.rm = TRUE)) %>%
  mutate(color_category = cut(avg_F_FMSy_norm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                              labels = c("blue", "green", "yellow", "orange", "red"), 
                              include.lowest = TRUE))

# Merge with spatial data
map_data_ICES_weighted <- left_join(ices_shape, avg_per_area_weighted, by = c("Area_Full" = "ICES_Area"))
map_data_GFCM_weighted <- left_join(gfcm_shape, avg_per_area_MEDBS_weighted, by = c("F_GSA_LIB" = "Area"))

# Plot weighted scenario map
map_plot_weighted <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_sf(data = map_data_ICES_weighted, aes(fill = color_category), color = "black", alpha = 0.6) +
  geom_sf(data = map_data_GFCM_weighted, aes(fill = color_category), color = "black", alpha = 0.6) +
  scale_fill_manual(
    name = "Weighted GES Status",
    values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue"),
    labels = c("red" = "BAD", "orange" = "POOR", "yellow" = "MODERATE", "green" = "GOOD", "blue" = "HIGH")
  ) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  labs(title = "Weighted Fisheries Status Across European Seas, Mediterranean, and Black Sea")

print(map_plot_weighted)
