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

# Load GFCM shapefile and world map
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

# Define bounding box for the Mediterranean and Black Sea
x_limits <- c(-2500000, 5000000)
y_limits <- c(3000000, 7000000)

# Load and clean species data
species_data_MEDBS <- read_csv("MEDandBSEA_stockdata.csv")
species_data_MEDBS <- species_data_MEDBS %>% 
  mutate(Area = gsub("\\s", "", as.character(Area))) # Remove spaces

gfcm_shape <- gfcm_shape %>% 
  mutate(F_GSA_LIB = gsub("GSA", "", F_GSA_LIB),
         F_GSA_LIB = gsub("\\s", "", F_GSA_LIB))

# Expand multi-area rows
expanded_species_data_MEDBS <- species_data_MEDBS %>% 
  mutate(Area = strsplit(Area, "_")) %>% 
  unnest(Area) %>% 
  mutate(Area = as.character(Area))

# Handle special cases for area 11 (split into 11.1 and 11.2)
expanded_species_data_MEDBS <- expanded_species_data_MEDBS %>%
  bind_rows(
    expanded_species_data_MEDBS %>% filter(Area == "11") %>% mutate(Area = "11.1"),
    expanded_species_data_MEDBS %>% filter(Area == "11") %>% mutate(Area = "11.2")
  ) %>%
  filter(Area != "11", Area != "18") # Remove original 11 and unwanted 18

# Normalize F/FMSy values per stock
normalized_species <- expanded_species_data_MEDBS %>%
  filter(!is.na(Year), !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1))) %>%
  ungroup()

# Average F/FMSy per stock for last 5 years
avg_per_stock <- normalized_species %>%
  filter(Year %in% tail(sort(unique(Year)), 5)) %>%
  group_by(StockKeyLabel, Area) %>%
  summarise(stock_avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE), .groups = "drop")

# Average F/FMSy per area
avg_per_area <- avg_per_stock %>%
  group_by(Area) %>%
  summarise(avg_F_FMSy_norm = mean(stock_avg_F_FMSy_norm, na.rm = TRUE), .groups = "drop") %>%
  mutate(color_category = cut(avg_F_FMSy_norm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("blue", "green", "yellow", "orange", "red"), include.lowest = TRUE))

# Merge data with spatial layer
map_data <- left_join(gfcm_shape, avg_per_area, by = c("F_GSA_LIB" = "Area"))

# Plot
map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_sf(data = map_data, aes(fill = color_category), color = "black", alpha = 0.6) +
  scale_fill_manual(
    name = "GES Status",
    values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue"),
    labels = c("red" = "BAD", "orange" = "POOR", "yellow" = "MODERATE", "green" = "GOOD", "blue" = "HIGH")
  ) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  labs(title = "Fisheries Status in the Mediterranean and Black Sea")

print(map_plot)
