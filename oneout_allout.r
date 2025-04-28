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
# Clean and expand multi-area mappings in species_data
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
  # Remove original "11" rows
  filter(Area != "11")

# Remove all rows where Area is "18"
species_data_MEDBS <- species_data_MEDBS %>%
  filter(Area != "18")

# Normalize F/FMSy values for ICES and GFCM datasets
species_data <- species_data %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1))) %>%
  ungroup()

species_data_MEDBS <- species_data_MEDBS %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1))) %>%
  ungroup()

# Get most recent 5 years
recent_years <- sort(unique(species_data$Year), decreasing = TRUE)[1:5]

# === ICES === #
# Calculate 5-year average F/FMSy per stock
ices_stock_avg <- species_data %>%
  filter(Year %in% recent_years) %>%
  group_by(StockKeyLabel) %>%
  summarise(avg_F_FMSy = mean(F.FMSy_norm, na.rm = TRUE)) %>%
  ungroup()

# Join stock to area
ices_stock_area <- ices_stock_avg %>%
  inner_join(area, by = "StockKeyLabel")

# Get the worst (highest avg) stock per area
worst_per_area_ICES <- ices_stock_area %>%
  group_by(ICES_Area) %>%
  slice_max(order_by = avg_F_FMSy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(color_category = cut(avg_F_FMSy, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("blue", "green", "yellow", "orange", "red"), include.lowest = TRUE))

# === MEDBS (GFCM) === #
medbs_stock_avg <- species_data_MEDBS %>%
  filter(Year %in% recent_years) %>%
  group_by(StockKeyLabel, Area) %>%  # Area is already in species_data_MEDBS
  summarise(avg_F_FMSy = mean(F.FMSy_norm, na.rm = TRUE)) %>%
  ungroup()

worst_per_area_MEDBS <- medbs_stock_avg %>%
  group_by(Area) %>%
  slice_max(order_by = avg_F_FMSy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(color_category = cut(avg_F_FMSy, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("blue", "green", "yellow", "orange", "red"), include.lowest = TRUE))

# Merge with spatial data
map_data_ICES <- left_join(ices_shape, worst_per_area_ICES, by = c("Area_Full" = "ICES_Area"))
map_data_GFCM <- left_join(gfcm_shape, worst_per_area_MEDBS, by = c("F_GSA_LIB" = "Area"))

# Plot the map
map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_sf(data = map_data_ICES, aes(fill = color_category), color = "black", alpha = 0.6) +
  geom_sf(data = map_data_GFCM, aes(fill = color_category), color = "black", alpha = 0.6) +
  scale_fill_manual(
    name = "GES Status (Worst Stock per Area)",
    values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue"),
    labels = c("red" = "BAD", "orange" = "POOR", "yellow" = "MODERATE", "green" = "GOOD", "blue" = "HIGH")
  ) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  labs(title = "Worst GES Status by Area (Highest F/FMSy Normalized Value)")

print(map_plot)
