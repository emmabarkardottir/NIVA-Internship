# Setup
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

# Load spatial data
ices_shape <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")

# Set CRS
ices_shape <- st_transform(ices_shape, crs = 3857)
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

# Load world
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = 3857)

# Load data
species_data <- read_csv("sag_complete2.csv")
area <- read_csv("StocksPerArea2024.csv")
species_data_MEDBS <- read_csv("MEDandBSEA_stockdata.csv")

# Set plot bounds
x_limits <- c(-4898058, 7625385)
y_limits <- c(3000000, 17500000)

# Format data
ices_shape <- ices_shape %>% mutate(Area_Full = as.character(Area_Full))
area <- area %>% mutate(ICES_Area = as.character(ICES_Area))
gfcm_shape <- gfcm_shape %>% mutate(F_GSA_LIB = gsub("GSA", "", as.character(F_GSA_LIB)))
species_data_MEDBS <- species_data_MEDBS %>% mutate(Area = gsub("\\s", "", as.character(Area)))

# Split multi-areas
gfcm_shape$F_GSA_LIB <- gsub("\\s", "", gfcm_shape$F_GSA_LIB)
species_data_MEDBS <- species_data_MEDBS %>%
  mutate(Area = strsplit(Area, "_")) %>%
  unnest(Area) %>%
  mutate(Area = as.character(Area))

# Split Area 11
species_data_MEDBS <- species_data_MEDBS %>%
  bind_rows(filter(., Area == "11") %>% mutate(Area = "11.1")) %>%
  bind_rows(filter(., Area == "11") %>% mutate(Area = "11.2")) %>%
  filter(Area != "11", Area != "18")

# Normalize F/FMSy
species_data <- species_data %>%
  filter(!is.na(Year), !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy)) %>%
  ungroup()

species_data_MEDBS <- species_data_MEDBS %>%
  filter(!is.na(Year), !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy)) %>%
  ungroup()

# Use last 5 years
recent_years <- sort(unique(species_data$Year), decreasing = TRUE)[1:5]

# ICES averages
ices_stock_avg <- species_data %>%
  filter(Year %in% recent_years) %>%
  group_by(StockKeyLabel) %>%
  summarise(avg_F_FMSy = mean(F.FMSy_norm, na.rm = TRUE)) %>%
  ungroup()

ices_stock_area <- inner_join(ices_stock_avg, area, by = "StockKeyLabel")

worst_per_area_ICES <- ices_stock_area %>%
  group_by(ICES_Area) %>%
  slice_max(avg_F_FMSy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(color_category = cut(avg_F_FMSy, 
                              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                              labels = c("HIGH", "GOOD", "MODERATE", "POOR", "BAD"),
                              include.lowest = TRUE)) %>%
  mutate(color_category = factor(color_category, levels = c("HIGH", "GOOD", "MODERATE", "POOR", "BAD")))

# MEDBS averages
medbs_stock_avg <- species_data_MEDBS %>%
  filter(Year %in% recent_years) %>%
  group_by(StockKeyLabel, Area) %>%
  summarise(avg_F_FMSy = mean(F.FMSy_norm, na.rm = TRUE)) %>%
  ungroup()

worst_per_area_MEDBS <- medbs_stock_avg %>%
  group_by(Area) %>%
  slice_max(avg_F_FMSy, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(color_category = cut(avg_F_FMSy,
                              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                              labels = c("HIGH", "GOOD", "MODERATE", "POOR", "BAD"),
                              include.lowest = TRUE)) %>%
  mutate(color_category = factor(color_category, levels = c("HIGH", "GOOD", "MODERATE", "POOR", "BAD")))

# Join to spatial shapes
map_data_ICES <- left_join(ices_shape, worst_per_area_ICES, by = c("Area_Full" = "ICES_Area"))
map_data_GFCM <- left_join(gfcm_shape, worst_per_area_MEDBS, by = c("F_GSA_LIB" = "Area"))

# Plot
map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_sf(data = map_data_ICES, aes(fill = color_category), color = "black", alpha = 0.6) +
  geom_sf(data = map_data_GFCM, aes(fill = color_category), color = "black", alpha = 0.6) +
  scale_fill_manual(
    name = "GES",
    values = c(
      "HIGH" = "blue",
      "GOOD" = "green",
      "MODERATE" = "yellow",
      "POOR" = "orange",
      "BAD" = "red"
    ),
    drop = FALSE
  ) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(title = "'One Out, All Out' Scenario Across European Seas")

print(map_plot)

# Save
output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, "oneout_allout.png"), plot = map_plot, width = 8, height = 6, dpi = 300)
