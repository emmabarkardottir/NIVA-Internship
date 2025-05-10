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

# Ensure CRS is EPSG:3857
ices_shape <- st_transform(ices_shape, crs = 3857)
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

# Load world map
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

# Load fisheries data
species_data <- read_csv("sag_complete2.csv")
area <- read_csv("StocksPerArea2024.csv")
species_data_MEDBS <- read_csv("MEDandBSEA_stockdata.csv")

# Define bounding box for Europe
x_limits <- c(-4898058, 7625385)
y_limits <- c(3000000, 17500000)

# Convert spatial data columns to character format
ices_shape <- ices_shape %>% mutate(Area_Full = as.character(Area_Full))
gfcm_shape <- gfcm_shape %>% mutate(F_GSA_LIB = gsub("GSA", "", as.character(F_GSA_LIB)))
area <- area %>% mutate(ICES_Area = as.character(ICES_Area))
species_data_MEDBS <- species_data_MEDBS %>% mutate(Area = gsub("\\s", "", as.character(Area)))

# Clean-up multi-area mappings in MEDBS species data
gfcm_shape$F_GSA_LIB <- gsub("\\s", "", gfcm_shape$F_GSA_LIB)
species_data_MEDBS <- species_data_MEDBS %>%
  mutate(Area = strsplit(Area, "_")) %>%
  unnest(Area) %>%
  mutate(Area = as.character(Area))

# Handling area "11" (splitting it into "11.1" and "11.2")
species_data_MEDBS <- species_data_MEDBS %>%
  bind_rows(species_data_MEDBS %>% filter(Area == "11") %>% mutate(Area = "11.1")) %>%
  bind_rows(species_data_MEDBS %>% filter(Area == "11") %>% mutate(Area = "11.2")) %>%
  filter(Area != "11")

# Remove Area "18"
species_data_MEDBS <- species_data_MEDBS %>%
  filter(Area != "18")

# Filter only Cod stocks
cod_species_data <- species_data %>%
  filter(grepl("^cod\\.", StockKeyLabel)) %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = ifelse(length(unique(F.FMSy[!is.na(F.FMSy)])) > 1, rescale(F.FMSy, to = c(0, 1)), 0.5)) %>%
  ungroup()

cod_species_data_MEDBS <- species_data_MEDBS %>%
  filter(grepl("^cod\\.", StockKeyLabel)) %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = ifelse(length(unique(F.FMSy[!is.na(F.FMSy)])) > 1, rescale(F.FMSy, to = c(0, 1)), 0.5)) %>%
  ungroup()

# Compute average F/FMSy per area for cod stocks (ICES region)
avg_cod_per_area <- cod_species_data %>%
  filter(Year %in% tail(sort(unique(Year)), 6)) %>%
  inner_join(area, by = "StockKeyLabel", relationship = "many-to-many") %>%
  group_by(ICES_Area) %>%
  summarise(avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE)) %>%
  mutate(color_category = cut(avg_F_FMSy_norm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("blue", "green", "yellow", "orange", "red"), include.lowest = TRUE))

# Compute average F/FMSy per area for cod stocks (Mediterranean & Black Sea)
avg_cod_per_area_MEDBS <- cod_species_data_MEDBS %>%
  filter(Year %in% tail(sort(unique(Year)), 6)) %>%
  group_by(Area) %>%
  summarise(avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE)) %>%
  mutate(color_category = cut(avg_F_FMSy_norm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), labels = c("blue", "green", "yellow", "orange", "red"), include.lowest = TRUE))

# Merge cod data with spatial data
cod_map_data_ICES <- left_join(ices_shape, avg_cod_per_area, by = c("Area_Full" = "ICES_Area"))
cod_map_data_GFCM <- left_join(gfcm_shape, avg_cod_per_area_MEDBS, by = c("F_GSA_LIB" = "Area"))

# Plot map for ONLY COD STOCKS
cod_map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_sf(data = cod_map_data_ICES, aes(fill = color_category), color = "black", alpha = 0.6) +
  geom_sf(data = cod_map_data_GFCM, aes(fill = color_category), color = "black", alpha = 0.6) +
  scale_fill_manual(
    name = "GES",
    values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue"),
    labels = c("red" = "BAD", "orange" = "POOR", "yellow" = "MODERATE", "green" = "GOOD", "blue" = "HIGH")
  ) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  labs(title = "Cod Stocks MSY Status Across European Seas")

print(cod_map_plot)

output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, "cod_map.png"), 
  plot = cod_map_plot, width = 8, height = 6, dpi = 300)
