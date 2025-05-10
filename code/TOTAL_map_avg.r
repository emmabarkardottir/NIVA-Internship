# Load libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(scales)
library(tidyr)

# Set working directory
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")

# Load shapefiles
ices_shape <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")

# Transform CRS
ices_shape <- st_transform(ices_shape, crs = 3857)
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)
world <- st_transform(ne_countries(scale = "medium", returnclass = "sf"), crs = 3857)

# Read data
species_data <- read_csv("sag_complete2.csv")
area <- read_csv("StocksPerArea2024.csv")
species_data_MEDBS <- read_csv("MEDandBSEA_stockdata.csv")

# Define map limits
x_limits <- c(-4898058, 7625385)
y_limits <- c(3000000, 17500000)

# Clean area columns
ices_shape <- ices_shape %>% mutate(Area_Full = as.character(Area_Full))
area <- area %>% mutate(ICES_Area = as.character(ICES_Area))
gfcm_shape <- gfcm_shape %>%
  mutate(F_GSA_LIB = gsub("GSA", "", as.character(F_GSA_LIB)),
         F_GSA_LIB = gsub("\\s+", "", F_GSA_LIB))

# Clean and expand MEDBS area column
species_data_MEDBS <- species_data_MEDBS %>%
  mutate(Area = gsub("\\s+", "", as.character(Area)),
         Area = strsplit(Area, "_")) %>%
  unnest(Area)

# Split GSA 11 into 11.1 and 11.2, and remove 11 and 18
species_data_MEDBS <- species_data_MEDBS %>%
  bind_rows(
    species_data_MEDBS %>% filter(Area == "11") %>% mutate(Area = "11.1"),
    species_data_MEDBS %>% filter(Area == "11") %>% mutate(Area = "11.2")
  ) %>%
  filter(Area != "11", Area != "18")

# Normalize F/FMSy
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

# ICES: Average over last 5 years with weighted mean
ices_recent <- species_data %>%
  filter(Year %in% sort(unique(Year), decreasing = TRUE)[1:5])

avg_per_stock_ICES <- ices_recent %>%
  group_by(StockKeyLabel) %>%
  summarise(
    stock_avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

avg_per_area <- avg_per_stock_ICES %>%
  inner_join(area, by = "StockKeyLabel") %>%
  group_by(ICES_Area) %>%
  summarise(
    avg_F_FMSy_norm = weighted.mean(stock_avg_F_FMSy_norm, w = n_years, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(color_category = cut(avg_F_FMSy_norm,
                              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                              labels = c("blue", "green", "yellow", "orange", "red"),
                              include.lowest = TRUE))

# MEDBS: Average over last 5 years with weighted mean
medbs_recent <- species_data_MEDBS %>%
  filter(Year %in% sort(unique(Year), decreasing = TRUE)[1:5])

avg_per_stock_MEDBS <- medbs_recent %>%
  group_by(StockKeyLabel, Area) %>%
  summarise(
    stock_avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE),
    n_years = n(),
    .groups = "drop"
  )

avg_per_area_MEDBS <- avg_per_stock_MEDBS %>%
  group_by(Area) %>%
  summarise(
    avg_F_FMSy_norm = weighted.mean(stock_avg_F_FMSy_norm, w = n_years, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Area = gsub("\\s+", "", Area)) %>%
  mutate(color_category = cut(avg_F_FMSy_norm,
                              breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                              labels = c("blue", "green", "yellow", "orange", "red"),
                              include.lowest = TRUE))

# Debug: Check unmatched GFCM areas
missing_gfcms <- setdiff(avg_per_area_MEDBS$Area, gfcm_shape$F_GSA_LIB)
if (length(missing_gfcms) > 0) {
  warning("Unmatched GFCM Areas: ", paste(missing_gfcms, collapse = ", "))
}

# Merge spatial data
map_data_ICES <- left_join(ices_shape, avg_per_area, by = c("Area_Full" = "ICES_Area"))
map_data_GFCM <- left_join(gfcm_shape, avg_per_area_MEDBS, by = c("F_GSA_LIB" = "Area"))

# Plot combined map
map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +
  geom_sf(data = map_data_ICES %>% filter(!is.na(color_category)), aes(fill = color_category), color = "black", alpha = 0.6) +
  geom_sf(data = map_data_GFCM %>% filter(!is.na(color_category)), aes(fill = color_category), color = "black", alpha = 0.6) +
 scale_fill_manual(
  name = "GES",
  values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue"),
  labels = c("HIGH", "GOOD", "MODERATE", "POOR", "BAD")
) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  labs(title = "Overall Averaged Fisheries Status for European Seas")

print(map_plot)

# Save plot
output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, "baltic_map_avg.png"),
  plot = map_plot, width = 8, height = 6, dpi = 300
)
