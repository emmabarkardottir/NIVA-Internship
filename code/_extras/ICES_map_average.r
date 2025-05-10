setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(scales)

ices_shape <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

# Ensure CRS is EPSG:3857
ices_shape <- st_transform(ices_shape, crs = 3857)

# Load background world map and transform it
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

species_data <- read_csv("sag_complete2.csv")
area <- read_csv("StocksPerArea2024.csv")

# Convert columns to character format
ices_shape <- ices_shape %>% mutate(Area_Full = as.character(Area_Full))
area <- area %>% mutate(ICES_Area = as.character(ICES_Area))

# Function to categorize F/FMSy_norm into color categories
color_categories <- function(F_FMSy_norm_value) {
  if (F_FMSy_norm_value >= 0.8) {
    return("red")
  } else if (F_FMSy_norm_value >= 0.6) {
    return("orange")
  } else if (F_FMSy_norm_value >= 0.4) {
    return("yellow")
  } else if (F_FMSy_norm_value >= 0.2) {
    return("green")
  } else {
    return("blue")
  }
}

# Define bounding box in EPSG:3857 (Europe focus)
x_limits <- c(-4898058, 7625385)
y_limits <- c(4000621, 17500000)


# Normalize F/FMSy values for all species in the dataset
species_data <- species_data %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1))) %>%
  ungroup()

# Calculate the average F/FMSy_norm per area across all stocks over 5 years 
avg_per_stock_ICES <- species_data %>%
  filter(Year %in% tail(sort(unique(Year)), 5)) %>%
  group_by(StockKeyLabel) %>%
  summarise(stock_avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE), .groups = "drop")

avg_per_area <- avg_per_stock_ICES %>%
  inner_join(area, by = "StockKeyLabel") %>%
  group_by(ICES_Area) %>%
  summarise(avg_F_FMSy_norm = mean(stock_avg_F_FMSy_norm, na.rm = TRUE), .groups = "drop") %>%
  mutate(color_category = cut(avg_F_FMSy_norm, breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1), 
                              labels = c("blue", "green", "yellow", "orange", "red"), include.lowest = TRUE))

# Merge with the ICES shape data
map_data <- left_join(ices_shape, avg_per_area, by = c("Area_Full" = "ICES_Area"))

# Ensure color_category is a factor
map_data$color_category <- factor(map_data$color_category, levels = c("red", "orange", "yellow", "green", "blue", "gray"))

# Plot the single averaged map
map_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +  
  geom_sf(data = map_data, aes(fill = color_category), color = "black", alpha = 0.6) +  
  scale_fill_manual(
    name = "GES Status",
    values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", "green" = "green", "blue" = "blue", "gray" = "gray"),
    labels = c(
      "red" = "BAD",
      "orange" = "POOR",
      "yellow" = "MODERATE",
      "green" = "GOOD",
      "blue" = "HIGH",
      "gray" = "NO DATA"
    )
  ) +  
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +  
  theme_minimal() +
  labs(title = "Average Normalized F/FMSy Across Stocks in the Baltics Sea Over the Last 5 Years")

print(map_plot)
