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

# Define ecoregion mapping
ecoregions <- c(
  "1" = "Widely", 
  "2" = "BoBiscay & Iberia", 
  "3" = "Greater North Sea", 
  "4" = "Arctic Ocean", 
  "5" = "Iceland", 
  "6" = "Baltic Sea", 
  "7" = "Celtic Seas", 
  "8" = "Azores"
)

# Choose the ecoregion of interest:
# ========================================= Change here
selected_ecoregion_id <- 6
ecoregion_selected <- ecoregions[as.character(selected_ecoregion_id)]

# Function to categorize F.FMSy_norm into color categories
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

# Get the list of stock labels within the selected ecoregion
stock_labels <- species_data %>%
  filter(Ecoregion == ecoregion_selected) %>%
  filter(!is.na(F.FMSy)) %>%  
  distinct(StockKeyLabel) %>%
  pull(StockKeyLabel)

# Loop through stock labels and generate plots
for (stock in stock_labels) {
  species <- species_data %>% 
    filter(StockKeyLabel == stock) %>%
    filter(!is.na(Year) & !is.na(F.FMSy))  

  species <- species %>%
    mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)))
  
  # Calculate the average F.FMSy for those last 5 years
  last_5_years <- species %>%
    filter(Year %in% tail(sort(unique(Year)), 5))

  average_F_FMSy_norm <- last_5_years %>%
    summarise(avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE))
  
  avg_value <- as.numeric(average_F_FMSy_norm$avg_F_FMSy_norm)
  if (is.na(avg_value)) {
    next
  }
  
  # Get the color for the average F.FMSy_norm value
  category_color <- color_categories(avg_value)
  
  # Filter the data for the stock label
  area_filtered <- area %>% filter(StockKeyLabel == stock)
  
  # Merge the filtered stock data with ICES areas
  map_data <- left_join(ices_shape, area_filtered, by = c("Area_Full" = "ICES_Area"))
  
  # Assign color to the stock area based on the calculated category
  map_data <- map_data %>%
    mutate(color_category = ifelse(!is.na(StockKeyLabel) & StockKeyLabel == stock, category_color, "gray"))
  
  # Ensure color_category is treated as a factor
  map_data$color_category <- factor(map_data$color_category, levels = c("red", "orange", "yellow", "green", "blue", "gray"))
  
  # Plot the ICES statistical rectangles with the filtered stock data
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
    labs(title = paste("Normalized Stock Distribution for", stock, "in", ecoregion_selected, "Over the Last 5 Years"))
  
  print(map_plot)
}
