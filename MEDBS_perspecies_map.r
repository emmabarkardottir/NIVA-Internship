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

# Read in the spatial shape data (GFMC)
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")

# Ensure CRS is EPSG:3857
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

# Load background world map and transform it
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

# Load species data (Med & Black Sea)
species_data_MEDBS <- read_csv("MEDandBSEA_stockdata.csv")

# Convert columns to character format
gfcm_shape <- gfcm_shape %>% mutate(F_GSA_LIB = as.character(F_GSA_LIB))
species_data_MEDBS <- species_data_MEDBS %>% mutate(Area = as.character(Area),
                                                      Ecoregion = as.character(Ecoregion))

# Remove "GSA" in the "F_GSA_LIB" column and clean whitespace
gfcm_shape$F_GSA_LIB <- gsub("GSA", "", gfcm_shape$F_GSA_LIB)
gfcm_shape$F_GSA_LIB <- gsub("\\s", "", gfcm_shape$F_GSA_LIB)

# Expand multi-area mappings in species_data
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

# Define ecoregion mapping (same structure as your provided code)
ecoregions <- c("1" = "Central Med.",
  "2" = "Eastern Med.",
  "3" = "Western Med.",
  "4" = "Black Sea") 

# Choose the ecoregion of interest:
# ========================================= Change here
selected_ecoregion_id <- 4
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

# Define bounding box in EPSG:3857 
x_limits <- c(-2500000, 5000000)
y_limits <- c(3000000, 7000000)

# Get the list of stock labels within the selected ecoregion
stock_labels <- species_data_MEDBS %>%
  filter(Ecoregion == ecoregion_selected, !is.na(F.FMSy)) %>%  
  distinct(StockKeyLabel) %>%
  pull(StockKeyLabel)

# Loop through stock labels and generate plots
for (stock in stock_labels) {
  
  # Filter species data for the stock (and non-missing Year and F.FMSy)
  species <- species_data_MEDBS %>% 
    filter(StockKeyLabel == stock) %>%
    filter(!is.na(Year) & !is.na(F.FMSy))
  
  # Normalize F.FMSy values
  species <- species %>%
    mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)))
  
  # Calculate the average F.FMSy_norm for the last 5 years
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
  
  # Get the areas associated with this stock (from species data)
  area_filtered <- species %>% distinct(Area)
  
  # Merge the filtered stock areas with GFMC shape:
  # Assign the chosen color to areas where the stock occurs, otherwise gray
  map_data <- gfcm_shape %>%
    mutate(color_category = ifelse(F_GSA_LIB %in% area_filtered$Area, category_color, "gray"))
  
  # Ensure color_category is treated as a factor
  map_data$color_category <- factor(map_data$color_category, 
                                    levels = c("red", "orange", "yellow", "green", "blue", "gray"))
  
  # Plot the GFMC areas with the filtered stock data highlighted
  map_plot <- ggplot() +
    geom_sf(data = world, fill = "gray90", color = "gray40") +  
    geom_sf(data = map_data, aes(fill = color_category), color = "black", alpha = 0.6) +  
    scale_fill_manual(
      name = "GES Status",
      values = c("red" = "red", "orange" = "orange", "yellow" = "yellow", 
                 "green" = "green", "blue" = "blue", "gray" = "gray"),
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
    labs(title = paste("Normalized Stock Distribution for", stock, 
                       "in", ecoregion_selected, "Over the Last 5 Years"))
  
  print(map_plot)
}
