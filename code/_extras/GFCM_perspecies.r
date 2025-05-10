# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(scales)
library(patchwork)
library (zoo)
library(tidyr)

# Set working directory
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

# Load GFCM shapefile and world map
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

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
y_limits <- c(3000000, 17500000)

unique_stocks <- unique(species_data_MEDBS$StockKeyLabel)

# Loop through stock labels and generate plots
for (stock in unique_stocks) {

  # Optional: save both plots to file
  output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/stocks_logbook"

  new_stock_folder <- file.path(output_dir, stock)

  # Create the folder (if it doesn't already exist)
  if (!dir.exists(new_stock_folder)) {
    dir.create(new_stock_folder)
  }

   area_filtered <- expanded_species_data_MEDBS %>% 
    filter(StockKeyLabel == stock) %>% 
    distinct(Area)

 area_filtered <- expanded_species_data_MEDBS %>% filter(StockKeyLabel == stock)
map_data <- left_join(gfcm_shape, area_filtered, by = c("F_GSA_LIB" = "Area"))


  # ==== PLOT 1: STOCK AREA ONLY (gray-highlighted) ====
  gfcm_shape_stock <- gfcm_shape %>%
    mutate(is_stock_area = ifelse(F_GSA_LIB %in% area_filtered$Area, TRUE, FALSE))

  location_plot <- ggplot() +
    geom_sf(data = world, fill = "gray90", color = "gray70") +
    geom_sf(data = gfcm_shape_stock, aes(fill = is_stock_area), color = "black", alpha = 0.5) +
    scale_fill_manual(
      values = c("TRUE" = "gray50", "FALSE" = "gray95"),
      guide = FALSE
    ) +
    coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
    theme_minimal() +
    labs(title = paste("Assessement Area for Stock", stock))

    ggsave(filename = file.path(new_stock_folder, paste0(stock, "_area_only.png")),
       plot = location_plot, width = 8, height = 6)
  
  
  # ==== DATA PREP ====
  species <- species_data_MEDBS %>% 
    filter(StockKeyLabel == stock) %>%
    filter(!is.na(Year) & !is.na(F.FMSy)) %>%
     mutate(F.FMSy_norm = 1 - rescale(F.FMSy, to = c(0, 1))) 
  
  if (dim(species)[1] == 0) {
    next
  }

  last_5_years <- species %>%
    filter(Year %in% tail(sort(unique(Year)), 5))
  
  average_F_FMSy_norm <- last_5_years %>%
    summarise(avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE))
  
  avg_value <- as.numeric(average_F_FMSy_norm$avg_F_FMSy_norm)

  category_color <- color_categories(avg_value)
  # ==== END DATA PREP ====


  # ==== PLOT 2: GES STATUS ====
  map_data_ges <- map_data %>%
    mutate(color_category = ifelse(!is.na(StockKeyLabel) & StockKeyLabel == stock, category_color, "gray")) %>%
    mutate(color_category = factor(color_category, levels = c("red", "orange", "yellow", "green", "blue", "gray")))

  ges_plot <- ggplot() +
    geom_sf(data = world, fill = "gray90", color = "gray40") +  
    geom_sf(data = map_data_ges, aes(fill = color_category), color = "black", alpha = 0.6) +  
    scale_fill_manual(
      name = "GES",
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
    labs(title = paste("GES Fisheries Status for Stock", stock))

  ggsave(filename = file.path(new_stock_folder, paste0(stock, "_GES_map.png")),
        plot = ges_plot, width = 8, height = 6)


  # ==== PLOT 3: STOCK TIME-SERIES ====
  # Calculate 3-year rolling average for F.FMSy
  time_series <- species %>%
    filter(StockKeyLabel == stock) %>%
    mutate(F_FMSy_rolling_avg = rollmean(F.FMSy_norm, k = 3, fill = NA, align = "center"))

  # Calculate the smoothed average for each year (if necessary for plotting points)
  species_avg <- time_series %>%
    group_by(Year) %>%
    summarise(F_FMSy_avg_smooth = mean(F.FMSy_norm, na.rm = TRUE))

  # Plotting the time series with GES status as colored rectangles
  time_series_plot <- ggplot(time_series, aes(x = Year)) +
    # GES categories (colored rectangles)
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.8, ymax = 1), fill = "blue") +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.6, ymax = 0.8), fill = "green") +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.4, ymax = 0.6), fill = "yellow") +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.2, ymax = 0.4), fill = "orange") +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0, ymax = 0.2), fill = "red") +
    geom_line(data = species_avg, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 1) +  
    geom_point(data = species_avg, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 1.5, shape = 16) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
    labs(title = paste("Time Series of F/FMSy for Stock", stock),
        x = "Year", y = "F/FMSy Normalized") +
    theme_minimal()  

  ggsave(filename = file.path(new_stock_folder, paste0(stock, "_timeseries.png")),
        plot = time_series_plot, width = 8, height = 6)

}

