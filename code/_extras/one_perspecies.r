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

# Set working directory
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

# Load ICES shape file and transform CRS
ices_shape <- st_read("ICES_Areas_20160601_cut_dense_3857.shp") %>%
  st_transform(crs = 3857)

# Load background world map and transform CRS
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = 3857)

# Load data
species_data <- read_csv("sag_complete2.csv")
area <- read_csv("StocksPerArea2024.csv")

# Convert columns to character format
ices_shape <- ices_shape %>% mutate(Area_Full = as.character(Area_Full))
area <- area %>% mutate(ICES_Area = as.character(ICES_Area))

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

unique_stocks <- unique(species_data$StockKeyLabel)

# Loop through stock labels and generate plots
for (stock in unique_stocks) {

  # Optional: save both plots to file
  output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/stocks_logbook"

  new_stock_folder <- file.path(output_dir, stock)

  # Create the folder (if it doesn't already exist)
  if (!dir.exists(new_stock_folder)) {
    dir.create(new_stock_folder)
  }

  area_filtered <- area %>% filter(StockKeyLabel == stock)
  map_data <- left_join(ices_shape, area_filtered, by = c("Area_Full" = "ICES_Area"))


  # ==== PLOT 1: STOCK AREA ONLY (gray-highlighted) ====
  ices_shape_stock <- ices_shape %>%
    mutate(is_stock_area = ifelse(Area_Full %in% area_filtered$ICES_Area, TRUE, FALSE))

  location_plot <- ggplot() +
    geom_sf(data = world, fill = "gray90", color = "gray70") +
    geom_sf(data = ices_shape_stock, aes(fill = is_stock_area), color = "black", alpha = 0.5) +
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
  species <- species_data %>% 
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

