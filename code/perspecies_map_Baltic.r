# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readr)
library(scales)
library(patchwork)  # For combining plots

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

# Define the list of StockKeyLabels
stock_labels <- c("her.27.28", "her.27.25-2932", "her.27.20-24", "ple.27.21-23", "ple.27.24-32", "spr.27.22-32")

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
x_limits <- c(1003750.83, 3011252.49)
y_limits <- c(6276421.52, 9149042.33)

# Initialize an empty list to store the plots
plot_list <- list()

# Loop through stock labels and generate plots
for (stock in stock_labels) {
  species <- species_data %>% 
    filter(StockKeyLabel == stock) %>%
    filter(!is.na(Year) & !is.na(F.FMSy)) %>%
    mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)))
  
  # Take the last 5 years
  last_5_years <- species %>%
    filter(Year %in% tail(sort(unique(Year)), 5))
  
  # Calculate the average F.FMSy for those last 5 years
  average_F_FMSy_norm <- last_5_years %>%
    summarise(avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE))
  
  avg_value <- as.numeric(average_F_FMSy_norm$avg_F_FMSy_norm)
  
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
      name = "GES",
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
    labs(title = paste("MSY Status for", stock))
  
  # Store the plot in the list
  plot_list[[stock]] <- map_plot
}

# Generate an additional map for "her.27.28"
stock_single <- "her.27.28"

species_single <- species_data %>% 
  filter(StockKeyLabel == stock_single) %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)))

last_5_years_single <- species_single %>%
  filter(Year %in% tail(sort(unique(Year)), 5))

average_F_FMSy_norm_single <- last_5_years_single %>%
  summarise(avg_F_FMSy_norm = mean(F.FMSy_norm, na.rm = TRUE))

avg_value_single <- as.numeric(average_F_FMSy_norm_single$avg_F_FMSy_norm)

category_color_single <- color_categories(avg_value_single)

area_filtered_single <- area %>% filter(StockKeyLabel == stock_single)

map_data_single <- left_join(ices_shape, area_filtered_single, by = c("Area_Full" = "ICES_Area"))

map_data_single <- map_data_single %>%
  mutate(color_category = ifelse(!is.na(StockKeyLabel) & StockKeyLabel == stock_single, category_color_single, "gray"))

map_data_single$color_category <- factor(map_data_single$color_category, levels = c("red", "orange", "yellow", "green", "blue", "gray"))

map_her_27_28 <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray40") +  
  geom_sf(data = map_data_single, aes(fill = color_category), color = "black", alpha = 0.6) +  
  scale_fill_manual(
    name = "GES",
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
  labs(title = paste("MSY Status for", stock_single))

# Print the additional single-stock map
print(map_her_27_28)


# Combine all plots in a grid layout (2 rows, 3 columns) and add a global title
combined_plot <- (plot_list[[1]] + plot_list[[2]] + plot_list[[3]] +
                  plot_list[[4]] + plot_list[[5]] + plot_list[[6]] +
                  plot_layout(ncol = 3, nrow = 2)) + 
                  plot_annotation(title = "Stock MSY Status in the Baltic Sea for a 5-Year Average", 
                                  theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)))

# Print the combined plot
print(combined_plot)

output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, "perspecies_baltic.png"), 
  plot = combined_plot, width = 8, height = 6, dpi = 300)
