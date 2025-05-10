# Set working directory (change as needed)
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

# Load packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(scales)
library(zoo)

# Load data
data <- read.csv("total_europe_stockdata2024.csv")

generate_plots <- function(ecoregion_id, output_folder = NULL) {
  ecoregions <- c("1" = "Widely", 
                  "2" = "BoBiscay & Iberia", 
                  "3" = "Greater North Sea", 
                  "4" = "Arctic Ocean", 
                  "5" = "Iceland", 
                  "6" = "Baltic Sea", 
                  "7" = "Celtic Seas", 
                  "8" = "Azores",
                  "9" = "Central Med.",
                  "10" = "Eastern Med.",
                  "11" = "Western Med.",
                  "12" = "Black Sea")

  ecoregion <- ecoregions[as.character(ecoregion_id)]
  species_area <- filter(data, Ecoregion == ecoregion)
  species_area <- species_area %>% filter(!is.na(Year) & !is.na(F.FMSy))

  # Normalize and flip F.FMSy
  species_area <- species_area %>% 
    group_by(StockKeyLabel) %>%
    mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)),
           F.FMSy_flipped = 1 - F.FMSy_norm) %>%
    ungroup()

  # Store individual plots
  plots <- list()
  species_list <- unique(species_area$StockKeyLabel)

  for (species in species_list) {
    species_data <- filter(species_area, StockKeyLabel == species) 

    plot <- ggplot(species_data, aes(x = Year, y = F.FMSy_flipped)) + 
      annotate("rect", xmin = min(species_data$Year), xmax = max(species_data$Year), ymin = 0.8, ymax = 1, fill = "blue", alpha = 0.7) +
      annotate("rect", xmin = min(species_data$Year), xmax = max(species_data$Year), ymin = 0.6, ymax = 0.8, fill = "green", alpha = 0.7) +
      annotate("rect", xmin = min(species_data$Year), xmax = max(species_data$Year), ymin = 0.4, ymax = 0.6, fill = "yellow", alpha = 0.8) +
      annotate("rect", xmin = min(species_data$Year), xmax = max(species_data$Year), ymin = 0.2, ymax = 0.4, fill = "orange", alpha = 0.8) +
      annotate("rect", xmin = min(species_data$Year), xmax = max(species_data$Year), ymin = 0, ymax = 0.2, fill = "red", alpha = 0.7) +
      geom_line(color = "black", linewidth = 1.5) +
      geom_point(color = "black", size = 2) +
      scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
      labs(title = paste("Flipped F/FMSy Over Time for", species, "in", ecoregion),
           x = "Year", y = "Flipped F/FMSy (normalized)") +
      theme_minimal()
    
    plots[[species]] <- plot

    if (!is.null(output_folder)) {
      if (!dir.exists(output_folder)) {
        dir.create(output_folder, recursive = TRUE)
      }
      filename <- paste0("Flipped_F_FMSy_", gsub("[^A-Za-z0-9]", "_", species), "_", gsub("[^A-Za-z0-9]", "_", ecoregion), ".png")
      ggsave(filename = file.path(output_folder, filename), plot = plot, width = 8, height = 5)
    }
  }

  # Compute average flipped F/FMSy
  species_avg <- species_area %>%
    group_by(Year) %>%
    summarise(F_FMSy_avg = mean(F.FMSy_flipped, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(F_FMSy_avg_smooth = rollmean(F_FMSy_avg, k = 3, fill = NA, align = "center"))

  # Combined plot
current_year <- as.numeric(format(Sys.Date(), "%Y"))

combined_plot <- ggplot() +
  annotate("rect", xmin = 1950, xmax = current_year, ymin = 0.8, ymax = 1, fill = "blue", alpha = 0.7) +
  annotate("rect", xmin = 1950, xmax = current_year, ymin = 0.6, ymax = 0.8, fill = "green", alpha = 0.7) +
  annotate("rect", xmin = 1950, xmax = current_year, ymin = 0.4, ymax = 0.6, fill = "yellow", alpha = 0.8) +
  annotate("rect", xmin = 1950, xmax = current_year, ymin = 0.2, ymax = 0.4, fill = "orange", alpha = 0.8) +
  annotate("rect", xmin = 1950, xmax = current_year, ymin = 0, ymax = 0.2, fill = "red", alpha = 0.7) +
  geom_line(data = species_area, aes(x = Year, y = F.FMSy_flipped, group = StockKeyLabel), color = "gray", size = 1) + 
  geom_point(data = species_area, aes(x = Year, y = F.FMSy_flipped, group = StockKeyLabel), color = "gray", size = 1.5) +
  geom_line(data = species_avg, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 1.5) +  
  geom_point(data = species_avg, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 2, shape = 16) +
  scale_x_continuous(limits = c(1950, current_year), breaks = seq(1950, current_year, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(title = paste("FMSY Status Over Time for all stocks in", ecoregion, "with Smoothed Average"),
       x = "Year", y = "Flipped F/FMSy (normalized)") +  
  theme_minimal()


  for (p in plots) {
    print(p)
  }
  print(combined_plot)

  if (!is.null(output_folder)) {
    filename <- paste0("Flipped_F_FMSy_combined_", gsub("[^A-Za-z0-9]", "_", ecoregion), ".png")
    ggsave(filename = file.path(output_folder, filename), plot = combined_plot, width = 10, height = 6)
  }
}

# Example usage:
generate_plots(
  ecoregion_id = 6,
  output_folder = "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
)

