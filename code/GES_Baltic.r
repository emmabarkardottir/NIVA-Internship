# Set working directory
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(patchwork)
library(scales)

# Load your data
data <- read.csv("sag_complete2.csv")

# Define selected stock labels
stock_labels <- c("her.27.28", "her.27.25-2932", "her.27.20-24", 
                  "ple.27.21-23", "ple.27.24-32", "spr.27.22-32")

# Filter, normalize, and flip F.FMSy for these stocks
cod_stocks <- data %>%
  filter(StockKeyLabel %in% stock_labels, !is.na(F.FMSy), !is.na(Year)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)),
         F.FMSy_flipped = 1 - F.FMSy_norm) %>%
  ungroup() %>%
  filter(!is.na(F.FMSy_flipped))

# Function to generate an individual plot for each stock
plot_flipped_fmsy <- function(stock_data, stock_label) {
  ggplot(stock_data, aes(x = Year, y = F.FMSy_flipped)) +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.8, ymax = 1), fill = "blue", alpha = 0.7) +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.6, ymax = 0.8), fill = "green", alpha = 0.7) +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.4, ymax = 0.6), fill = "yellow", alpha = 0.7) +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0.2, ymax = 0.4), fill = "orange", alpha = 0.7) +
    geom_rect(aes(xmin = min(Year), xmax = max(Year), ymin = 0, ymax = 0.2), fill = "red", alpha = 0.7) +
    geom_line(color = "black", linewidth = 0.7) +
    geom_point(color = "black", size = 1) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
    labs(title = paste("FMSY Trend for", stock_label),
         x = "Year", y = "Flipped F/FMSy (normalized)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11))  # Adjust size value as needed
}

# Generate plots for each stock
plots <- lapply(stock_labels, function(label) {
  stock_data <- cod_stocks %>% filter(StockKeyLabel == label)
  plot_flipped_fmsy(stock_data, label)
})

# Combine all plots into one figure using patchwork
combined_plot <- wrap_plots(plots, ncol = 3) +
  plot_annotation(title = "FMSY Trends for Baltic Sea Stocks")

# Print the combined plot
print(combined_plot)

output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, "GES_speciestrend.png"), 
  plot = combined_plot, width = 8, height = 6, dpi = 300)
