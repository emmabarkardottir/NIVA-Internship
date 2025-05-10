# Set working directory
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

# Load required libraries
library(dplyr)  # For data manipulation
library(tidyr)  # For reshaping data
library(readr)  # For reading .csv files
library(ggplot2) # For plotting
library(patchwork) # For combining the plots

# Load data
data1 <- read.csv("sag_complete2_BalticSea.csv") 

# Filter for the specific stock and remove missing values
species_data1 <- data1 %>%
  filter(StockKeyLabel == "her.27.28") %>%
  filter(!is.na(Year) & !is.na(F.FMSy) & !is.na(B.BMSY))  # Ensure both columns exist

# Check if necessary columns exist before plotting
if (!all(c("F.FMSy", "B.BMSY") %in% names(species_data1))) {
  stop("Required columns (F.FMSy or B.BMSY) are missing in the dataset.")
}

# Create the plot (F/FMSy)
plot1 <- ggplot(species_data1, aes(x = Year, y = F.FMSy)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", linewidth = 1) + 
  labs(title = "F/FMSy Over Time for her.27.28",
       x = "Year",
       y = "F/FMSy") +
  theme_minimal()

# Create the plot (B/BMSY)
plot2 <- ggplot(species_data1, aes(x = Year, y = B.BMSY)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "blue", size = 2) +
  geom_hline(yintercept = 1, color = "black", linetype = "dashed", linewidth = 1) + 
  labs(title = "B/BMSy Over Time for her.27.28",
       x = "Year",
       y = "B/BMSY") +
  theme_minimal()

# Combine plots side by side for a landscape format
combined_plot <- plot1 + plot2 + plot_layout(ncol = 2)  # Changed to ncol = 2 for a landscape layout
print(combined_plot)
