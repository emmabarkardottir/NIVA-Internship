# Set working directory (change as needed)
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")
getwd()

# Load required packages
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(zoo)

# Read the dataset
data <- read.csv("total_europe_stockdata2024.csv")

# Filter out missing values and normalize F.FMSy within each stock, then flip the values
data_all <- data %>%
  filter(!is.na(Year) & !is.na(F.FMSy)) %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)),
         F.FMSy_flipped = 1 - F.FMSy_norm) %>%
  ungroup()

# Compute average flipped F/FMSy per year across all stocks
avg_all <- data_all %>%
  group_by(Year) %>%
  summarise(F_FMSy_avg = mean(F.FMSy_flipped, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(F_FMSy_avg_smooth = rollmean(F_FMSy_avg, k = 3, fill = NA, align = "center"))

# Data for vertical lines and legend labels
events <- data.frame(
  Year = c(1983, 1992, 2002, 2013),
  Event = c("CFP Inception: 1983", 
            "First Reform: 1992", 
            "Second Reform: 2002", 
            "Third Reform: 2013")
)

# Generate the flipped plot with gray vertical lines and legend
plot_avg_all <- ggplot() +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.8, ymax = 1, fill = "blue", alpha = 0.7) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.6, ymax = 0.8, fill = "green", alpha = 0.7) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.4, ymax = 0.6, fill = "yellow", alpha = 0.8) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.2, ymax = 0.4, fill = "orange", alpha = 0.8) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0, ymax = 0.2, fill = "red", alpha = 0.7) +
  geom_line(data = avg_all, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 1.5) +
  geom_point(data = avg_all, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 2, shape = 16) +
  geom_vline(data = events, aes(xintercept = Year, color = Event), linetype = "dashed", size = 1) +
  scale_color_manual(name = "Policy Milestones", 
                     values = rep("gray40", 4)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(title = "Average Flipped F/FMSy Over Time Across All Stocks and Ecoregions",
       x = "Year", y = "Flipped F/FMSy (normalized)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the plot
print(plot_avg_all)

# Optional: Save to file
ggsave("Flipped_F_FMSy_avg_all_ecoregions_with_event_legend.png", plot = plot_avg_all, width = 10, height = 6)
