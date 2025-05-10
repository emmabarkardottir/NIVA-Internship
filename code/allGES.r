# Set working directory (adjust path if needed)
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

# Filter out missing values and normalize F/FMSY within each stock, then flip the values
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

# Data for policy milestone lines
events <- data.frame(
  Year = c(1983, 1992, 2002, 2013),
  Event = c("CFP Inception: 1983", 
            "First Reform: 1992", 
            "Second Reform: 2002", 
            "Third Reform: 2013")
)

# Plot with colored bands, smooth trend line, and event markers
# Plot with text labels slightly shifted left
plot_avg_all <- ggplot() +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.8, ymax = 1, fill = "blue", alpha = 0.7) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.6, ymax = 0.8, fill = "green", alpha = 0.7) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.4, ymax = 0.6, fill = "yellow", alpha = 0.8) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0.2, ymax = 0.4, fill = "orange", alpha = 0.8) +
  annotate("rect", xmin = min(data_all$Year), xmax = max(data_all$Year), ymin = 0, ymax = 0.2, fill = "red", alpha = 0.7) +
  geom_line(data = avg_all, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 1.5) +
  geom_point(data = avg_all, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 2, shape = 16) +
  geom_vline(data = events, aes(xintercept = Year), color = "gray40", linetype = "dashed", size = 0.7) +
  annotate("text", x = events$Year - 1.4, y = 0.02, label = events$Event, angle = 90, hjust = 0, size = 3, color = "black") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(title = "FMSY Status Over Time Across All Stocks",
       x = "Year", y = "Flipped F/FMSy (normalized)") +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_avg_all)

# Save the correct plot
output_dir <- "/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/plots"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  filename = file.path(output_dir, "overallGES.png"), 
  plot = plot_avg_all,      
  width = 8, 
  height = 6, 
  dpi = 300
)

