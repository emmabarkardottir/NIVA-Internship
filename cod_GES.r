setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")  
getwd()

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(zoo)

data <- read.csv("total_europe_stockdata2024.csv")

# Filter only cod stocks and remove missing values
cod_stocks <- data %>%
  filter(grepl("^cod\\.", StockKeyLabel)) %>%
  filter(!is.na(Year) & !is.na(F.FMSy))

# Normalize and flip F/FMSy
cod_stocks <- cod_stocks %>%
  group_by(StockKeyLabel) %>%
  mutate(F.FMSy_norm = rescale(F.FMSy, to = c(0, 1)),
         F.FMSy_flipped = 1 - F.FMSy_norm) %>%
  ungroup() %>%
  filter(!is.na(F.FMSy_flipped))

# Compute the average flipped F/FMSy per year
cod_avg <- cod_stocks %>%
  group_by(Year) %>%
  summarise(F_FMSy_avg = mean(F.FMSy_flipped, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(F_FMSy_avg_smooth = rollmean(F_FMSy_avg, k = 3, fill = NA, align = "center")) %>%
  filter(!is.na(F_FMSy_avg_smooth))

# Build the plot
cod_plot <- ggplot() +
  annotate("rect", xmin = min(cod_stocks$Year), xmax = max(cod_stocks$Year), ymin = 0.8, ymax = 1, fill = "blue", alpha = 0.7) +
  annotate("rect", xmin = min(cod_stocks$Year), xmax = max(cod_stocks$Year), ymin = 0.6, ymax = 0.8, fill = "green", alpha = 0.7) +
  annotate("rect", xmin = min(cod_stocks$Year), xmax = max(cod_stocks$Year), ymin = 0.4, ymax = 0.6, fill = "yellow", alpha = 0.8) +
  annotate("rect", xmin = min(cod_stocks$Year), xmax = max(cod_stocks$Year), ymin = 0.2, ymax = 0.4, fill = "orange", alpha = 0.8) +
  annotate("rect", xmin = min(cod_stocks$Year), xmax = max(cod_stocks$Year), ymin = 0, ymax = 0.2, fill = "red", alpha = 0.7) +
  geom_line(data = cod_stocks, aes(x = Year, y = F.FMSy_flipped, group = StockKeyLabel), color = "gray", size = 1) + 
  geom_point(data = cod_stocks, aes(x = Year, y = F.FMSy_flipped, group = StockKeyLabel), color = "gray", size = 1.5) +
  geom_line(data = cod_avg, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 1.5) +  
  geom_point(data = cod_avg, aes(x = Year, y = F_FMSy_avg_smooth), color = "black", size = 2, shape = 16) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  labs(title = "Flipped F/FMSy Over Time for Cod Stocks in All Ecoregions",
       x = "Year", y = "Flipped F/FMSy (normalized)") +  
  theme_minimal()

print(cod_plot)
