# Set working directory
setwd("/Users/emma/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/NIVA/data")

# Load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Load spatial data
ices_shape <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")
gfcm_shape <- st_read("GSAs_simplified_updated_division/GSAs_simplified_division.shp")

# Transform to EPSG:3857
ices_shape <- st_transform(ices_shape, crs = 3857)
gfcm_shape <- st_transform(gfcm_shape, crs = 3857)

# Load and transform world map
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 3857)

# Define bounding box
x_limits <- c(-4898058, 7625385)
y_limits <- c(3000000, 17500000)

# Create a new column to distinguish the two types
ices_shape$type <- "ICES Areas"
gfcm_shape$type <- "GFCM Areas"

# Combine into one sf object
combined_grid <- rbind(
  select(ices_shape, geometry, type),
  select(gfcm_shape, geometry, type)
)

# Plot with legend
grid_plot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "gray70") +
  geom_sf(data = combined_grid, aes(color = type), fill = NA) +
  scale_color_manual(
    name = "Grids",
    values = c("ICES Areas" = "blue", "GFCM Areas" = "red")
  ) +
  coord_sf(xlim = x_limits, ylim = y_limits, expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(
    title = "Statistical Fisheries Areas for European Seas",
    x = NULL, y = NULL
  )

# Print the plot
print(grid_plot)

# Save the plot
ggsave("Grid_Map_Europe.png", grid_plot, width = 10, height = 8, dpi = 300)
