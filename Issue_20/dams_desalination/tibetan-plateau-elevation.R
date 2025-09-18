library(raster)
library(rayshader)
library(geodata)
library(ggplot2)
library(scales)

countries <- c("CHN", "IND", "NPL", "BTN", "MMR", "THA", "VNM", "LAO", "KHM", "BGD", "LKA", "TWN")

cols <- colorRampPalette(c(
  "#08306b",  # dark blue (low elevation)
  "#4292c6",  # light blue
  "#66c2a4",  # green
  "#fdae61",  # yellow-orange
  "#f46d43",  # orange
  "#f0f0f0"   # white (high elevation)
))(256)

# Download elevation rasters (30 arc-seconds ≈ 1km)
rasters <- lapply(countries, function(code) {
  elevation_30s(country = code, path = tempdir())
})

# Merge all into a single raster
asia_dem <- do.call(merge, rasters)

# Crop to a tidy extent (adjust as needed)
asia_crop <- crop(asia_dem, extent(60, 125, 5, 45))

# Replace all negative values with 0
asia_crop[asia_crop < 0] <- 0


# Example: Reduce size by 4x in both dimensions (16x faster)
lowres <- aggregate(asia_crop, fact = 4, fun = mean)
elev_matrix <- raster_to_matrix(lowres)

# Shade the heightmap (returns an RGB array)
colored <- height_shade(elev_matrix, texture = cols)


# Now render 3D with plot_3d
# NORTH AT TOP: Use theta = 0 and phi = 90 (straight down) for map-style orientation
tibet_shade <- plot_3d(
  colored,
  heightmap = elev_matrix,
  zscale = 10,       # Increase for more "height"
  fov = 0,           # Orthographic projection
  theta = 0,         # Looking from the top (north is up)
  phi = 70,          # Directly overhead
  zoom = 0.7,
  shadow = FALSE,
  windowsize = c(700, 500),
  background = "transparent"
  )


save_png(tibet_shade, "tibetan-plateau-output1.png") # not 3D

# Save output
render_snapshot("tibetan-plateau-output.png") # 3D but low res

####

## LEGEND

elev_range <- range(elev_matrix, na.rm = TRUE)

# Create a data frame for the legend
legend_df <- data.frame(
  elev = seq(elev_range[1], elev_range[2], length.out = 256),
  color = cols
)

# Plot the legend
ggplot(legend_df, aes(x = 1, y = elev, fill = elev)) +
  geom_tile() +
  scale_fill_gradientn(colors = cols, name = "Elevation (meters)") +
  theme_void() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggsave("elevation_legend.svg", width = 1, height = 5, dpi = 300)


