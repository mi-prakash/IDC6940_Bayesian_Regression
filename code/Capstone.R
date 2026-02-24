library(ggplot2)
library(sf)
library(dplyr)
library(tigris)

# Load data
gsoy <- read.csv("/Users/miprakash/UWF/4 - Capsone Project/Project/IDC6940_Bayesian_Regression/code/4163416.csv")

gsoy$YEAR <- as.integer(substr(gsoy$DATE, 1, 4))
# Convert station coordinates into spatial data
stations_sf <- st_as_sf(gsoy, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# -------------------------
# 1) Distribution of annual precipitation across stations (2024)
# -------------------------
ggplot(gsoy, aes(x = PRCP)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25,
                 color = "white",
                 fill = "steelblue",
                 alpha = 0.9) +
  geom_density(linewidth = 1, color = "black") +
  labs(
    title = "Distribution of Annual Precipitation (2024)",
    x = "Precipitation (mm)",
    y = "Density"
  ) +
  theme_minimal()

# ------------------------------------
# 2) Spatial visualization (Florida map)
# ------------------------------------
options(tigris_use_cache = TRUE)

florida <- states(cb = TRUE) |>
  filter(STUSPS == "FL") |>
  st_transform(4326)

ggplot() +
  geom_sf(data = florida, fill = "white", color = "black") +
  geom_sf(data = stations_sf, aes(color = PRCP), size = 1.5, alpha = 0.85) +
  scale_color_viridis_c(option = "roma", name = "Rainfall (mm)") +
  coord_sf(
    xlim = st_bbox(florida)[c("xmin","xmax")],
    ylim = st_bbox(florida)[c("ymin","ymax")]
  ) +
  labs(
    title = "Spatial Distribution of Average Annual Rainfall (Florida)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()