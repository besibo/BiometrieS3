# Load necessary packages
library(tidyverse)
library(sf)
library(marmap)

# Import AR data
geo <- read_csv("data/geo.csv")

# Transform data points into geographic objects
Sites_geo <- geo %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Get bathymetry data: please note I zoomed on the area of interest
# to get a more manageable dataset. If you want a larger area,
# you should increase res (e.g. res = 10) in order to get a
# bathy object of a reasonable size
bathy <- getNOAA.bathy(-45, -30, -20, -5, res = 4, keep = TRUE)
# load the bathymetry 
ggbathy <- fortify(bathy)

# Get countries outline
pays <- rnaturalearth::ne_countries(
  country = c("Brazil"),
  scale = "large", returnclass = "sf"
)

# Base plot
pl <- ggplot(data = pays) +
  geom_contour(
    data = ggbathy, aes(x = x, y = y, z = z),
    binwidth = 200, color = "grey90", size = 0.3
  ) +
  geom_contour(
    data = ggbathy, aes(x = x, y = y, z = z),
    binwidth = 1000, color = "grey70", size = 0.4
  ) +
  geom_sf() +
  geom_path(data = geo, aes(x = lon, y = lat, group = sex, col = sex)) +
  geom_sf(data = Sites_geo, aes(fill = speed, color = sex), shape = 21, size = 2) +
  coord_sf(xlim = c(-42, -32), ylim = c(-16, -10), expand = FALSE) +
  scale_fill_viridis_c() +
  labs(x = "", y = "", color = "Sex") +
  theme_bw(base_family = "ArcherPro Book")

# Add scale and North arrow
pl +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )
