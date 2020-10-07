library(dplyr)
library(tidyr)
library(ecat)
library(sf)
mapview::mapviewOptions(fgb = FALSE)

# ecat prediction bounding box
ecat_bb <-
    st_bbox(ecat:::elevation.raster) %>%
    st_as_sfc(ecat_bb) %>%
    st_transform(5072)

# define grid
# 250 m square cells; 153,594 total grid cells
grid_sf <-
  ecat_bb %>%
  st_make_grid(
    offset = st_bbox(ecat_bb)[c("xmin", "ymin")],
    cellsize = c(250, 250),
    what = "polygons"
  ) %>%
  st_as_sf()

# estimate ecat at each grid centroid
# and use as value for polygon
grid_sf$ecat <-
  grid_sf %>%
  st_centroid() %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lat = Y, lon = X) %>%
  mutate(id = seq_len(nrow(.))) %>%
  ecat::calculate_ecat()

# remove grids where ecat cannot be predicted
# i.e. points in bounding box but outside of elevation raster extent
grid_sf <- na.omit(grid_sf)

sf::st_write(grid_sf, "ecat_grid.gpkg")

mapview::mapview(grid_sf, zcol = "ecat")

# create example map
map_roads <-
    tigris::primary_roads() %>%
    st_transform(5072) %>%
    st_intersection(ecat_bb)

library(tmap)

tm <-
    tm_shape(st_transform(map_roads, 3735)) +
    tm_lines(col = "black", lwd = 4, lty = "dashed") +
    tm_shape(st_transform(grid_sf, 3735)) +
    tm_fill("ecat",
        n = 10, pallete = "YlOrRd", alpha = 0.8,
        title = expression(ECAT~(ug/m^"3")),
      legend.format = list(text.separator = "-")) +
    tm_scale_bar() +
    tm_legend(position = c("left", "top"))
tm

tmap_save(tm, "ecat_map_250_m_grid.png")

tmap_save(tm, "ecat_map_250_m_grid.html")

# define grid shifted up and right by 125 m
grid_sf_shifted <-
  ecat_bb %>%
  st_make_grid(
    offset = st_bbox(ecat_bb)[c("xmin", "ymin")] + c(125, 125),
    cellsize = c(250, 250),
    what = "polygons"
  ) %>%
  st_as_sf()

grid_sf_shifted$ecat <-
  grid_sf_shifted %>%
  st_centroid() %>%
  st_transform(4326) %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(lat = Y, lon = X) %>%
  mutate(id = seq_len(nrow(.))) %>%
  ecat::calculate_ecat()

sf::st_write(grid_sf_shifted, "ecat_grid_shifted.gpkg")