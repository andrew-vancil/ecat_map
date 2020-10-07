library(dplyr)
library(tidyr)
library(ecat)
library(sf)
mapview::mapviewOptions(fgb = FALSE)

# sample random points and calculate ecat

ecat_bb <-
  st_bbox(ecat:::elevation.raster) %>%
  st_as_sfc(ecat_bb)

# ecat_points <-
#   st_sample(ecat_bb, size = 100000) %>%
#   st_as_sf()

# ecat_points$ecat <-
#   ecat_points %>%
#   st_transform(4326) %>%
#   st_coordinates() %>%
#   as_tibble() %>%
#   rename(lat = Y, lon = X) %>%
#   mutate(id = 1:nrow(ecat_points)) %>%
#   ecat::calculate_ecat()

# saveRDS(ecat_points, "ecat_points.rds")
ecat_points <- readRDS("ecat_points.rds")

mapview::mapview(ecat_bb) +
  mapview::mapview(ecat_points, zcol = "ecat")

library(sp)
# library(gstat)
# library(automap)

ecat_points <-
  ecat_points %>%
  na.omit() %>%
  st_transform(5072)

vg_af <- automap::autofitVariogram(ecat ~ 1, as(ecat_points, "Spatial"))

plot(vg_af)
grDevices::dev.copy2pdf(file = "ecat_points_variogram.pdf",
                        width = 11,
                        height = 8.5)


# define grid
grid_sf <-
  ecat_bb %>%
  st_transform(5072) %>%
  st_make_grid(
    cellsize = c(400, 400),
    what = "centers"
  ) %>%
  st_as_sf() %>%
  cbind(., st_coordinates(.))

mapview::mapview(grid_sf)

grid_sp <- as(grid_sf, "Spatial")
gridded(grid_sp) <- TRUE
grid_sp <- as(grid_sp, "SpatialPixels")

ecat_grid <-
  gstat::krige(ecat ~ coords.x1 + coords.x2,
               as(ecat_points, "Spatial"),
               grid_sp,
               model = vg_af$var_model) %>%
  st_as_sf() %>%
  select(ecat = var1.pred)

sf::st_write(ecat_grid, "ecat_grid.gpkg")

mapview::mapview(ecat_grid, zcol = "ecat")