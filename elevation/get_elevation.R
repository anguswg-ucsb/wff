# Angus Watters
# Get mean elevation data for each NAIP tile

# notes:
# - results generate should be summarized on the basis of "NAIP_Entity" (eg 3610745_nw) and year of NAIP flight.
# - For NWM Flow, results are on a HUC12 basis, identify HUC12 w/ most area within each NAIP cell & link the NAIP grid to HUC12

rm(list = ls())

# Libraries
library(sf)
library(raster)
library(tidyverse)
library(mapview)
library(climateR)
library(zonal)
library(elevatr)
library(nhdplusTools)

source("utils/utils.R")

# NAIP Grid
grid <- sf::read_sf("data/grid/NAIP_Grid_CRB.shp") %>% 
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON") %>% 
  janitor::clean_names()

# get bounding box of subbasins
bb <-  
  grid %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

mapview(bb) + tmp

# Get elevation raster
system.time({
  elev <- elevatr::get_elev_raster(bb, z = 5) %>% 
    crop(bb)
})
mapview(bb)  + elev + tmp 

# mask stacks to districts 
elev_tiles <- lapply(
  X   = seq_len(nrow(grid)),
  FUN = function(x) {
    raster::mask(elev, grid[x, ])
  }
)

elev_tiles     <- lapply(X = elev_tiles, FUN = raster::stack)       # stack list of masked rasterstacks 
cell_names     <- paste0(grid$cell_id)                        # district number
elev_tiles     <- setNames(elev_tiles, nm = cell_names)   # add district names to stacks

# create tidy tibbles from each raster stack in list, then wrangle date columns
tidy_elev <- lapply(X = elev_tiles, FUN = tidy_elev_raster)

tidy_elev <- lapply(
  X = names(tidy_elev),
  FUN = function(x) {
    dplyr::mutate(
      tidy_elev[[x]],
      cell_id = x
    )
  }
) %>%
  dplyr::bind_rows() %>% 
  dplyr::group_by(cell_id) %>% 
  dplyr::summarize(elevation = mean(elevation, na.rm = T)) 

saveRDS(tidy_elev, "data/elevation/naip_elevation.rds")

mapview(bb)  + elev 

