# Angus Watters
# Join the NAIP entities and HUC12s to create a consistent dataset to work from

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
library(fasterize)
library(exactextractr)
library(nhdplusTools)

source("utils/utils.R")

# NAIP Grid
grid <- sf::read_sf("data/grid/NAIP_Grid_CRB.shp") %>% 
  sf::st_transform(4326) %>%
  st_transform(5070) %>% 
  sf::st_cast("MULTIPOLYGON") 

# HUC12s
huc12 <- sf::read_sf("data/huc12/WBDHU12_CO_CRB.shp") %>% 
  sf::st_transform(4326) %>%
  st_transform(5070) %>% 
  sf::st_cast("MULTIPOLYGON") %>% 
  mutate(
    new_id = 1:n()
  )

# empty raster
rtemp <- raster::raster(
  ext = extent(huc12),
  res = c(500, 500),
  crs = crs(huc12)
)

# Create raster of HUC12s
huc12_r <- fasterize::fasterize(
  huc12,
  raster = rtemp, 
  field  = "new_id"
  )

# extract most frequent HUC12 cells in NAIP tiles
huc_stats <- exactextractr::exact_extract(
  huc12_r,
  grid, 
  fun = "mode"
  )

# Join HUC12 ID to grid 
grid_huc <-
  grid %>% 
  dplyr::mutate(
    new_id = huc_stats
  ) %>% 
  dplyr::left_join(
    dplyr::select(sf::st_drop_geometry(huc12), huc12, new_id),
    by = "new_id"
  ) 

saveRDS(grid_huc, "data/grid/naip_huc_grid.rds")

# *******************************************************************



