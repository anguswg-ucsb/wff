# Angus Watters
# Join climate means, elevation, NWM, and HUC12 information w/ NAIP tiles

# notes:
# - results generate should be summarized on the basis of "NAIP_Entity" (eg 3610745_nw) and year of NAIP flight.
# - For NWM Flow, results are on a HUC12 basis, identify HUC12 w/ most area within each NAIP cell & link the NAIP grid to HUC12

rm(list = ls())

# Libraries
library(sf)
library(raster)
library(dplyr)
library(mapview)
library(terra)
library(foreach)
library(exactextractr)
# library(tidyverse)

source("utils/utils.R")

# ******************************************
# ---- Calculate NAIP tile wetland area ----
# ******************************************

# *******************************
# ---- Extract raster values ----
# *******************************

# CRS 
new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

# States of interest
states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

# States boundaries, transform to new CRS
state_shp <-
  USAboundaries::us_states() %>% 
  dplyr::filter(stusps %in% states) %>% 
  terra::vect() %>% 
  terra::project(new_crs)

# ---- rm1 ----
# Path to simplified state wetland geometries
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))

# Path to buffer raster tiles subsetted by state
r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))

# Total number of rasters to keep track of in iterations
nraster_files  <- 
  tibble::tibble(
    file = list.files(r_path)
  ) %>% 
  dplyr::mutate(
    state = substr(file, 0, 2)
  ) %>% 
  dplyr::group_by(state) %>% 
  dplyr::summarize(
    total_files      = dplyr::n()
    # total_files_csum = cumsum(total_files)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(
    total_files_csum   = cumsum(total_files),
    total_raster_files = sum(total_files)
  ) 

# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    cell_id       = as.character(CELL_ID),
    naip_entit    = as.character(NAIP_ENTIT),
    new_id        = 1:n()
  ) %>%
  dplyr::select(new_id, cell_id, naip_entit) %>% 
  sf::st_transform(new_crs) 

# i <- 1
# i <- 2

# success
# INFO [2022-05-19 16:58:01] Calculating total area of buffer wetlands
# INFO [2022-05-19 16:58:02] 36 of 40
# State: AZ
# Raster path: data/wetlands/raster_subset/AZ/AZ_buff_37110.tif

# error
# INFO [2022-05-19 16:58:04] 1 of 0
# INFO [2022-05-19 16:58:04] CELL ID: NA
# NAIP ENTIT: NA
# Buffer raster: AZ_buff_37110.tif
# Error: [as,sf] coercion failed. You can try coercing via a Spatial* (sp) class



final_naip_lst1 <- list()
final_naip_lst2 <- list()
final_skip_lst  <- list()
i = 1
# for (i in 1:length(wl_geom_path)) {
  
  # system.time(
  # naip_area_loop <- foreach::foreach(i = 1:length(wl_geom_path),  .combine = "rbind", .packages = c("terra", "sf", "dplyr", "raster", "tibble", "exactextractr" )) %dopar% {
  
  
  # State abbreviation
  state_abb   <- substr(wl_geom_path[i], 28, 29)
  
  nrasters <- 
    nraster_files %>%  
    dplyr::filter(state == state_abb)
  
  logger::log_info("\n\nLoading {state_abb} wetlands...\nState         -->  {state_abb}\n{state_abb} rasters    -->  {nrasters$total_files} of {nrasters$total_raster_files}  ({round(nrasters$total_files/nrasters$total_raster_files, 2)*100} % of total)\nTotal rasters -->  {nrasters$total_files_csum} of {nrasters$total_raster_files}  ({round(nrasters$total_files_csum/nrasters$total_raster_files, 2)*100} % completed)")
  
  # State wetlands 
  aoi         <- terra::vect(readRDS(wl_geom_path[i]))
  # aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c(1, 2, 6, 11, 12)]
  st_as_sf(gUnaryUnion(as_Spatial(layer), id = layer$DISID))},

  aoi_sf <-
    aoi %>% 
    sf::st_as_sf() %>% 
    sf::st_make_valid() %>% 
    dplyr::mutate(
      DISID = 1
      ) %>% 
    dplyr::select(ATTRIBUTE, DISID) %>% 
    sf::st_cast("MULTIPOLYGON")
  
 
  library(rgeos)
 aoi_u <-  st_as_sf(
    gUnaryUnion(
      as_Spatial(aoi_sf), 
      id = aoi_sf$DISID
      )
    )

  aoi_u <- 
    aoi_sf %>% 
    sf::st_make_valid()
  
  # %>% 
  #   sf::st_union()
    # summarise()
  terra::union()
  e1 <- ext(-10, 10, -20, 20)
  e2 <- ext(0, 20, -40, 5)
  union(e1, e2)
  
  #SpatVector
  v <- vect(system.file("ex/lux.shp", package="terra"))
  v <- v[,3:4]
  p <- vect(c("POLYGON ((5.8 49.8, 6 49.9, 6.15 49.8, 6 49.65, 5.8 49.8))", 
              "POLYGON ((6.3 49.9, 6.2 49.7, 6.3 49.6, 6.5 49.8, 6.3 49.9))"), crs=crs(v))
  values(p) <- data.frame(pid=1:2, value=expanse(p))
  u <- union(v, p)
  plot(u, "pid")
  union(v)
  
  plot(v)
  b <- buffer(v, 1000)
  # plot(u$id_1)
  u <- union(b)
# }

  # I think here since 50% is kinda rough anyway, I would fasterize the polygons to the binary mask, stack those, SpatRast => data.frame, group_by/summarize
  # haha so then make the naip file a third raster object in your stack





