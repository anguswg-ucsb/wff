# Angus Watters
# Simplify wetland geometries to boost performance/time spent on future calculations
# Divide CONUS Raster into raster covering each State of Interest

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
# library(tidyverse)

source("utils/utils.R")

# ***********************************
# ---- Simplify wetland polygons ----
# ***********************************
# # NAIP Grid
# grid <- readRDS("data/grid/naip_huc_grid.rds") %>% 
#   dplyr::mutate(cell_id = as.character(CELL_ID)) %>% 
#   dplyr::select(-CELL_ID)


# rm(buffer_path)
states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

# Setup parallel processing
cores <-  parallel::detectCores()
cl    <- parallel::makeCluster(cores[1]-4) #not to overload your computer
doParallel::registerDoParallel(cl)
# i <- 4


# huc_loop <- foreach::foreach(i = 1:length(states),  .combine = "rbind", .packages = c("terra", "sf")) %dopar% {
for (i in 4:length(states)) {
  
  aoi <- states[i] 

  wl_path <- paste0("data/wetlands/", aoi, "_geodatabase_wetlands.gdb")
  
  logger::log_info("State: {aoi}\nPath: {wl_path}")

  logger::log_info("Loading {aoi} wetlands polygons")

  # State wetlands coverage (terra)
  wl <- terra::vect(
    wl_path,  
    layer = paste0(aoi, "_Wetlands")
  )
  
  # State Wetlands_Project_Metadata (terra)
  wl_meta <- terra::vect(
    wl_path,  
    layer = paste0(aoi, "_Wetlands_Project_Metadata")
  )
  
  # New CRS to project too
  new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
  
  # Reproject, simplify and convert to SF
  logger::log_info("Reprojecting CRS:\n  {new_crs}")
  
  poly <-   terra::project(wl, new_crs)

  logger::log_info("Simplifying geometries")
  poly <- terra::simplifyGeom(poly, 75)

  logger::log_info("Converting to SF object")
  poly <- sf::st_as_sf(poly)

  # poly <  
  #   wl %>% 
  #   terra::project(new_crs) %>% 
  #   terra::simplifyGeom(75) %>% 
  #   sf::st_as_sf() 
  
  # mapview::npts(poly_simple)
  # mapview::npts(sf::st_as_sf(wl))

  logger::log_info("Joining wetland geometries w/ metadata")
  poly <- 
    poly %>% 
    sf::st_join(
      sf::st_as_sf(
        terra::project(wl_meta, new_crs)
        )
      )
  
  logger::log_info("Saving {aoi} simplified wetland polygons:\n
  data/wetlands/simple_geoms/")

 # Save
  saveRDS(
    poly,
    paste0(
      "data/wetlands/simple_geoms/", aoi, 
      "_wetlands_simple.rds" 
    )
    )
  
}

# Stop workers  
doParallel::stopImplicitCluster()
parallel::stopCluster(cl)

# **********************************************************

# *******************************
# ---- Locate state coverage ----
# *******************************
# Identify which state a raster is covering and save out the subset

buffer_path <- paste0(
  "data/wetlands/rasters/",
  grep(
    ".tif$",
    list.files("data/wetlands/rasters/", ".tif"),
    value = T
  )
)

new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

state_shp <- USAboundaries::us_states() %>% 
  filter(stusps %in% states) %>% 
  terra::vect() %>% 
  terra::project(new_crs)

for (i in 1:length(buffer_path)) {
  
  r <- terra::rast(buffer_path[i])

  logger::log_info("Identify raster location by state\nFile: {buffer_path[i]}\n{i} of {length(buffer_path)}")
  
  logger::log_info("Checking which state raster covers...")
  
  for (z in 1:length(state_shp$stusps)) {
    # z <- 4
    
    shp <- state_shp[z,]
    

    mask <- terra::mask(r, shp)
    
    extremes <-
      mask %>% 
      terra::minmax() %>% 
      tibble::tibble() %>% 
      setNames(c("min_max")) %>% 
      dplyr::mutate(
        name = c("min", "max")
      ) %>% 
      tidyr::pivot_wider(
        names_from  = name, 
        values_from = min_max 
        ) %>% 
      setNames(c("min", "max")) 
    
    if (is.nan(extremes$min[1]) & is.nan(extremes$max[1])) {
      
      logger::log_info("{shp$state_name} - No")
      
    } else {
     
      logger::log_info("{shp$state_name} - Yes")
      
      rname         <- gsub("data/wetlands/rasters/", "", buffer_path[i])
      
      tif_save_path <- paste0(
        "data/wetlands/raster_subset/",
        shp$state_abbr, "/", shp$state_abbr,  "_", rname
        )
      
      logger::log_info("Saving raster:\n {tif_save_path}")
      
      terra::writeRaster(
        r, 
        tif_save_path,
        overwrite = T
      )
      
    }
  }
}
# rm(buffer_path)
states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

# Setup parallel processing
cores <-  parallel::detectCores()
cl    <- parallel::makeCluster(cores[1]-4) #not to overload your computer
doParallel::registerDoParallel(cl)
# i <- 4


# huc_loop <- foreach::foreach(i = 1:length(states),  .combine = "rbind", .packages = c("terra", "sf")) %dopar% {
for (i in 4:length(states)) {
  
  aoi <- states[i] 
  
  wl_path <- paste0("data/wetlands/", aoi, "_geodatabase_wetlands.gdb")
  
  logger::log_info("State: {aoi}\nPath: {wl_path}")
  
  logger::log_info("Loading {aoi} wetlands polygons")
  
  # State wetlands coverage (terra)
  wl <- terra::vect(
    wl_path,  
    layer = paste0(aoi, "_Wetlands")
  )
  
  # State Wetlands_Project_Metadata (terra)
  wl_meta <- terra::vect(
    wl_path,  
    layer = paste0(aoi, "_Wetlands_Project_Metadata")
  )
  
  # New CRS to project too
  new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
  
  # Reproject, simplify and convert to SF
  logger::log_info("Reprojecting CRS:\n  {new_crs}")
  
  poly <-   terra::project(wl, new_crs)
  
  logger::log_info("Simplifying geometries")
  poly <- terra::simplifyGeom(poly, 75)
  
  logger::log_info("Converting to SF object")
  poly <- sf::st_as_sf(poly)
  
  # mapview::npts(poly_simple)
  # mapview::npts(sf::st_as_sf(wl))
  
  logger::log_info("Joining wetland geometries w/ metadata")
  poly <- 
    poly %>% 
    sf::st_join(
      sf::st_as_sf(
        terra::project(wl_meta, new_crs)
      )
    )
  
  logger::log_info("Saving {aoi} simplified wetland polygons:\n
  data/wetlands/simple_geoms/")
  
  # Save
  saveRDS(
    poly,
    paste0(
      "data/wetlands/simple_geoms/", aoi, 
      "_wetlands_simple.rds" 
    )
  )
  
}

doParallel::stopImplicitCluster()
parallel::stopCluster(cl)

# *******************************
# *******************************

