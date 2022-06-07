# *****************************************************
# *****************************************************
# Angus Watters
# Buffer NWI by 10m by state 

rm(list = ls())

# Libraries
library(sf)
library(raster)
library(dplyr)
library(mapview)
library(terra)
# # library(foreach)
# library(exactextractr)
# library(tidyverse)

source("utils/utils.R")

# **************************
# ---- Wetlands buffers ----
# **************************
wl_geom_path <- paste0(
  "data/wetlands/", 
  grep(
    "_geodatabase",
    list.files("data/wetlands/"),
    value = T 
  )
)

# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    cell_id       = as.character(CELL_ID),
    naip_entit    = as.character(NAIP_ENTIT),
    new_id        = 1:n()
  ) %>%
  # janitor::clean_names() %>% 
  dplyr::select(new_id, PRIMARY_ST, cell_id, naip_entit) %>% 
  sf::st_transform(crs("PROJCRS[\"NAD_1983_Albers\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]],\n            ID[\"EPSG\",6269]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"Degree\",0.0174532925199433]]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",23,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",29.5,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",45.5,\n            ANGLEUNIT[\"Degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"))
# %>% 
#   dplyr::mutate(
#     state = 
#       case_when(
#         primary_st == "Arizona"     ~ "AZ",   
#         primary_st == "Colorado"    ~ "CO",   
#         primary_st == "California"  ~ "CA",   
#         primary_st == "Nevada"      ~ "NV",   
#         primary_st == "New Mexico"  ~ "NM",   
#         primary_st == "Utah"        ~ "UT",   
#         primary_st == "Wyoming"     ~ "WY",   
#         TRUE                        ~ primary_st
#       )
#   ) %>% 
#   dplyr::relocate(new_id, cell_id, naip_entit, primary_st, state, geometry)
# Path to simplified state wetland geometries
# wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))
# wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))[c(2:7)]
crs(aoi)
# Path to NWI ATTRIBUTES to subset and buffer
veg_att_path <- "data/wetlands/NWI/NWI_Veg_Subset.txt"

# CRS 
new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

veg_att_lst <- readLines(veg_att_path)
# States of interest
# states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

# States boundaries, transform to new CRS
# state_shp <-
#   USAboundaries::us_states() %>% 
#   dplyr::filter(stusps %in% states) %>% 
#   terra::vect() %>% 
#   terra::project(new_crs)

# i <- 2
for (i in 1:length(wl_geom_path)) {
  
  # State abbreviation
  state_abb   <- substr(wl_geom_path[i], 15, 16)
  
  logger::log_info("\n\nLoading {state_abb} wetlands...\nState         -->  {state_abb}")
  
  aoi <- sf::read_sf(
    wl_geom_path[i],  
    layer = paste0(state_abb, "_Wetlands")
  ) %>% 
    dplyr::select(ATTRIBUTE)
  
  # State wetlands coverage (terra)
  # aoi <- terra::vect(
  #   wl_geom_path[i],  
  #   layer = paste0(state_abb, "_Wetlands")
  # )[,c("ATTRIBUTE")]
  if (state_abb == "CA") {
    
    grid_ca <- 
      grid %>%
      filter(PRIMARY_ST == "California") %>% 
      st_buffer(1000) %>% 
      sf::st_bbox() %>% 
      sf::st_as_sfc() %>% 
      sf::st_as_sf()
    
    aoi <- 
      aoi %>%
      sf::st_filter(grid_ca) %>% 
      dplyr::filter(ATTRIBUTE %in% veg_att_lst) %>% 
      sf::st_cast("MULTIPOLYGON") %>% 
      terra::vect()
    # mapview(aoi_ca) + grid_ca
    
  } else if(state_abb != "CA") {
    
    aoi <-
      aoi %>% 
      dplyr::filter(ATTRIBUTE %in% veg_att_lst) %>% 
      sf::st_cast("MULTIPOLYGON") %>% 
      terra::vect()
  }

  # aoi2 <- aoi[1:10000,]
  nbatches <- floor(nrow(aoi)/5000) 

  # k <- 5
  # rm(k)
  buffer_lst <- list()
  
  # for (k in 0:nrow(aoi2)){
  for (k in 0:nbatches){
    
    logger::log_info("\n\nbatch {k} of {nbatches}")
    
    row_batch <- (5000*k + 1):(5000 * (k + 1)) ##  define the rownumbers of the current batch
    aoi_sub <-  aoi[row_batch,] ## gives the current batch of rows in your dataframe 
 
    # aoi_buff <-
    #   aoi_sub %>%
    #   sf::st_cast("MULTIPOLYGON") %>% 
    #   sf::st_buffer(10)
    # system.time(
    aoi_buff <-
        aoi_sub %>%
        terra::buffer(10) %>%
        sf::st_as_sf()
    # )

    saveRDS(
      aoi_buff,
      paste0("data/wetlands/buffer/", state_abb, "/", state_abb, "_nwi_veg_buffer_batch_", k, ".rds"),
    )
    # k2 <- k + 1
    # tmp <- readRDS("data/wetlands/buffer/AZ/AZ_nwi_veg_buffer_batch_14.rds")
    # buffer_lst[[k2]] <- aoi_buff
    
  }
  
  # final_buff <- dplyr::bind_rows(buffer_lst)
  
}

buffer_subs_path <-  list.files("data/wetlands/buffer/")[!grepl(
  "final",
  list.files("data/wetlands/buffer/")
  )]

# i <- 1

for (i in 1:length(buffer_subs_path)) {
  
  logger::log_info("\n\n{buffer_subs_path[i]} buffered polygons")
  
  batch_paths <- list.files(paste0("data/wetlands/buffer/", buffer_subs_path[i]))
  
  batch_lst <- list() 

  
  for (z in 1:length(batch_paths)) {
    
    logger::log_info("\n\n{z} of {length(batch_paths)} batches")
    
    buffer_poly <- readRDS(paste0("data/wetlands/buffer/", buffer_subs_path[i], "/", batch_paths[z]))
    # %>% 
    #   sf::st_cast("MULTIPOLYGON")
    
    buffer_poly <- 
      buffer_poly %>% 
      # dplyr::group_by(ATTRIBUTE) %>% 
      dplyr::summarise() %>% 
      # dplyr::ungroup() %>% 
      sf::st_cast("MULTIPOLYGON") %>% 
      dplyr::mutate(
        buffer_batch = gsub(".rds", "", batch_paths[z])
      )
    
    # mapview(buffer_poly_u) + buffer_poly
    batch_lst[[z]] <- buffer_poly
    
  }
  
  final_buff <- dplyr::bind_rows(batch_lst)
  
  logger::log_info("Saving {buffer_subs_path[i]}")
  
  sf::st_write(
    final_buff,
    paste0("data/wetlands/buffer/final/", buffer_subs_path[i], "_nwi_veg_buffer.shp"),
    # paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.gpkg"),
    layer = paste0(buffer_subs_path[i], '_nwi_veg_buffer')
    # append = FALSE
  )
  
}

list.files(paste0("data/wetlands/buffer/", list.files("data/wetlands/buffer/")))

#  ****************************************************
#  ****************************************************

  # aoi_buff <- 
  #   aoi_buff %>% 
  #   sf::st_buffer(10) %>% 
  #   sf::st_cast("MULTIPOLYGON") 
  # 
  # logger::log_info("\n\nSaving buffered polygons as shapefiles...\nPath: data/wetlands/buffer/")
  # 
  # sf::st_write(
  #   # sf::write_sf(
  #   aoi_buff,
  #   paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.shp"),
  #   # paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.gpkg"),
  #   layer = paste0(state_abb, '_nwi_veg_buffer')
  #   # append = FALSE
  # )
  # 
  
# }


  aoi_sf <- aoi %>% 
    sf::st_as_sf()
  aoi_u <- aoi %>% 
    terra::union

  aoi_buff <- 
    aoi %>% 
    terra::buffer(10)
  # State Wetlands_Project_Metadata (terra)
  aoi_meta <- terra::vect(
    wl_geom_path[i],  
    layer = paste0(state_abb, "_Wetlands_Project_Metadata")
  )
  logger::log_info("Joining wetland geometries w/ metadata")
  
  aoi_join <- 
    aoi %>% 
    sf::st_as_sf() %>% 
    sf::st_join(
      sf::st_as_sf(
        aoi_meta
      )
    )
  aoi_join2 <-  aoi_join %>% distinct()
  system.time(
  aoi_join2 <- 
    aoi_join %>% 
    terra::vect()
  )
  
  terra::buffer()
  # # New CRS to project too
  # new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
  # 
  # # Reproject, simplify and convert to SF
  # logger::log_info("Reprojecting CRS:\n  {new_crs}")
  # 
  # poly <-   terra::project(wl, new_crs)
  
  # logger::log_info("Simplifying geometries")
  # poly <- terra::simplifyGeom(poly, 75)
  
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
  # State wetlands coverage (terra)
  aoi <- sf::read_sf(
    wl_geom_path[i],  
    layer = paste0(state_abb, "_Wetlands")
  )
  
  aoi2         <- sf::st_as_sf(readRDS(paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))[1]))
                          
  # State wetlands 
  # aoi         <- sf::st_as_sf(readRDS(wl_geom_path[i]))
  aoi         <- sf::read_sf(wl_geom_path[i])
  # aoi         <- sf::st_as_sf(readRDS(wl_geom_path[i]))[,c("ATTRIBUTE", "WETLAND_TYPE", "PROJECT_NAME","IMAGE_YR", "IMAGE_DATE")]
  # aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c("ATTRIBUTE", "WETLAND_TYPE", "PROJECT_NAME","IMAGE_YR", "IMAGE_DATE")]

  aoi_buff <- 
    aoi %>%
    dplyr::filter(ATTRIBUTE %in% veg_att_lst) 

    # aoi_buff <-  aoi_buff %>%  terra::buffer(10) %>% sf::st_as_sf()

  num_wl           <- length(unique(aoi_buff$ATTRIBUTE))
  individual_wl    <- nrow(aoi_buff)
  
  logger::log_info("\n\nUnique ATTRIBUTES in {state_abb} ---> {num_wl}\nIndividual polygons     ---> {individual_wl}")
  
  logger::log_info("\n\nCreating 10m buffer")

  aoi_buff <- 
    aoi_buff %>% 
    sf::st_buffer(10) %>% 
    sf::st_cast("MULTIPOLYGON") 
  
 # aoi_sub <- 
 #   aoi_buff2 %>% 
 #   mutate(wl_area = st_area(.)) %>% 
 #   arrange(-wl_area) %>% 
 #   dplyr::slice(1:10)
 # 
 # aoi_tmp <- 
 #   aoi %>%
 #   dplyr::filter(ATTRIBUTE %in% veg_att_lst) %>% 
 #   mutate(wl_area = st_area(.)) %>% 
 #   arrange(-wl_area) %>% 
 #   dplyr::slice(1:10)
 
 # mapview(aoi_sub, col.regions = "red")  + aoi_tmp
  # dplyr::filter(ATTRIBUTE %in%  c("PEM1B", "PEM1Fx"))

  logger::log_info("\n\nSaving buffered polygons as shapefiles...\nPath: data/wetlands/buffer/")
  
  # tmp <- sf::read_sf(paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.gpkg"))
  # aoi_u <-  aoi_buff %>%  dplyr::summarise() %>% 
  #   dplyr::mutate( state        = state_abb,   n_attributes = num_wl,  buffer       = "10m" )
  
  # Save individual buffers
  # sf::st_write(
  # # sf::write_sf(
  #   aoi_buff,
  #   # paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.shp")
  #   paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.gpkg"),
  #   layer = paste0(state_abb, '_nwi_veg_buffer')
  #   # append = FALSE
  #   )
  sf::st_write(
    # sf::write_sf(
    aoi_buff,
    paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.shp"),
    # paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer.gpkg"),
    layer = paste0(state_abb, '_nwi_veg_buffer')
    # append = FALSE
  )
  # logger::log_info("\n\nUnioning geometries to single polygon area")
  
  # Save union
  # sf::write_sf( aoi_u, paste0("data/wetlands/buffer/", state_abb, "_nwi_veg_buffer_union.shp"))
  # mapview(aoi_u, col.regions = "red") + mapview(aoi_sub, col.regions = "green") + mapview(aoi_buff, col.regions = "blue")

  # plot(aoi_sub$geometry)
  # plot(aoi_u$geometry, col = "red", add = T)
  # plot(aoi_buff$geometry, add = T)
}















