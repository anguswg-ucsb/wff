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
# new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
# new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
 new_crs <- "PROJCRS[\"WGS_1984_Web_Mercator_Auxiliary_Sphere\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    ID[\"EPSG\",3857]]"
 
# States of interest
states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

# States boundaries, transform to new CRS
state_shp <-
  USAboundaries::us_states() %>% 
  dplyr::filter(stusps %in% states) %>% 
  terra::vect() %>% 
  terra::project(new_crs)

# --- rm1 ---
# Path to simplified state wetland geometries
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))
# wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))[c(4, 7)]

# Path to buffer raster tiles subsetted by state
# r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))
r_path           <- "D:/wff/buffer_raster/final_filter_tif.tif"
save_subset_path <- "D:/wff/subset_raster/"

# CONUS full raster buffer
r_buff <- terra::rast(r_path)

rm(r_buff)
gc()

# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    new_id        = 1:n()
  ) %>%
  janitor::clean_names() %>% 
  dplyr::select(new_id, cell_id, naip_entit, primary_st) %>% 
  sf::st_transform(new_crs) %>%
  dplyr::mutate(
    state = 
      case_when(
        primary_st == "Arizona"         ~ "AZ",   
        primary_st == "Baja California" ~ "AZ",
        primary_st == "Sonora"          ~ "AZ",
        primary_st == "Chihuahua"       ~ "AZ",
        primary_st == "Colorado"        ~ "CO",   
        primary_st == "California"      ~ "CA",   
        primary_st == "Nevada"          ~ "NV",   
        primary_st == "New Mexico"      ~ "NM",   
        primary_st == "Utah"            ~ "UT",   
        primary_st == "Wyoming"         ~ "WY",   
        TRUE                            ~ primary_st
      )
  ) %>% 
  dplyr::select(new_id, cell_id, naip_entit, state, geometry) %>% 
  dplyr::group_by(state) %>% 
  dplyr::group_split()


i <- 1

# ---- i loop ----
for (i in 1:length(wl_geom_path)) {
  
  # State abbreviation
  state_abb   <- substr(wl_geom_path[i], 28, 29)
  
  logger::log_info("\n\nLoading {state_abb} wetlands...\n{i} of {length(wl_geom_path)} states")

  # State wetlands 
  # aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c(1, 2, 6, 11, 12)]

  st_grid <-
    grid[[i]] %>% 
    sf::st_bbox() %>% 
    sf::st_as_sfc() %>% 
    sf::st_as_sf()

  
  logger::log_info("\n\nCropping raster to state bbox...")
  
  # Crop buffer raster to state grid 
  rsub <- 
    r_buff %>%
    terra::crop(st_grid) 
  

  logger::log_info("\n\nSaving cropped raster...")
  
  terra::writeRaster(
    rsub, 
    paste0(save_subset_path, state_abb, "_buffer_raster.tif"),
    overwrite = T
    )
  
  logger::log_info("\n\nRemoving tmp files...")
  tmpFiles(current = TRUE, orphan = FALSE, old = FALSE, remove = TRUE)
  
  rm(rsub)
  
  logger::log_info("\n\nCalling garbage collector...")
  
  gc()
  # mapview(grid_bb) + st_grid
  

  # aoi         <- aoi[,c(1, 2, 6, 11, 12)]
  
  # path to state raster subset
  # subset_path <- grep(
  #   state_abb,
  #   r_path,
  #   value = T
  #   
  # ) # full file path to state raster subsets
  # state_subs_path <- paste0(subset_path, "/", list.files(subset_path)) 
  # 
  # r_buff <- terra::rast(state_subs_path[15]) 
  # 
  # r1 <- terra::rast(state_subs_path[15]) %>% 
  #   raster::raster()
  # # c(11758, 11759, 2053, 4784,4579, 4580, 10395)
  # grid_tmp <-
  #   st_grid %>%
  #   filter(new_id %in% c(11758, 11759, 2053, 4784,  4579, 4580,
  #                        10395, 2054, 4785, 10026, 11829, 2055))
  # # mapview(grid_tmp, col.regions = "red") + r1 + st_grid
  # grid_bb <- 
  #   grid_tmp %>% 
  #   sf::st_bbox() %>% 
  #   sf::st_as_sfc() %>% 
  #   sf::st_as_sf()
  # 
  # mapview(grid_tmp, col.regions = "red") + r1 + grid_bb
  # 
  # rsub <- 
  #   r_buff %>%
  #   terra::crop(grid_bb) 
  # # %>% 
  # #   terra::mask(terra::vect(grid_tmp))
  # 
  # plot(rsub)
  
  # rsub <-
  #   r1 %>% 
  #   raster::crop(grid_bb)
  # %>% 
  #   raster::mask(grid_tmp)
  
  # mapview(grid_tmp, col.regions = "red") + grid_bb + rsub

}

# ***************************
# ---- State subset loop ----
# ***************************

# CRS 
new_crs <- "PROJCRS[\"WGS_1984_Web_Mercator_Auxiliary_Sphere\",\n    BASEGEOGCRS[\"WGS 84\",\n        DATUM[\"World Geodetic System 1984\",\n            ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4326]],\n    CONVERSION[\"unnamed\",\n        METHOD[\"Popular Visualisation Pseudo Mercator\",\n            ID[\"EPSG\",1024]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"False easting\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    ID[\"EPSG\",3857]]"
# new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

# Path to simplified state wetland geometries
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))

# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    new_id        = 1:n()
  ) %>%
  janitor::clean_names() %>% 
  dplyr::select(new_id, cell_id, naip_entit, primary_st) %>% 
  sf::st_transform(new_crs) %>%
  dplyr::mutate(
    state = 
      case_when(
        primary_st == "Arizona"         ~ "AZ",   
        primary_st == "Baja California" ~ "AZ",
        primary_st == "Sonora"          ~ "AZ",
        primary_st == "Chihuahua"       ~ "AZ",
        primary_st == "Colorado"        ~ "CO",   
        primary_st == "California"      ~ "CA",   
        primary_st == "Nevada"          ~ "NV",   
        primary_st == "New Mexico"      ~ "NM",   
        primary_st == "Utah"            ~ "UT",   
        primary_st == "Wyoming"         ~ "WY",   
        TRUE                            ~ primary_st
      )
  ) %>% 
  # dplyr::filter(state %in% c("CO", "NV", "NM", "UT", "WY")) %>% 
  dplyr::select(new_id, cell_id, naip_entit, state, geometry) %>% 
  dplyr::group_by(state) %>% 
  dplyr::group_split()

# path to buffer raster state subsets
save_subset_path <- "D:/wff/subset_raster/"

#  buffer raster state subsets
subset_path <- paste0(save_subset_path, list.files(save_subset_path))

# i <- 3

final_naip_lst1 <- list()
final_naip_lst2 <- list()

for (i in 1:length(subset_path)) {
  
  # State abbreviation
  state_abb   <- substr(wl_geom_path[i], 28, 29)
  
  logger::log_info("\n\nLoading {state_abb} wetlands...\n{i} of {length(wl_geom_path)} states")
  
  # State wetlands 
  aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c("ATTRIBUTE", "WETLAND_TYPE", "PROJECT_NAME", "IMAGE_YR", "IMAGE_DATE")]

  # state raster buffer
  r_buff <- terra::rast(subset_path[i])
  
  # State NAIP tiles subset
  st_grid <- grid[[i]] 
  # rm(tmp)
  # tmp <- final_naip_df1 %>% filter(state == "CA") 
  # length(unique(tmp$cell_id))
  logger::log_info("\n\nCropping raster to state bbox...")
  

     # tmp_aoi <-  st_grid %>% st_filter(aoi2)
      # st_filter(st_cast(filter(aoi2, ATTRIBUTE == "L1UBHh"), "MULTIPOLYGON"))
    
      # tmp_grid <- filter(st_grid, !cell_id %in% unique(tmp_aoi$cell_id))
      
      # mapview(tmp_aoi) + st_cast(filter(aoi2, ATTRIBUTE == "L1UBHh"), "MULTIPOLYGON")
    
      # 3611105_nw, 10348
      # 3410961_sw, 8216
      # k  <- 1
      # k <- 4398
  
  sub_lst1 <- list()
  sub_lst2 <- list()
  
  for (k in 1:nrow(st_grid)) {
    
      logger::log_info("\n\n{k} of {nrow(st_grid)} NAIP tiles --- {round(((k)/(nrow(st_grid)))*100, 2)} %")
      
      
      # NAIP tiles info
      k_new_id     <- st_grid[k,]$new_id
      k_cell_id    <- st_grid[k,]$cell_id
      k_naip_entit <- st_grid[k,]$naip_entit
      buffer_path  <- subset_path[i]
      buffer       <- substr(subset_path[i], 22, 41)
      
      logger::log_info("\n\n\n{k} of {nrow(st_grid)} NAIP Tiles\nCELL ID       -->  {k_cell_id}\nNAIP ENTIT    -->  {k_naip_entit}\nBuffer raster -->  {buffer}\n")
      
      
      # mask raster to NAIP tile
      r_mask <- 
        r_buff %>%
        terra::crop(terra::vect(st_grid[k,])) %>%
        terra::mask(terra::vect(st_grid[k,]))
      
  
      
      # # Crop state wetlands to state grid/buffer raste
      aoi_crop <- clip_polygons(
        polygon = aoi,
        # polygon = terra::vect(st_grid[k,]),
        rmask   = r_mask
      )
    
    # mapview(r_mask) + st_grid[k,] + aoi_crop
    
    if(is.null(aoi_crop)) {
      
        # Empty NAIP WETLANDS dataframe for 0 matches 
        naip_wetlands  <-
            empty_wetlands(
              k_new_id     = k_new_id,
              k_cell_id    = k_cell_id, 
              k_naip_entit = k_naip_entit,
              state_abb    = state_abb,
              buffer       = buffer,   
              buffer_path  = buffer_path
            ) %>% 
            dplyr::select(new_id, cell_id, naip_entit, state,majority, wl_area, 
                          tmp_id, ATTRIBUTE, WETLAND_TYPE, PROJECT_NAME, IMAGE_YR,
                          IMAGE_DATE, buffer, buffer_path)
        
        # sapply(naip_wetlands, class)
        # sapply(naip_wetlands2, class)
        
        # Empty AOI U dataframe for 0 matches 
        aoi_u         <-
            empty_aoi(
            k_new_id     = k_new_id,
            k_cell_id    = k_cell_id, 
            k_naip_entit = k_naip_entit,
            state_abb    = state_abb,
            buffer       = buffer,   
            buffer_path  = buffer_path
          )
        
        sub_lst1[[k]] <- aoi_u
        sub_lst2[[k]] <- naip_wetlands
      
    } else {
      
        logger::log_info("\n\nExtract raster values from wetland polygons")
      
        # Extract raster values from wetland polygons
        polygon_area <- 
          extract_join(
            polygon     = aoi_crop, 
            rmask       = r_mask,
            extract_fun = "majority",
            join_by     = "tmp_id"
            )     
        
        # Individual NAIP tile wetlands
        naip_wetlands <-
          polygon_area %>% 
          sf::st_drop_geometry() %>% 
          tibble::tibble() %>% 
          dplyr::mutate(
            new_id      = k_new_id,
            cell_id     = k_cell_id,
            naip_entit  = k_naip_entit,
            state       = state_abb,
            buffer      = buffer,
            buffer_path = buffer_path
          ) %>% 
          dplyr::select(new_id, cell_id, naip_entit, state,majority, wl_area,
                        tmp_id, ATTRIBUTE, WETLAND_TYPE, PROJECT_NAME, IMAGE_YR,
                        IMAGE_DATE, buffer, buffer_path)
        
          # dplyr::select(new_id, cell_id, naip_entit, state,majority, wl_area,
                      # tmp_id, ATTRIBUTE, IMAGE_YR, IMAGE_DATE, buffer, buffer_path)
        
        logger::log_info("\n\nSummarizing NAIP tile wetland area by ATTRIBUTE & IMAGE_YR...")
        
        # ATTRIBUTE & IMAGE_YR total NAIP tile wetland area
        aoi_u <-
          extract_area(
            polygon = polygon_area
            ) %>% 
          dplyr::mutate(
            new_id      = k_new_id,
            cell_id     = k_cell_id,
            naip_entit  = k_naip_entit,
            state       = state_abb,
            buffer      = buffer,
            buffer_path = buffer_path
          ) %>% 
          dplyr::select(new_id, cell_id, naip_entit, state, total_wl_area,
                        ATTRIBUTE, IMAGE_YR, buffer, buffer_path)
        
        logger::log_info("\n\nTotal wetland area  ---> {round(sum(aoi_u$total_wl_area), 2)} m^2")
        
        sub_lst1[[k]] <- aoi_u
        sub_lst2[[k]] <- naip_wetlands
    }
      
    rm(r_mask)
    
  }
  
  final_naip_lst1[[i]] <- dplyr::bind_rows(sub_lst1)
  final_naip_lst2[[i]] <- dplyr::bind_rows(sub_lst2)
  
  logger::log_info("\n\nRemoving variables...")
  
  rm(rbuff, aoi)
  
  logger::log_info("\n\nCalling garbage collector...")
  
  gc()
  
}

# bind loop output lists
final_naip_df1 <-  dplyr::bind_rows(final_naip_lst1)
final_naip_df2 <-  dplyr::bind_rows(final_naip_lst2)

# local save path
save_naip_path <- "D:/wff/wetlands/naip"

# Save table 1
saveRDS(
    final_naip_df1,
    paste0(save_naip_path, "/naip_wetlands_area.rds")
  )

# Save table 2
saveRDS(
    final_naip_df2,
    paste0(save_naip_path, "/naip_wetlands.rds")
  )

# # Save table
# saveRDS(
#   naip_wetland_area,
#   paste0(save_naip_path, "/naip_wetlands_area_final.rds")
# )
# 
# saveRDS(
#   naip_wetland,
#   paste0(save_naip_path, "/naip_wetlands_final.rds")
# )

# ************************************

# ********************
# ---- Final save ----
# ********************

save_naip_path <- "D:/wff/wetlands/naip"

naip_wetland_area <- readRDS(paste0(save_naip_path, "/naip_wetlands_area_final.rds"))
naip_wetland      <- readRDS(paste0(save_naip_path, "/naip_wetlands_final.rds"))

naip_wetland_area <- naip_wetland_area[!duplicated(naip_wetland_area$cell_id), ] 

naip_wetland_area <-
  naip_wetland_area %>% 
  dplyr::mutate(total_wetland_area = round(total_wl_area, 2)) %>% 
  dplyr::select(cell_id, naip_entit, total_wetland_area)

wl_summary <- 
  # wl_df[!duplicated(wl_df$wl_area), ] %>%
  naip_wetland %>% 
  distinct(cell_id, ATTRIBUTE,IMAGE_YR, majority, wl_area) %>%
  dplyr::mutate(
    wl_area = case_when(
      majority == 1 ~ wl_area,
      majority == 0 ~ 0,
    )
  ) %>% 
  dplyr::group_by(cell_id, ATTRIBUTE, IMAGE_YR) %>% 
  dplyr::summarize(
    wetland_area = round(sum(wl_area, na.rm = T), 3)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(
    dplyr::select(naip_wetland_area, cell_id, naip_entit),
    by = "cell_id"
  ) %>% 
  dplyr::relocate(cell_id, naip_entit) 

length(unique(wl_summary$cell_id))

# save skipped NAIP tiles 
saveRDS(
  naip_wetland_area,
  here::here("data","wetlands", "naip_area", "final", "naip_wetlands_area2.rds")
)

# save skipped NAIP tiles 
saveRDS(
  wl_summary,
  here::here("data","wetlands", "naip_area", "final", "naip_wetlands2.rds")
)

# save skipped NAIP tiles 
readr::write_csv(
  wl_summary,
  here::here("data","wetlands", "naip_area", "final", "naip_wetlands2.csv")
)

# ************************************



  