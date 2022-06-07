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

# --- rm1 ---
# Path to simplified state wetland geometries
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))
# wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))[c(4, 7)]

# Path to buffer raster tiles subsetted by state
r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))

# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    cell_id       = as.character(CELL_ID),
    naip_entit    = as.character(NAIP_ENTIT),
    new_id        = 1:n()
  ) %>%
  dplyr::select(new_id, cell_id, naip_entit) %>% 
  sf::st_transform(new_crs) %>% 
  dplyr::mutate(
    state = 
      case_when(
        primary_st == "Arizona"     ~ "AZ",   
        primary_st == "Colorado"    ~ "CO",   
        primary_st == "California"  ~ "CA",   
        primary_st == "Nevada"      ~ "NV",   
        primary_st == "New Mexico"  ~ "NM",   
        primary_st == "Utah"        ~ "UT",   
        primary_st == "Wyoming"     ~ "WY",   
        TRUE                        ~ primary_st
      )
  ) %>% 
  dplyr::relocate(new_id, cell_id, naip_entit, primary_st, state, geometry)


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

final_naip_lst1 <- list()
final_naip_lst2 <- list()
final_skip_lst  <- list()

# Setup parallel processing
# cores <-  parallel::detectCores()
# cl    <- parallel::makeCluster(cores[1]-4) #not to overload your computer
# doParallel::registerDoParallel(cl)
# system.time(
# rm(aoi, aoi1, aoi2, all_aoi)
# i <- 1
# i <- 2
# ---- i loop ----
for (i in 1:length(wl_geom_path)) {

  # State abbreviation
  state_abb   <- substr(wl_geom_path[i], 28, 29)
  
  nrasters <- 
    nraster_files %>%  
    dplyr::filter(state == state_abb)
  
  logger::log_info("\n\nLoading {state_abb} wetlands...\nState         -->  {state_abb}\n{state_abb} rasters    -->  {nrasters$total_files} of {nrasters$total_raster_files}  ({round(nrasters$total_files/nrasters$total_raster_files, 2)*100} % of total)\nTotal rasters -->  {nrasters$total_files_csum} of {nrasters$total_raster_files}  ({round(nrasters$total_files_csum/nrasters$total_raster_files, 2)*100} % completed)")
  
  # State wetlands 
  aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c(1, 2, 6, 11, 12)]
  # aoi         <- aoi[,c(1, 2, 6, 11, 12)]

  # path to state raster subset
  subset_path <- grep(
    state_abb,
    r_path,
    value = T
  )
  
  # logger::log_info("\n\nExtracting raster values...\nState: {state_abb}")
  

  # full file path to state raster subsets
  state_subs_path <- paste0(subset_path, "/", list.files(subset_path)) 

  state_subs_path <- state_subs_path[ !grepl(
                                    "AZ_buff_37112.tif|AZ_buff_37113.tif|AZ_buff_37114|AZ_buff_37111|AZ_buff_37110", 
                                    state_subs_path) ]

  # --- rm2 ---
  # state_subs_path <- paste0(subset_path, "/", list.files(subset_path))[11:12]
  # state_subs_path <- paste0(subset_path, "/", list.files(subset_path))[1:2]
  
  
  
  # State of interest polygon 
  shp <- 
    state_shp %>% 
    sf::st_as_sf() %>% 
    dplyr::filter(stusps == state_abb) # %>% terra::vect()
  
  # Remove grid tiles that are less than 50% covered in state
  aoi_grid <- trim_tiles(
                    grid_polygon  = grid,
                    bb            = shp,
                    coverage_pct  = 0.50
                    )
  
  rsub_lst1 <- list()
  rsub_lst2 <- list()
  skip_lst  <- list()
  
  # z = 1
  # z <- 2
  # ---- z loop ----
  for (z in 1:length(state_subs_path)) {
  # for (z in 35:36) {
    
    logger::log_info("\n\n{z} of {length(state_subs_path)} raster tiles\nState        --> {state_abb}\nRaster path  --> {state_subs_path[z]}")
    
    rsub <- terra::rast(state_subs_path[z])
    # rsub2 <- raster::raster(state_subs_path[z])
    
    # aggregate to speed up coverage pct check
    r_agg <-
      rsub %>%
      terra::aggregate(fact = 6, fun = "max") %>%
      raster::raster()

    # mapview(naip_cover, color = "red") + aoi_grid + rsub + r_agg
    
    # raster tile bounding box
    r_bb <- 
      r_agg %>%
      # rsub %>%
      sf::st_bbox() %>% 
      sf::st_as_sfc() %>%
      sf::st_as_sf()
    
    # filter naip tiles to those within the raster tiles bounding box
    naip_tiles <-
      aoi_grid %>% 
      sf::st_filter(r_bb)
    
    # mapview(rsub2) + mapview(aoi_grid, col.regions = "red") + r_bb +  mapview(naip_tiles, col.regions = "blue")
    
    logger::log_info("Raster covering {nrow(naip_tiles)} NAIP tiles")
    
    if(nrow(naip_tiles) == 0) {
      
      logger::log_info("Skipping raster:\n{state_subs_path[z]}")
      
      # take note of raster w/o any NAIP tile coverage
      skip_df <- 
        tibble::tibble(
          raster_path = state_subs_path[z],
          state       = state_abb
          )
      
      # Add to list, all skipped "z" raster subsets in "i" state  
      skip_lst[[z]] <- skip_df
      
      next                                         # Skip iteration
      
      } else {

        naip_tiles <- 
          naip_tiles %>% 
          dplyr::mutate(tmp_id = 1:n()) 
      
    
        # Percent of polygon covered by raster cells
        r_pct_cover <- exactextractr::exact_extract(
          r_agg,
          # rsub,
          naip_tiles,
          coverage_area = T,
          fun           = "count",
          force_df      = T
        ) %>%
          dplyr::mutate(tmp_id = 1:dplyr::n()) %>% 
          setNames(c("cover_area", "tmp_id"))
  
          # base::names(r_pct_cover) <- c("cover_area", "tmp_id")
        # --- rm3 ---
        
        # filter NAIP tiles that are not atleast 50% covered by raster tile
        naip_cover <-
          naip_tiles %>%
          dplyr::left_join(
            r_pct_cover,
            by = "tmp_id"
          ) %>% # dplyr::left_join(r_pct_cover_agg, by = "tmp_id") %>%
          dplyr::mutate(
            naip_tile_area = as.numeric(naip_tile_area),    # naip_tile_area  = round(as.numeric(naip_tile_area), 5),
            cover_pct      = cover_area/naip_tile_area      # cover_pct       = round(cover_area/naip_tile_area, 5)
            ) %>%
          dplyr::relocate(
            new_id, cell_id, naip_entit, tmp_id, naip_tile_area,
            intersect_area, coverage, cover_area, cover_pct, geometry
            )  %>%
          dplyr::filter(cover_pct >= 0.50) 
        
        # %>% dplyr::slice(1:2)
        
        if(nrow(naip_cover) == 0) {
          
          logger::log_info("Insufficient coverage - skipping raster:\n{state_subs_path[z]}")
          
          # take note of raster w/o any NAIP tile coverage
          skip_df <- 
            tibble::tibble(
              raster_path = state_subs_path[z],
              state       = state_abb
            )
          
          # Add to list, all skipped "z" raster subsets in "i" state  
          skip_lst[[z]] <- skip_df
          
          next  
          
          } else {
    
        # r2 <- raster::raster(rsub)
        # mapview(r2) + naip_cover
        
        # k <- 1
        # k <- 2
        
        naip_lst1 <- list()
        naip_lst2 <- list()
        
        # ---- k loop ----
        for(k in 1:nrow(naip_cover)){
          
       
          # NAIP tiles info
          k_new_id     <- naip_cover[k,]$new_id
          k_cell_id    <- naip_cover[k,]$cell_id
          k_naip_entit <- naip_cover[k,]$naip_entit
          buffer_path  <- state_subs_path[z]
          buffer       <- substr(state_subs_path[z], 32, 49)
      
          logger::log_info("\n\n\n{k} of {nrow(naip_cover)} NAIP Tiles\nCELL ID       -->  {k_cell_id}\nNAIP ENTIT    -->  {k_naip_entit}\nBuffer raster -->  {buffer}\n")
          

          # mask raster to NAIP tile
          r_mask <- 
            rsub %>%
            terra::crop(terra::vect(naip_cover[k,])) %>%
            terra::mask(terra::vect(naip_cover[k,]))
          
          aoi_crop <- clip_polygons(
            polygon = aoi,
            # polygon = terra::vect(naip_cover[k,]),
            rmask   = r_mask
          )

         if(is.null(aoi_crop)) {
           
            # logger::log_info("\n \nZero wetlands found in NAIP tile:\n \nCELL ID           -->  {k_cell_id}\nNAIP ENTIT        -->  {k_naip_entit}\nWetlands polygons -->  {nrow(aoi_crop)}\n")
           
           # Empty NAIP WETLANDS dataframe for 0 matches 
           naip_wetlands <- empty_wetlands(
                               k_new_id     = k_new_id,
                               k_cell_id    = k_cell_id, 
                               k_naip_entit = k_naip_entit,
                               state_abb    = state_abb,
                               buffer       = buffer,   
                               buffer_path  = buffer_path
                               )
           
           # Empty AOI U dataframe for 0 matches 
           aoi_u         <- empty_aoi(
                               k_new_id     = k_new_id,
                               k_cell_id    = k_cell_id, 
                               k_naip_entit = k_naip_entit,
                               state_abb    = state_abb,
                               buffer       = buffer,   
                               buffer_path  = buffer_path
                               )
          
            # naip_lst1[[k]]    <- aoi_u
            # naip_lst2[[k]]    <- naip_wetlands
    
          } else {
            
            logger::log_info("\n\n\n{nrow(aoi_crop)} wetlands found in NAIP tile:\n \nCELL ID           -->  {k_cell_id}\nNAIP ENTIT        -->  {k_naip_entit}\nWetlands polygons -->  {nrow(aoi_crop)}")

            # Identify which Wetland polygons have a majority 1 value from buffer raster 
            # union geometries and calculate total area

            aoi_u         <- mask_area(
                                rmask     = r_mask,
                                polygon   = aoi_crop,
                                union     = TRUE
                              )
            aoi_u <- 
              aoi_u %>% 
              dplyr::mutate(
                new_id      = k_new_id,
                cell_id     = k_cell_id,
                naip_entit  = k_naip_entit,
                state       = state_abb,
                buffer      = buffer,
                buffer_path = buffer_path
                ) %>% 
              dplyr::relocate(new_id, cell_id, naip_entit, total_wl_area, state, buffer, buffer_path) 
            
            logger::log_info("\n\nTotal wetland area  -->  {round(aoi_u$total_wl_area, 2)} m^2\n")
            
            # Individual wetlands in NAIP tile 
            naip_wetlands <- mask_area(
                                rmask     = r_mask,
                                polygon   = aoi_crop,
                                union     = FALSE
                              )
            
            naip_wetlands <- 
              naip_wetlands %>% 
              dplyr::mutate(
                new_id      = k_new_id,
                cell_id     = k_cell_id,
                naip_entit  = k_naip_entit,
                state       = state_abb,
                buffer      = buffer,
                buffer_path = buffer_path
              )
          }
          
          # Add all NAIP tiles to list within "k" NAIP tiles in "z" raster subset in "i" state 
          naip_lst1[[k]]    <- aoi_u
          naip_lst2[[k]]    <- naip_wetlands
              }
          }
        }
    
    # Add all NAIP tiles to list within "z" raster subset in "i" state  
    rsub_lst1[[z]]      <- dplyr::bind_rows(naip_lst1)
    rsub_lst2[[z]]      <- dplyr::bind_rows(naip_lst2)
  }
  
  # Bind rows for all NAIP tiles within "i" state subset 
  naip_wl_area  <- dplyr::bind_rows(rsub_lst1)
  naip_wl       <- dplyr::bind_rows(rsub_lst2)
  naip_skip     <- dplyr::bind_rows(skip_lst)
  
  # save NAIP wetlands area
  saveRDS(
    naip_wl_area,
    paste0(
      here::here("data","wetlands", "naip_area", state_abb, "/"),
      state_abb, "_naip_wetlands_area.rds"
      )
    )  
  
  # save individual NAIP wetlands 
  saveRDS(
    naip_wl,
    paste0(
      here::here("data","wetlands", "naip_area", state_abb, "/"),
      state_abb, "_naip_wetlands.rds"
      )
    )
  
  # save skipped NAIP tiles 
  saveRDS(
    naip_skip,
    paste0(
      here::here("data","wetlands", "naip_area", state_abb, "/"),
      state_abb, "_skip.rds"
      )
    )
  
  # Add all NAIP tiles to list within "i" state subset 
  # final_naip_lst1[[i]] <- dplyr::bind_rows(rsub_lst1)
  # final_naip_lst2[[i]] <- dplyr::bind_rows(rsub_lst2)
  # final_skip_lst[[i]]  <- dplyr::bind_rows(skip_lst)
  # 
}

# *****************************************************
# *****************************************************
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

# --- rm1 ---
# Path to simplified state wetland geometries
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))
# wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))[c(4, 7)]

# Path to buffer raster tiles subsetted by state
r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))

# *****************************
# ---- NAIP Wetlands loop ----
# ****************************

# Path to buffer raster tiles subsetted by state
r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))

# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    new_id        = 1:n()
  ) %>%
  janitor::clean_names() %>% 
  dplyr::mutate(
    cell_id       = as.character(cell_id),
    naip_entit    = as.character(naip_entit)
  ) %>%
  dplyr::select(new_id, cell_id, naip_entit, primary_st) %>% 
  sf::st_transform(new_crs) %>% 
  dplyr::mutate(
    state = 
      case_when(
        primary_st == "Arizona"     ~ "AZ",   
        primary_st == "Colorado"    ~ "CO",   
        primary_st == "California"  ~ "CA",   
        primary_st == "Nevada"      ~ "NV",   
        primary_st == "New Mexico"  ~ "NM",   
        primary_st == "Utah"        ~ "UT",   
        primary_st == "Wyoming"     ~ "WY",   
        TRUE                        ~ primary_st
        )
    ) %>% 
  dplyr::mutate(
      state = dplyr::case_when(
        state %in% c("Sonora", "Baja California") ~ "AZ",
        state == "Chihuahua" ~ "NM", 
        TRUE ~ state
        
      )
    )
  dplyr::relocate(new_id, cell_id, naip_entit, primary_st, state, geometry)

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

# naip_area_path <- list.files(here::here("data","wetlands",  "naip_area", list.files(here::here("data", "wetlands", "naip_area"))))
naip_area_path <- list.files(here::here("data","wetlands",  "naip_area"), recursive	= T)


area_paths <-
  here::here("data","wetlands",  "naip_area", 
             grep(
               "area",
               grep(
                 "_naip_wetlands", 
                 naip_area_path,
                 value = T
                 ),
               value = T
               )
  )

skip_paths <-
  here::here("data","wetlands",  "naip_area",
               grep(
                 "skip", 
                 naip_area_path,
                 value = T
               )
  )


# i <- 1
# rm(i)
area_lst <- list()
skip_lst <- list()

for (i in 1:length(area_paths)) {

  logger::log_info("\n\n{i} of {length(area_paths)}\nFilepath: {substr(area_paths[i], 70, 95)}")
  
  wl_area    <- readRDS(area_paths[i])
  skip_tiles <- readRDS(skip_paths[i])
  
  area_lst[[i]] <- wl_area
  skip_lst[[i]] <- skip_tiles
}


area_df <- dplyr::bind_rows(area_lst)
skip_df <- dplyr::bind_rows(skip_lst)


# shp <- 
#   state_shp %>% 
#   st_as_sf() %>% 
#   dplyr::filter(state_abbr == "CA")

# grid <-
#   grid %>% 
#   dplyr::filter(!new_id %in% area_df$new_id) 

grid <- 
  grid %>% 
  filter(!cell_id %in% area_df$cell_id) %>% 
  mutate(
    state = case_when(
      state %in% c("Sonora", "Baja California") ~ "AZ",
      state == "Chihuahua" ~ "NM", 
      TRUE ~ state
      
    )
  )
# sub_miss <-
#   missed_tiles %>% 
#   dplyr::filter(state == "CA") 

# mapview(sub_miss, col.regions = "red") + missed_tiles

# i <- 1
# i <- 2
# ---- i loop ----
for (i in 1:length(wl_geom_path)) {
  
  # State abbreviation
  state_abb   <- substr(wl_geom_path[i], 28, 29)
  
  nrasters <-
    nraster_files %>%
    dplyr::filter(state == state_abb)
  
  logger::log_info("\n\nLoading {state_abb} wetlands...\nState         -->  {state_abb}\n{state_abb} rasters    -->  {nrasters$total_files} of {nrasters$total_raster_files}  ({round(nrasters$total_files/nrasters$total_raster_files, 2)*100} % of total)\nTotal rasters -->  {nrasters$total_files_csum} of {nrasters$total_raster_files}  ({round(nrasters$total_files_csum/nrasters$total_raster_files, 2)*100} % completed)")
  
  # State wetlands 
  aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c(1, 2, 6, 11, 12)]
  # aoi         <- aoi[,c(1, 2, 6, 11, 12)]
  
  # path to state raster subset
  subset_path <- grep(
    state_abb,
    r_path,
    value = T
  )
  
  # full file path to state raster subsets
  state_subs_path <- paste0(subset_path, "/", list.files(subset_path)) 
  
  # state_subs_path <- state_subs_path[ !grepl(
  #   "AZ_buff_37112.tif|AZ_buff_37113.tif|AZ_buff_37114|AZ_buff_37111|AZ_buff_37110",
  #   state_subs_path) ]
  
  # --- rm2 ---
  # state_subs_path <- paste0(subset_path, "/", list.files(subset_path))[11:12]
  # state_subs_path <- paste0(subset_path, "/", list.files(subset_path))[1:2]
  
  
  
  # State of interest polygon 
  shp <- 
    state_shp %>% 
    sf::st_as_sf() %>% 
    dplyr::filter(stusps == state_abb) # %>% terra::vect()
  
  
  # Remove grid tiles that are less than 50% covered in state
  # aoi_grid <- trim_tiles(
  #   grid_polygon  = grid,
  #   bb            = shp,
  #   coverage_pct  = 0.50
  # )
  
  # aoi_grid <- grid
  aoi_grid <-
    grid %>%
    dplyr::filter(state == state_abb)
  
  rsub_lst1 <- list()
  rsub_lst2 <- list()
  skip_lst  <- list()
  
  # z = 3
  # z <- 2
  # ---- z loop ----
  for (z in 1:length(state_subs_path)) {
    # for (z in 35:36) {
    
    logger::log_info("\n\n{z} of {length(state_subs_path)} raster tiles\nState        --> {state_abb}\nRaster path  --> {state_subs_path[z]}")
    
    rsub <- terra::rast(state_subs_path[z])
    # rsub2 <- raster::raster(state_subs_path[z])
    
    # aggregate to speed up coverage pct check
    r_agg <-
      rsub %>%
      terra::aggregate(fact = 6, fun = "max") %>%
      raster::raster()
    
    # mapview(naip_cover, color = "red") + aoi_grid + rsub + r_agg
    
    # raster tile bounding box
    r_bb <- 
      r_agg %>%
      # rsub %>%
      sf::st_bbox() %>% 
      sf::st_as_sfc() %>%
      sf::st_as_sf()
    
    # filter naip tiles to those within the raster tiles bounding box
    naip_tiles <-
      aoi_grid %>% 
      sf::st_filter(r_bb)
    
    # mapview(rsub2) + mapview(aoi_grid, col.regions = "red") + r_bb +  mapview(naip_tiles, col.regions = "blue")
    
    logger::log_info("Raster covering {nrow(naip_tiles)} NAIP tiles")
    
    if(nrow(naip_tiles) == 0) {
      
      logger::log_info("Skipping raster:\n{state_subs_path[z]}")
      
      # take note of raster w/o any NAIP tile coverage
      skip_df <- 
        tibble::tibble(
          raster_path = state_subs_path[z],
          state       = state_abb
        )
      
      # Add to list, all skipped "z" raster subsets in "i" state  
      skip_lst[[z]] <- skip_df
      
      next                                         # Skip iteration
      
    } else {
      
      naip_tiles <- 
        naip_tiles %>%
        # aoi_grid %>% 
        dplyr::mutate(tmp_id = 1:n()) 
      
      
      # Percent of polygon covered by raster cells
      r_pct_cover <- exactextractr::exact_extract(
        r_agg,
        # rsub,
        naip_tiles,
        coverage_area = T,
        fun           = "count",
        force_df      = T
      ) %>%
        dplyr::mutate(tmp_id = 1:dplyr::n()) %>% 
        setNames(c("cover_area", "tmp_id"))
      
      # base::names(r_pct_cover) <- c("cover_area", "tmp_id")
      # --- rm3 ---
      
      # filter NAIP tiles that are not atleast 50% covered by raster tile
      naip_cover <-
        naip_tiles %>%
        dplyr::left_join(
          r_pct_cover,
          by = "tmp_id"
        ) %>% # dplyr::left_join(r_pct_cover_agg, by = "tmp_id") %>%
        dplyr::mutate(
          naip_tile_area = as.numeric(st_area(.)),
          # naip_tile_area = as.numeric(naip_tile_area),  
          cover_pct      = cover_area/naip_tile_area    
        ) %>% 
        dplyr::filter(cover_pct >= 0.50)
      
      # %>% dplyr::slice(1:2)
      
      if(nrow(naip_cover) == 0) {
        
        logger::log_info("Insufficient coverage - skipping raster:\n{state_subs_path[z]}")
        
        # take note of raster w/o any NAIP tile coverage
        skip_df <- 
          tibble::tibble(
            raster_path = state_subs_path[z],
            state       = state_abb
          )
        
        # Add to list, all skipped "z" raster subsets in "i" state  
        skip_lst[[z]] <- skip_df
        
        next  
        
      } else {
        
        # r2 <- raster::raster(rsub)
        # mapview(r2) + naip_cover
        
        # k <- 26
        # k <- 2
        # k <- 1
        naip_lst1 <- list()
        naip_lst2 <- list()

        # ---- k loop ----
        for(k in 1:nrow(naip_cover)){
          
          
          # NAIP tiles info
          k_new_id     <- naip_cover[k,]$new_id
          k_cell_id    <- naip_cover[k,]$cell_id
          k_naip_entit <- naip_cover[k,]$naip_entit
          buffer_path  <- state_subs_path[z]
          buffer       <- substr(state_subs_path[z], 32, 49)
          
          logger::log_info("\n\n\n{k} of {nrow(naip_cover)} NAIP Tiles\nCELL ID       -->  {k_cell_id}\nNAIP ENTIT    -->  {k_naip_entit}\nBuffer raster -->  {buffer}\n")
          
          
          # mask raster to NAIP tile
          r_mask <- 
            rsub %>%
            terra::crop(terra::vect(naip_cover[k,])) %>%
            terra::mask(terra::vect(naip_cover[k,]))
          
          aoi_crop <- clip_polygons(
            polygon = aoi,
            # polygon = terra::vect(naip_cover[k,]),
            rmask   = r_mask
          )
          
          if(is.null(aoi_crop)) {
            
            # logger::log_info("\n \nZero wetlands found in NAIP tile:\n \nCELL ID           -->  {k_cell_id}\nNAIP ENTIT        -->  {k_naip_entit}\nWetlands polygons -->  {nrow(aoi_crop)}\n")
            
            # Empty NAIP WETLANDS dataframe for 0 matches 
            naip_wetlands <- empty_wetlands(
              k_new_id     = k_new_id,
              k_cell_id    = k_cell_id, 
              k_naip_entit = k_naip_entit,
              state_abb    = state_abb,
              buffer       = buffer,   
              buffer_path  = buffer_path
            )
            
            # Empty AOI U dataframe for 0 matches 
            aoi_u         <- empty_aoi(
              k_new_id     = k_new_id,
              k_cell_id    = k_cell_id, 
              k_naip_entit = k_naip_entit,
              state_abb    = state_abb,
              buffer       = buffer,   
              buffer_path  = buffer_path
            )
            
            # naip_lst1[[k]]    <- aoi_u
            # naip_lst2[[k]]    <- naip_wetlands
            
          } else {
            
            logger::log_info("\n\n\n{nrow(aoi_crop)} wetlands found in NAIP tile:\n \nCELL ID           -->  {k_cell_id}\nNAIP ENTIT        -->  {k_naip_entit}\nWetlands polygons -->  {nrow(aoi_crop)}")
            
            # Identify which Wetland polygons have a majority 1 value from buffer raster 
            # union geometries and calculate total area
            
            aoi_u         <- mask_area(
              rmask     = r_mask,
              polygon   = aoi_crop,
              union     = TRUE
            )

            aoi_u <- 
              aoi_u %>% 
              dplyr::mutate(
                new_id      = k_new_id,
                cell_id     = k_cell_id,
                naip_entit  = k_naip_entit,
                state       = state_abb,
                buffer      = buffer,
                buffer_path = buffer_path
              ) %>% 
              dplyr::relocate(new_id, cell_id, naip_entit, total_wl_area, state, buffer, buffer_path) 
            
            logger::log_info("\n\nTotal wetland area  -->  {round(aoi_u$total_wl_area, 2)} m^2\n")
            
            # Individual wetlands in NAIP tile 
            naip_wetlands <- mask_area(
              rmask     = r_mask,
              polygon   = aoi_crop,
              union     = FALSE
            )
            
            naip_wetlands <- 
              naip_wetlands %>% 
              dplyr::mutate(
                new_id      = k_new_id,
                cell_id     = k_cell_id,
                naip_entit  = k_naip_entit,
                state       = state_abb,
                buffer      = buffer,
                buffer_path = buffer_path
              )
          }
          
          # Add all NAIP tiles to list within "k" NAIP tiles in "z" raster subset in "i" state 
          naip_lst1[[k]]    <- aoi_u
          naip_lst2[[k]]    <- naip_wetlands
        }
      }
    }
    
    # Add all NAIP tiles to list within "z" raster subset in "i" state  
    rsub_lst1[[z]]      <- dplyr::bind_rows(naip_lst1)
    rsub_lst2[[z]]      <- dplyr::bind_rows(naip_lst2)
  }
  
  # Bind rows for all NAIP tiles within "i" state subset 
  naip_wl_area  <- dplyr::bind_rows(rsub_lst1)
  naip_wl       <- dplyr::bind_rows(rsub_lst2)
  naip_skip     <- dplyr::bind_rows(skip_lst)
  
  # save NAIP wetlands area
  saveRDS(
    naip_wl_area,
    paste0(
      here::here("data","wetlands", "naip_area", state_abb, "/"),
      state_abb, "_naip_wetlands_area.rds"
    )
  )  
  
  # save individual NAIP wetlands 
  saveRDS(
    naip_wl,
    paste0(
      here::here("data","wetlands", "naip_area", state_abb, "/"),
      state_abb, "_naip_wetlands.rds"
    )
  )
  
  # save skipped NAIP tiles 
  saveRDS(
    naip_skip,
    paste0(
      here::here("data","wetlands", "naip_area", state_abb, "/"),
      state_abb, "_skip.rds"
    )
  )
  
  # Add all NAIP tiles to list within "i" state subset 
  # final_naip_lst1[[i]] <- dplyr::bind_rows(rsub_lst1)
  # final_naip_lst2[[i]] <- dplyr::bind_rows(rsub_lst2)
  # final_skip_lst[[i]]  <- dplyr::bind_rows(skip_lst)
  # 
}

# ***************************
# ---- Summarize results ----
# ***************************
rm(list = ls())

# CRS 
new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

# States of interest
states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]


# Path to buffer raster tiles subsetted by state
r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))


# NAIP Grid
grid <- 
  readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    new_id        = 1:n()
  ) %>%
  janitor::clean_names() %>% 
  dplyr::mutate(
    cell_id       = as.character(cell_id),
    naip_entit    = as.character(naip_entit)
  ) %>%
  dplyr::select(new_id, cell_id, naip_entit, primary_st) %>% 
  sf::st_transform(new_crs) %>% 
  dplyr::mutate(
    state = 
      case_when(
        primary_st == "Arizona"     ~ "AZ",   
        primary_st == "Colorado"    ~ "CO",   
        primary_st == "California"  ~ "CA",   
        primary_st == "Nevada"      ~ "NV",   
        primary_st == "New Mexico"  ~ "NM",   
        primary_st == "Utah"        ~ "UT",   
        primary_st == "Wyoming"     ~ "WY",   
        TRUE                        ~ primary_st
      )
  ) %>% 
  dplyr::relocate(new_id, cell_id, naip_entit, primary_st, state, geometry)

rm_final_paths <- !grepl(
  "final",
  list.files(here::here("data","wetlands",  "naip_area"), recursive	= T)
)

naip_area_path <- list.files(here::here("data","wetlands",  "naip_area"), recursive	= T)[rm_final_paths]

area_paths <-
  here::here("data","wetlands", "naip_area", 
             grep("area",
                  grep(
                    "_naip_wetlands", 
                    naip_area_path,
                    value = T
                    ), 
                  value = T))

wl_paths <-
  here::here("data","wetlands", "naip_area", 
                  grep(
                    "_naip_wetlands.rds|_naip_wetlands2.rds|_naip_wetlands3.rds", 
                    naip_area_path,
                    value = T))

skip_paths <-
  here::here("data","wetlands",  "naip_area",
             grep(
               "skip", 
               naip_area_path,
               value = T))


# i <- 1
# rm(i)
area_lst <- list()
skip_lst <- list()
wl_lst   <- list()

for (i in 1:length(area_paths)) {
  
  logger::log_info("\n\n{i} of {length(area_paths)}\nFilepath: {substr(area_paths[i], 70, 95)}")
  
  wl_area    <- readRDS(area_paths[i])
  wl         <- readRDS(wl_paths[i])
  skip_tiles <- readRDS(skip_paths[i])
  
  area_lst[[i]] <- wl_area
  wl_lst[[i]]   <- wl
  skip_lst[[i]] <- skip_tiles
}

# Path to simplified state wetland geometries
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))

area_lst <-
  area_lst %>%
  purrr::map(~ .x %>% mutate(across(starts_with("total_wl_area"), as.numeric)))

area_df <- dplyr::bind_rows(area_lst)

area_df <- area_df[!duplicated(area_df$cell_id), ] 

area_df <-
  area_df %>% 
  dplyr::mutate(total_wetland_area = round(total_wl_area, 2)) %>% 
  dplyr::select(cell_id, naip_entit, total_wetland_area)

wl_df   <- dplyr::bind_rows(wl_lst)

length(unique(area_df$cell_id))
area_df[!duplicated(area_df$cell_id), ] 
tmp <- wl_summary %>%
  filter(cell_id == 106557)

wl_summary %>% 
  group_by(cell_id)
wl_df[!duplicated(wl_df$wl_area), ] %>% 
  group_by(cell_id)

wl_summary <- 
  # wl_df[!duplicated(wl_df$wl_area), ] %>%
  wl_df %>% 
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
    dplyr::select(area_df, cell_id, naip_entit),
    by = "cell_id"
    ) %>% 
  dplyr::relocate(cell_id, naip_entit)

skip_df <- dplyr::bind_rows(skip_lst)

# save skipped NAIP tiles 
saveRDS(
  area_df,
  here::here("data","wetlands", "naip_area", "final", "naip_wetlands_area.rds")
)

# save skipped NAIP tiles 
saveRDS(
  wl_summary,
  here::here("data","wetlands", "naip_area", "final", "naip_wetlands.rds")
)
# save skipped NAIP tiles 
readr::write_csv(
  wl_summary,
  here::here("data","wetlands", "naip_area", "final", "naip_wetlands.csv")
)
# save skipped NAIP tiles 
saveRDS(
  skip_df,
  here::here("data","wetlands", "naip_area", "final", "skip_rasters.rds")
)

miss_tiles <- 
  grid %>% 
  filter(!cell_id %in% area_df$cell_id)
mapview(miss_tiles)
pth <- paste0(r_path[1], "/", list.files(r_path[1]))

aoi         <- terra::vect(readRDS(wl_geom_path[1]))[,c(1, 2, 6, 11, 12)]

aoi_valid <- 
  aoi %>% 
  sf::st_as_sf() %>% 
  sf::st_make_valid() %>% 
  st_cast("MULTIPOLYGON")

aoi <- 
  aoi_valid %>% 
  terra::vect()

r1 <- raster::raster(pth[1])
r2 <- raster::raster(pth[2])
r3 <- raster::raster(pth[3])
r4 <- raster::raster(pth[4])

r_bb <- 
  r3 %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  terra::vect()

crp <- 
  aoi %>% 
  terra::crop(r_bb) %>% 
  sf::st_as_sf() %>% 
  sf::st_cast("MULTIPOLYGON")

mapview(r3)  + crp + grid


# rm(r1, r2, r4)
mapview(r1) + r2 + r3 + r4 + tmp_rm
area_df <- dplyr::bind_rows(area_lst)
skip_df <- dplyr::bind_rows(skip_lst)

tmp_rm = grid %>% filter(!new_id %in% area_df$new_id)
mapview(tmp_rm, col.regions = "red") + grid
logger::log_info("\n\nInteresecting grid w/ bounding box")

# # Calculate area and tidy up
intersect_pct <-
  # missed_tiles %>%
  # dplyr::filter(state == "CA") %>%
  sub_miss %>%
  sf::st_intersection(shp) %>%
  dplyr::mutate(
    intersect_area = sf::st_area(.)
  ) %>%
  dplyr::select(cell_id, intersect_area) %>%
  sf::st_drop_geometry()

logger::log_info("\n\nClipping grid elements\nTolerance: {coverage_pct*100}% area within bounding box")

# # Create a fresh area variable for counties
aoi_grid <-
  sub_miss %>%
  dplyr::mutate(naip_tile_area = sf::st_area(sub_miss)) %>%
  dplyr::left_join(
    intersect_pct,
    by = "cell_id"
  ) %>%
  dplyr::mutate(
    coverage = as.numeric(intersect_area/naip_tile_area)
  ) %>%
  dplyr::relocate(
    new_id, cell_id, naip_entit, naip_tile_area,
    intersect_area, coverage, geometry) %>%
  dplyr::filter(coverage >= coverage_pct)

logger::log_info("\n\nGrid elements after clipping: {nrow(aoi_grid)}")

# path to state raster subset
subset_path <- grep(
  "CA",
  r_path,
  value = T
)

# logger::log_info("\n\nExtracting raster values...\nState: {state_abb}")
 raster::raster(state_subs_path[1])


# full file path to state raster subsets
state_subs_path <- paste0(subset_path, "/", list.files(subset_path)) 

r1 <- raster::raster(state_subs_path[1])
# r2 <- raster::raster(state_subs_path[2])
# r3 <- raster::raster(state_subs_path[3])
rsub <- terra::rast(state_subs_path[1])
# rsub2 <- raster::raster(state_subs_path[z])

# aggregate to speed up coverage pct check
r_agg <-
  rsub %>%
  terra::aggregate(fact = 6, fun = "max") %>%
  raster::raster()

# mapview(naip_cover, color = "red") + aoi_grid + rsub + r_agg

# raster tile bounding box
r_bb <- 
  r_agg %>%
  # rsub %>%
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf()

# filter naip tiles to those within the raster tiles bounding box
naip_tiles <-
  aoi_grid %>% 
  sf::st_filter(r_bb)

naip_tiles <- 
  naip_tiles %>% 
  dplyr::mutate(tmp_id = 1:n()) 


# Percent of polygon covered by raster cells
r_pct_cover <- exactextractr::exact_extract(
  r_agg,
  # rsub,
  naip_tiles,
  coverage_area = T,
  fun           = "count",
  force_df      = T
) %>%
  dplyr::mutate(tmp_id = 1:dplyr::n()) %>% 
  setNames(c("cover_area", "tmp_id"))

# base::names(r_pct_cover) <- c("cover_area", "tmp_id")
# ---- NAIP COVER ----

# filter NAIP tiles that are not atleast 50% covered by raster tile
naip_cover <-
  naip_tiles %>%
  dplyr::left_join(
    r_pct_cover,
    by = "tmp_id"
  ) %>% # dplyr::left_join(r_pct_cover_agg, by = "tmp_id") %>%
  dplyr::mutate(
    naip_tile_area = as.numeric(naip_tile_area),    # naip_tile_area  = round(as.numeric(naip_tile_area), 5),
    cover_pct      = cover_area/naip_tile_area      # cover_pct       = round(cover_area/naip_tile_area, 5)
  ) %>%
  dplyr::relocate(
    new_id, cell_id, naip_entit, tmp_id, naip_tile_area,
    intersect_area, coverage, cover_area, cover_pct, geometry
  )  %>%
  dplyr::filter(cover_pct >= 0.50) 
k = 1

# mask raster to NAIP tile
r_mask <- 
  rsub %>%
  terra::crop(terra::vect(naip_cover[k,])) %>%
  terra::mask(terra::vect(naip_cover[k,]))

r2 <- raster::raster(r_mask)
mapview(r2) + naip_cover[k,]

aoi_crop <- clip_polygons(
  polygon = ca_aoi,
  # polygon = terra::vect(naip_cover[k,]),
  rmask   = r_mask
)
aoi_crop2 <- 
  aoi_crop %>% 
  st_as_sf() %>% 
  st_cast("MULTIPOLYGON")
mapview(naip_cover, col.regions = "red")  +r2 + aoi_crop2 + r1
# mapview(naip_cover, col.regions = "red") + r1 +sub_miss + r2 + r3


aoi_u         <- mask_area(
  rmask = r_mask,
  polygon = aoi_crop,
  union = TRUE
)

logger::log_info("\n\nTotal wetland area  -->  {round(aoi_u$total_wl_area, 2)} m^2\n")

# Individual wetlands in NAIP tile 
naip_wetlands <- mask_area(
  rmask = r_mask,
  aoi   = aoi_crop,
  union = FALSE
)
# mapview(r1) + r2 +r3 +r4 + sub_miss + r5 + r6 + r7
mapview(sub_miss, col.regions = "red") + missed_tiles 

r2_path <- here::here("data", "wetlands", "raster_subset",  "CA",
                      list.files(r_path))
ca_r <- raster::raster(r2_path[41])

ca_aoi <- terra::vect(readRDS(wl_geom_path[2]))

r_bb <- 
  ca_r %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf()

ca_crop <- 
  ca_aoi %>% 
  terra::crop(r_bb) %>% 
  st_as_sf() %>% 
  st_cast("MULTIPOLYGON") %>% 
  dplyr::select(1, 2, 3, IMAGE_YR, IMAGE_DATE, geometry)

mapview::mapview(tmp_rm) + ca_r + ca_crop

tile_new_id <- 11765

tmp_tile <- 
  tmp_rm %>% 
  filter(new_id == tile_new_id)

# union geometries and calculate total area
# mask raster to NAIP tile
r_mask <- 
  ca_r %>%
  terra::rast() %>% 
  terra::crop(terra::vect(tmp_tile)) %>%
  terra::mask(terra::vect(tmp_tile))

r2 <- raster::raster(r_mask)
mapview(r2) + tmp_tile

aoi_crop <- clip_polygons(
  polygon = terra::vect(ca_crop),
  # polygon = terra::vect(naip_cover[k,]),
  rmask   = r_mask
)


aoi_u         <- mask_area(
  rmask = r_mask,
  aoi   = aoi_crop,
  union = TRUE
)

aoi_crop <- 
  aoi_crop %>% 
  st_cast("MULTIPOLYGON")  
mapview(aoi_u, col.regions = "red") + aoi_crop + r2 + tmp_tile + tmp_rm
logger::log_info("\n\nTotal wetland area  -->  {round(aoi_u$total_wl_area, 2)} m^2\n")

# Individual wetlands in NAIP tile 
naip_wetlands <- mask_area(
  rmask = r_mask,
  aoi   = aoi_crop,
  union = FALSE
)


az_aoi <- terra::vect(readRDS(wl_geom_path[1]))
mapview::mapview(tmp_rm) + ca_r


# *****************************************************
# *****************************************************

# doParallel::stopImplicitCluster()
# parallel::stopCluster(cl)

# NAIP tile wetland area 
# final_naip_area <- dplyr::bind_rows(final_naip_lst1)
# 
# # Specific wetlands found in NAIP tile
# final_naip_info <- dplyr::bind_rows(final_naip_lst2) 
# 
# # Skipped rasters due to zero NAIP tile coverage
# skipped_rast    <- dplyr::bind_rows(final_skip_lst) 
# wl_final <- bind_cols(poly_simple_sf, wl_stats)

# *******************************











