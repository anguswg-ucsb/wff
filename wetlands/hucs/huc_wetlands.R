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

# Path to simplified state wetland geometries
# wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))
wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))[c(3:7)]

# Path to buffer raster tiles subsetted by state
r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))

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

# HUC12s
huc12 <- sf::read_sf("data/huc12/WBDHU12_CO_CRB.shp") %>% 
  sf::st_transform(4326) %>%
  sf::st_transform(new_crs) %>% 
  sf::st_cast("MULTIPOLYGON") %>% 
  dplyr::mutate(
    new_id = 1:n()
  ) %>% 
  dplyr::select(new_id, tnmid, areaacres,areasqkm,states,huc12, geometry)

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

# final_lst1 <- list()
# final_lst2 <- list()
# final_lst3 <- list()

 # i <- 1
# tmp <- huc12 %>% 
#   filter(huc12 %in% c(
#     101900050304, 101900050303, 101900050203, 101900060605, 101900070209,
#                       150502020306, 150802000502, 150502020305,150802000501, 150502020501))
# r <- raster("data/wetlands/raster_subset/WY/WY_buff_40105.tif")
#  mapview(tmp) + r
# i <- 1

# ---- i loop ----
for (i in 1:length(wl_geom_path)) {
  
    # State abbreviation
    state_abb   <- substr(wl_geom_path[i], 28, 29)
    
    # number of rasters iterated through/processed
    nrasters <-
      nraster_files %>%
      dplyr::filter(state == state_abb)
    
    logger::log_info("\n\nLoading {state_abb} wetlands...\nState         -->  {state_abb}\n{state_abb} rasters    -->  {nrasters$total_files} of {nrasters$total_raster_files}  ({round(nrasters$total_files/nrasters$total_raster_files, 2)*100} % of total)\nTotal rasters -->  {nrasters$total_files_csum} of {nrasters$total_raster_files}  ({round(nrasters$total_files_csum/nrasters$total_raster_files, 2)*100} % completed)")
    
    # State wetlands 
    # aoi         <- terra::vect(readRDS(wl_geom_path[i]))
    aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c("ATTRIBUTE", "WETLAND_TYPE", "PROJECT_NAME","IMAGE_YR", "IMAGE_DATE")]
    # aoi         <- terra::vect(readRDS(wl_geom_path[i]))[,c(1, 2, 6, 11, 12)]
    
    # path to state raster subset
    subset_path <- grep(
      state_abb,
      r_path,
      value = T
    )
    
    # full file path to state raster subsets
    state_subs_path <- paste0(subset_path, "/", list.files(subset_path))
    # state_subs_path <- paste0(subset_path, "/", list.files(subset_path))[1:2]
    
    #state_subs_path<-state_subs_path[!grepl("AZ_buff_37112.tif|AZ_buff_37113.tif|AZ_buff_37114|AZ_buff_37111|AZ_buff_37110",state_subs_path)]
    
    
    # State of interest polygon 
    shp <- 
      state_shp %>% 
      sf::st_as_sf() %>% 
      dplyr::filter(stusps == state_abb) # %>% terra::vect()
    
    
    # Remove grid tiles that are less than 50% covered in state
    # aoi_huc <- trim_hucs(  hucs  = huc12,   crop_shp = shp,  coverage_pct  = 0.25 )
    # aoi_huc2 <- sf::st_filter(huc12, shp) #sf::st_filter(shp, .predicate = st_within)
    
  
    rsub_lst1 <- list()
    rsub_lst2 <- list()
    skip_lst  <- list()
  
    # z <- 1
  
    # ---- z loop ----
    for (z in 1:length(state_subs_path)) {
        
        logger::log_info("\n\n{z} of {length(state_subs_path)} raster tiles\nState        --> {state_abb}\nRaster path  --> {state_subs_path[z]}")
        
        rsub    <- terra::rast(state_subs_path[z])
        # rsub2   <- raster::raster(state_subs_path[z])
        
        # aggregate to speed up coverage pct check
        # r_agg <- rsub %>%terra::aggregate(fact = 6, fun = "max") %>% raster::raster()
        
        r_bb <- 
          rsub %>%  # r_agg %>%
          sf::st_bbox() %>% 
          sf::st_as_sfc() %>%
          sf::st_as_sf()
 
        # Subset HUC12s within raster bounding box
        sub_huc <- 
          huc12 %>%
          sf::st_filter(r_bb) # sf::st_filter(r_bb, .predicate = st_within)
        
        logger::log_info("Raster covering {nrow(sub_huc)} HUC12s")
        
        if(nrow(sub_huc) == 0) {
          
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
          
    
            huc_lst1 <- list()
            huc_lst2 <- list()
            
            # k = 3
            
            for (k in 1:nrow(sub_huc)) {
                # k <- 3
                k_huc12      <- sub_huc$huc12[k]
                buffer_path  <- state_subs_path[z]
                buffer       <- substr(state_subs_path[z], 32, 49)
                
          
                logger::log_info("\n\n\n--- {z} of {length(state_subs_path)} {state_abb} rasters ---\n---- {k} of {nrow(sub_huc)} HUC12s ---- \n--------------------------\n\nHUC12 ID  --->  {k_huc12}")
                 # mask raster to NAIP tile
                r_mask <- 
                  rsub %>%
                  terra::crop(terra::vect(sub_huc[k,])) %>%
                  terra::mask(terra::vect(sub_huc[k,]), updatevalue = 2)
            
                # plot(sub_huc$geometry, add = F)
                # plot(r_mask, add = T)
                # plot(r_bb$x, lwd = 5, add = T)
                
                wl_crop <- clip_hucs(
                  polygon = aoi,
                  # polygon = terra::vect(naip_cover[k,]),
                  rmask   = r_mask
                )
          
                if (is.null(wl_crop)) {
                  
                  logger::log_info("\n\n\nZERO WETLANDS FOUND\nHUC12: {k_huc12}")
                  
                  # Empty HUC WETLANDS dataframe for 0 matches 
                  huc_wl <- 
                    tibble::tibble(
                      ATTRIBUTE     = NA,
                      WETLAND_TYPE  = NA,
                      PROJECT_NAME  = NA,
                      IMAGE_YR      = NA,
                      IMAGE_DATE    = NA,
                      tmp_id        = NA,
                      majority      = NA,
                      wl_area       = NA,
                      huc12         = k_huc12,
                      state         = state_abb,
                      buffer        = buffer,   
                      buffer_path   = buffer_path
                    )
                  
                  # Empty AOI U dataframe for 0 matches 
                  huc_area <- 
                    tibble::tibble(
                      huc12         = k_huc12,
                      total_wl_area = NA,
                      state         = state_abb,
                      buffer        = buffer,   
                      buffer_path   = buffer_path
                    )
          
          
                  
                } else {   
                  
                logger::log_info("\n\n\nHUC12 wetlands polygons -->  {nrow(wl_crop)}")
                
                # Identify which Wetland polygons have a majority 1 value from buffer raster 
                # union geometries and calculate total area
                  
                  huc_area <- mask_huc_area(
                    rmask     = r_mask,
                    polygon   = wl_crop,
                    union     = TRUE,
                    remove_na = FALSE
                  )
                  
                  huc_area <-
                    huc_area %>% 
                    dplyr::mutate(
                      huc12       = k_huc12,
                      state       = state_abb,
                      buffer      = buffer,
                      buffer_path = buffer_path
                    ) %>% 
                    dplyr::relocate(huc12, total_wl_area, state, buffer, buffer_path) 
                  
                  logger::log_info("\n\nTotal wetland area  -->  {round(huc_area$total_wl_area, 2)} m^2\n")
                  
                  huc_wl <- mask_huc_area(
                    rmask     = r_mask,
                    polygon   = wl_crop,
                    union     = FALSE,
                    remove_na = FALSE
                  )
                  
                  huc_wl <- 
                    huc_wl %>% 
                    dplyr::mutate(
                      huc12       = k_huc12,
                      state       = state_abb,
                      buffer      = buffer,
                      buffer_path = buffer_path
                    ) %>% 
                    dplyr::relocate(ATTRIBUTE, WETLAND_TYPE, PROJECT_NAME, IMAGE_YR, IMAGE_DATE,
                                    tmp_id, majority, wl_area, huc12, state, buffer, buffer_path) 
           
                
           
              
                }
              # Add all NAIP tiles to list within "k" NAIP tiles in "z" raster subset in "i" state 
              huc_lst1[[k]]    <- huc_area
              huc_lst2[[k]]    <- huc_wl
             }
          }
        # Add all NAIP tiles to list within "z" raster subset in "i" state  
        rsub_lst1[[z]]      <- dplyr::bind_rows(huc_lst1)
        rsub_lst2[[z]]      <- dplyr::bind_rows(huc_lst2)
        
      
      }
    
  # Bind rows for all NAIP tiles within "i" state subset 
  huc_wl_area  <- dplyr::bind_rows(rsub_lst1)
  huc_wl       <- dplyr::bind_rows(rsub_lst2)
  huc_skip     <- dplyr::bind_rows(skip_lst)
    
  # final_lst1[[i]] <- dplyr::bind_rows(rsub_lst1)
  # final_lst2[[i]] <- dplyr::bind_rows(rsub_lst2)
  # final_lst3[[i]] <- dplyr::bind_rows(skip_lst)
 
  # # save huc wetlands area
  saveRDS(
    huc_wl_area,
    paste0(
      here::here("data","wetlands", "huc_area", state_abb, "/"),
      state_abb, "_huc_wetlands_area.rds"
    )
  )

  # save individual huc wetlands
  saveRDS(
    huc_wl,
    paste0(
      here::here("data","wetlands", "huc_area", state_abb, "/"),
      state_abb, "_huc_wetlands.rds"
    )
  )

  # save skipped huc rasters
  saveRDS(
    huc_skip,
    paste0(
      here::here("data","wetlands", "huc_area", state_abb, "/"),
      state_abb, "_huc_skip.rds"
    )
  )
  
  
# ***************************
# ---- Summarize outputs ----
# ***************************
base_path <- here::here("data","wetlands", "huc_area")
huc_wl_path <-   paste0(base_path, "/", list.files(base_path, recursive  = T))

area_paths <-grep(
  "_huc_wetlands_area", 
  huc_wl_path,
  value = T
  )
            
wl_paths <- huc_wl_path[!grepl("_huc_wetlands_area|skip", huc_wl_path)]

skip_paths <-grep(
  "skip", 
  huc_wl_path,
  value = T
)

# huc_wl   <- readRDS(huc_wl_path[2])
# huc_area <- readRDS(huc_wl_path[3])

# rm(i)
area_lst <- list()
skip_lst <- list()
wl_lst   <- list()
# i <- 1
rm(i)
for (i in 1:length(area_paths)) {
  
  logger::log_info("\n\n{i} of {length(area_paths)}\nFilepath: {substr(area_paths[i], 70, 95)}")
  
  wl_area    <- readRDS(area_paths[i])
  wl         <- readRDS(wl_paths[i])
  skip_tiles <- readRDS(skip_paths[i])
  
  area_lst[[i]] <- wl_area
  wl_lst[[i]]   <- wl
  skip_lst[[i]] <- skip_tiles
}
# summarized total area dataframe 
skip_df <- dplyr::bind_rows(skip_lst)

# summarized total area dataframe 
area_df <- dplyr::bind_rows(area_lst)

# summarized total area dataframe 
area_df <-
  area_df %>% 
  dplyr::mutate(total_wetland_area = round(total_wl_area, 2)) %>% 
  dplyr::select(huc12, state, total_wetland_area) %>%
  distinct(huc12, state, total_wetland_area)               # Remove duplicates

# Remove duplicates
# area_df <- area_df[!duplicated(area_df$huc12), ]




# Individual Wetland dataframe 
wl_df   <- dplyr::bind_rows(wl_lst)

# length(unique(wl_df$huc12))

# Individual Wetland summary dataframe 
wl_summary <- 
  wl_df[!duplicated(wl_df$wl_area), ] %>%
  # wl_df %>% distinct(huc12, ATTRIBUTE,IMAGE_YR, majority, wl_area) %>%
  dplyr::mutate(
    wl_area = case_when(
      majority == 1 ~ wl_area,
      majority == 0 ~ 0,
    )
  ) %>% 
  dplyr::group_by(huc12, ATTRIBUTE, IMAGE_YR) %>% 
  dplyr::summarize(
    wetland_area = round(sum(wl_area, na.rm = T), 3)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::relocate(huc12)

length(unique(wl_df$huc12))
length(unique(wl_summary$huc12))
length(unique(area_df$huc12))

# save skipped NAIP tiles 
saveRDS(
  area_df,
  here::here("data","wetlands", "huc_area", "final", "huc_wetlands_area.rds")
)

# save skipped NAIP tiles 
saveRDS(
  wl_summary,
  here::here("data","wetlands", "huc_area", "final", "huc_wetlands.rds")
)

# save skipped NAIP tiles 
readr::write_csv(
  wl_summary,
  here::here("data","wetlands", "huc_area", "final", "huc_wetlands.csv")
)
# *************************************

wlsm_tmp <- 
  wl_summary %>%
  filter(huc12 == 101900010507)


sum(wlsm_tmp$wetland_area)

27339444/sum(wlsm_tmp$wetland_area)
sm <- 47655.444+188737.090+5250.123+61375.575+281.271+8394.250+16857.816+719

sm/637790.5


20954.054+10662.919+914.512+





final1 <- dplyr::bind_rows(final_lst1)
final2 <- dplyr::bind_rows(final_lst2)
final3 <- dplyr::bind_rows(final_lst3)

final_lst2[[i]] <- dplyr::bind_rows(rsub_lst2)
final_lst3[[i]] <- dplyr::bind_rows(skip_lst)
    # %>%
    #   dplyr::mutate(tmp_id = 1:n()) %>%
    #   terra::vect()
    # 
    # r_disagg <-
    #   r_mask %>%
    #   terra::disagg(fact = 6)
    # rm(r_disagg)
    # rm1
    # rm(xx, rm1, xx_touch, wl_crop2)
    # xx <-
    #   wl_crop %>%
    #   terra::rasterize(r_disagg, "tmp_id", touches=F) %>%
    #   raster::raster()
    # 
    # xx_touch <-
    #   wl_crop %>%
    #   terra::rasterize(r_disagg, "tmp_id", touches=T) %>%
    #   raster::raster()
    # wl_crop2 <- 
    #   wl_crop %>% 
    #   st_as_sf() %>%
    #   st_cast("MULTIPOLYGON")
    # 
    # mapview(wl_crop2) + xx + xx_touch
    
    plot(xx_touch)
    huc_area <- mask_area(
      rmask     = r_mask,
      polygon   = aoi_crop,
      remove_na = TRUE
    )
    
    # mapview(rm1) + sub_huc[k,] + rsub2
    # filter naip tiles to those within the raster tiles bounding box
    # sub_huc2 <-
    #   aoi_huc %>% 
    #   sf::st_filter(r_bb)
    # mapview(sub_huc, col.regions="red") + r_bb + sub_huc2
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
 