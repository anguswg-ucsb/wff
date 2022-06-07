extract_join <- function(
  polygon,
  rmask, 
  extract_fun = "majority", 
  join_by     = "tmp_id"
  ) {
  
  # # Crop buffer raster to state grid 
  extr <- exactextractr::exact_extract(
    rmask,
    polygon,
    # r_mask,
    # aoi_crop,
    fun           = extract_fun,
    default_value = 2,
    force_df      = T
  ) %>%
    dplyr::mutate(tmp_id = 1:dplyr::n()) %>% 
    setNames(c(extract_fun, join_by))

  # Identify which Wetland polygons have a majority 1 value from buffer raster 
  polygon_area <- 
    polygon %>% 
    # aoi_crop %>%
    dplyr::left_join(
      extr, 
      by = join_by
      # by = "tmp_id"
    ) %>% 
    dplyr::mutate(
      wl_area = round(as.numeric(sf::st_area(.)), 4)
    ) %>% 
    sf::st_cast("MULTIPOLYGON")
  
  return(polygon_area)
}

extract_area <- function(polygon) {
  
  logger::log_info("\n\nCalculating total wetland area...")
  
  # union geometries and calculate total area
  polygon_u <- 
    polygon %>% 
    dplyr::group_by(ATTRIBUTE, IMAGE_YR) %>% 
    sf::st_make_valid() %>%
    dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      total_wl_area = as.numeric(sf::st_area(.))
    ) %>%
    sf::st_drop_geometry() %>%  
    tibble::tibble() %>% 
    replace(is.na(.), 0) 
  
  return(polygon_u)
}
mask_huc_area <- function(
  rmask, 
  polygon,
  union     = TRUE,
  remove_na = FALSE
) {
  
  # rmask   <- r_mask
  # polygon <- wl_crop
  # polygon <- aoi_crop
  
  if(remove_na == TRUE) {
    
    logger::log_info("\n\nExtracting raster values...\nOmitting NA values...")
    
    extr <- exactextractr::exact_extract(
      rmask,
      # r_mask,
      polygon,
      fun      = "majority",
      force_df = T
    ) %>%
      dplyr::mutate(tmp_id = 1:dplyr::n()) %>% 
      na.omit()
    
  } else {
    
    logger::log_info("\n\nExtracting raster values...")
    
    extr <- exactextractr::exact_extract(
      rmask,
      # r_mask,
      polygon,
      fun      = "majority",
      force_df = T
    ) %>%
      dplyr::mutate(tmp_id = 1:dplyr::n())
  }
  
  # Identify which Wetland polygons have a majority 1 value from buffer raster 
  polygon_area <- 
    polygon %>%
    # dplyr::inner_join(
    dplyr::left_join(
      extr, 
      by = "tmp_id"
    ) %>% 
    dplyr::mutate(
      wl_area = round(as.numeric(sf::st_area(.)), 4)
      # wl_area = case_when(majority == 1 ~ wl_area,  majority == 0 ~ 0 )
    ) %>% 
    sf::st_cast("MULTIPOLYGON") 

  # sum(polygon_area$wl_area)

  polygon_calc <- 
    polygon_area %>% 
    dplyr::filter(majority == 1)       # filter areas over 50% in buffer raster area (values of 1)
  
  # r1 <- raster(rmask) mapview(polygon_calc, col.regions = "red")  + r1 + polygon_area
  # polygon_calc %>% summarise(total_area = sum(st_area(.)))
  # tmp_poly<-sf::st_cast(polygon, "MULTIPOLYGON")tmp_rm<-raster::raster(r_mask)
  # tmp_maj<-dplyr::filter(polygon_area, majority == 1) mapview(polygon_area, col.regions = "red") + tmp_rm  + tmp_maj + polygon_u
  
  if (union == TRUE) {
    
    logger::log_info("\n\nCalculating total wetland area...\n")
    if (nrow(polygon_calc) == 0) {

      polygon_u <-
        tibble::tibble(
          total_wl_area = 0
        )

    } else {
   
      # union geometries and calculate total area
      polygon_u <- 
        polygon_calc %>% 
        # polygon_area %>% 
        sf::st_make_valid() %>% 
        dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
        dplyr::mutate(
          total_wl_area = round(as.numeric(sf::st_area(.)), 4)
        ) %>%
        sf::st_drop_geometry() %>%  
        tibble::tibble() %>% 
        replace(is.na(.), 0) 
      
    }
    
    return(polygon_u)
    
  } else if(union == FALSE) {
    
    logger::log_info("\n\nCalculating individual wetland areas...\n")
    
    # Individual wetlands in NAIP tile 
    huc_wetlands <-
      polygon_area %>% 
      sf::st_drop_geometry() %>% 
      dplyr::filter(majority != 2) %>%   
      tibble::tibble()
    
    return(huc_wetlands)
    
  }
}

clip_hucs <- function(
  polygon, 
  rmask
) {
  # rmask <- r_mask
  
  # polygon <- aoi
  # Bounding box of subsetted buffer raster covering NAIP tile 
  # bb <-
  #   rmask %>%
  #   # r_mask %>% 
  #   sf::st_bbox() %>% 
  #   sf::st_as_sfc() %>%
  #   sf::st_as_sf() %>% 
  #   terra::vect()
  
  logger::log_info("\n\nLocating wetlands within polygon...")
  
  # Crop wetland polygons to NAIP tile area
  polygon_crop <- 
    polygon %>% # aoi_t %>%
    # aoi %>%
    terra::crop(
      terra::ext(
        rmask 
      )
    ) %>% 
    sf::st_as_sf() 
  # %>% 
  #   sf::st_cast("MULTIPOLYGON")
  # mapview(polygon_crop) + rsub2 + rm1
  # %>% dplyr::mutate(tmp_id = 1:n())
  
  if(nrow(polygon_crop) > 0) {
    
    # logger::log_info("\n\n\n{nrow(polygon_crop)} wetlands found in NAIP tile:\n \nCELL ID           -->  {k_cell_id}\nNAIP ENTIT        -->  {k_naip_entit}\nWetlands polygons -->  {nrow(polygon_crop)}")
    
    polygon_crop <-
      polygon_crop %>%
      dplyr::mutate(tmp_id = 1:dplyr::n())
    
    return(polygon_crop)
    
  } else {
    
    
    # logger::log_info("\n\n\nZERO WETLANDS FOUND\n\nCELL ID           -->  {k_cell_id}\nNAIP ENTIT        -->  {k_naip_entit}\nWetlands polygons -->  {nrow(polygon_crop)}\n\n --> Returning NULL value")
    
    nval <- NULL
    
    return(nval)
    
  }
  
}
trim_hucs <- function(
  hucs, 
  crop_shp, 
  coverage_pct = 0.50
) {
  # bb   <- shp
  # hucs <- aoi_huc

  logger::log_info("\n\n\nInteresecting polygons w/ shape... \nTolerance: {coverage_pct*100}%")

  # # Calculate area and tidy up
  intersect_pct <-
    hucs %>%
    # aoi_huc %>% 
    sf::st_filter(crop_shp) %>%
    sf::st_intersection(crop_shp) %>%
    dplyr::mutate(
      intersect_area = round(as.numeric(sf::st_area(.)), 3)
    ) %>%
    dplyr::select(new_id, intersect_area) %>%
    sf::st_drop_geometry()
  
  
  # # Create a fresh area variable for counties
  huc_pct <-
    hucs %>% 
    # aoi_huc %>%
    dplyr::mutate(huc12_area = round(as.numeric(sf::st_area(hucs)), 3)) %>%
    dplyr::left_join(
      intersect_pct,
      by = "new_id"
    ) %>%
    dplyr::mutate(
      intersect_pct = round(as.numeric(intersect_area/huc12_area), 5)
    ) %>%
    dplyr::select(
      new_id, huc12,  huc12_area, intersect_area, intersect_pct, geometry) %>%
    dplyr::filter(intersect_pct >= coverage_pct)
  
  logger::log_info("\n\n\n# Polygons after clipping  --->  {nrow(huc_pct)}")
  
  return(huc_pct)
  
}

clip_polygons <- function(
                      polygon, 
                      rmask
                      ) {
  
  logger::log_info("\n\nCropping AOI polygons to raster extent")
  
  # Crop wetland polygons to NAIP tile area
  polygon_crop <- 
    polygon %>% 
    # aoi %>% 
    terra::crop(
      terra::ext(
        rmask
        # r_mask
      )
    ) %>% 
    sf::st_as_sf() 
  
  if(nrow(polygon_crop) > 0) {
    
    logger::log_info("\n\n{nrow(polygon_crop)} AOI polygons in raster extent")
    
    polygon_crop <-
      polygon_crop %>%
      dplyr::mutate(tmp_id = 1:dplyr::n())
    
    return(polygon_crop)
    
  } else {
    
    logger::log_info("\n\n0 AOI polygons raster extent\nreturning NULL")
    
    nval <- NULL
    
    return(nval)
    
  }
    
}

mask_area <- function(
  rmask, 
  polygon,
  union     = TRUE,
  remove_na = FALSE
  ) {
  
    # rmask   <- r_mask
    # polygon <- wl_crop
    # polygon <- aoi_crop

    if(remove_na == TRUE) {
      
      logger::log_info("\n\nExtracting raster values...\nOmitting NA values...")
      
      extr <- exactextractr::exact_extract(
        rmask,
        # r_mask,
        polygon,
        fun      = "majority",
        force_df = T
      ) %>%
        dplyr::mutate(tmp_id = 1:dplyr::n()) %>% 
        na.omit()
      
    } else {
      
      logger::log_info("\n\nExtracting raster values...")
      
      extr <- exactextractr::exact_extract(
        rmask,
        # r_mask,
        polygon,
        fun      = "majority",
        force_df = T
      ) %>%
        dplyr::mutate(tmp_id = 1:dplyr::n())
    }
  
    # Identify which Wetland polygons have a majority 1 value from buffer raster 
    polygon_area <- 
      polygon %>%
      # aoi_crop %>% 
      dplyr::left_join(
        extr, 
        by = "tmp_id"
      ) %>% 
      dplyr::mutate(
        wl_area = round(as.numeric(sf::st_area(.)), 4)
      ) %>% 
      sf::st_cast("MULTIPOLYGON") 
    
    polygon_calc <- 
      polygon_area %>% 
      dplyr::filter(majority == 1)       # filter areas over 50% in buffer raster area (values of 1)
    

    # tmp_poly <- sf::st_cast(polygon, "MULTIPOLYGON") 
    # tmp_rm <- raster::raster(r_mask) tmp_maj <-  dplyr::filter(polygon_area, majority == 1)
    # mapview(polygon_area, col.regions = "red") + tmp_rm  + tmp_maj + polygon_u
    # mapview(tmp_maj, col.regions = "red") + tmp_rm   + polygon_u
    # polygon_calc %>% summarise(total_area = sum(st_area(.)))

    if (union == TRUE) {
      
      logger::log_info("\n\nCalculating total NAIP wetland area...\n")
      if (nrow(polygon_calc) == 0) {
        
        polygon_u <-
          tibble::tibble(
            total_wl_area = units::as_units(0, "m^2")
            )

      } else {
        
        # union geometries and calculate total area
        polygon_u <- 
          polygon_calc %>% 
          sf::st_make_valid() %>% 
          dplyr::summarize(geometry = sf::st_union(geometry)) %>% 
          dplyr::mutate(
            total_wl_area = sf::st_area(.)
          ) %>%
          sf::st_drop_geometry() %>%  
          tibble::tibble() %>% 
          replace(is.na(.), 0) %>% 
          dplyr::mutate(
            total_wl_area =units::as_units(total_wl_area)
            )

      }
      
      return(polygon_u)
    
    } else if(union == FALSE) {
      
      logger::log_info("\n\nCalculating individual NAIP tile wetland areas...\n")
      # Individual wetlands in NAIP tile 
      naip_wetlands <-
        polygon_area %>% 
        sf::st_drop_geometry() %>% 
        tibble::tibble()
      
      return(naip_wetlands)
      
    }
}

trim_tiles <- function(
  grid_polygon, 
  bb, 
  coverage_pct = 0.50
  ) {
  
  logger::log_info("\n\nInteresecting grid w/ bounding box")
  
  # # Calculate area and tidy up
  intersect_pct <-
    grid_polygon %>%
    sf::st_filter(bb) %>%
    sf::st_intersection(bb) %>%
    dplyr::mutate(
      intersect_area = sf::st_area(.)
    ) %>%
    dplyr::select(cell_id, intersect_area) %>%
    sf::st_drop_geometry()

  logger::log_info("\n\nClipping grid elements\nTolerance: {coverage_pct*100}% area within bounding box")
  
  # # Create a fresh area variable for counties
  aoi_grid <-
    grid_polygon %>%
    dplyr::mutate(naip_tile_area = sf::st_area(grid_polygon)) %>%
    # dplyr::mutate(naip_tile_area = sf::st_area(aoi_grid)) %>%
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
  
  return(aoi_grid)
  
}
empty_aoi <- function(k_new_id, k_cell_id, k_naip_entit, state_abb, buffer, buffer_path) {
  
  aoi_df <- tibble::tibble(
    new_id        = k_new_id,
    cell_id       = k_cell_id,
    naip_entit    = k_naip_entit,
    state         = state_abb,
    total_wl_area = 0,
    ATTRIBUTE     = "no_wetlands",
    IMAGE_YR      = as.integer(10000),
    buffer        = buffer,
    buffer_path   = buffer_path
  )
  # dplyr::select(new_id, cell_id, naip_entit, state, total_wl_area,
  #               ATTRIBUTE, IMAGE_YR, buffer, buffer_path)
  return(aoi_df)
}

# empty_aoi <- function(k_new_id, k_cell_id, k_naip_entit, state_abb, buffer, buffer_path) {
#   
#   aoi_df <- tibble::tibble(
#               new_id        = k_new_id,
#               cell_id       = k_cell_id,
#               naip_entit    = k_naip_entit,
#               total_wl_area = NA,
#               state         = state_abb,
#               buffer        = buffer,
#               buffer_path   = buffer_path
#             )
#   return(aoi_df)
# }

empty_wetlands <- function(k_new_id, k_cell_id, k_naip_entit, state_abb, buffer, buffer_path) {
  naip_wetlands <- tibble::tibble(
    ATTRIBUTE     = "no_wetlands",
    WETLAND_TYPE  = "no_wetlands",
    PROJECT_NAME  = "no_wetlands",
    IMAGE_YR      = as.integer(10000),
    IMAGE_DATE    = "no_wetlands",
    tmp_id        = as.integer(10000),
    majority      = 3,
    wl_area       = 0,
    new_id        = k_new_id,
    cell_id       = k_cell_id,
    naip_entit    = k_naip_entit,
    state         = state_abb,
    buffer        = buffer,
    buffer_path   = buffer_path
  )
  return(naip_wetlands)

}



# loads climateR gridmet data for desired paramter rasters, masks to the specified basin, seperates data by district, and aggregates to a tidy tibble
get_gridmet <- function(grid, 
                        # tile, 
                        param, start_date, end_date = NULL) {
  
  # tile       <- c(220361, 220363)
  # param      <- "prcp"
  # start_date <- "2005-06-01"
  # end_date   <- "2005-06-03"
  
  # read shapefile, filter to a basin, cast to a multipolygon for masking later on
  shp <- 
    grid %>%
    # grid2 %>% 
    # dplyr::filter(CELL_ID %in% !!tile) %>%
    sf::st_transform(4326) %>%
    sf::st_cast("MULTIPOLYGON") 
  
  doParallel::stopImplicitCluster()
  
  # pull gridmet data from climateR gridMET
  gridmet <-
    climateR::getGridMET(
      AOI       = shp,
      param     = param,
      startDate = start_date,
      endDate   = end_date
    ) %>%
    raster::stack()
  
   
  # mask stacks to districts 
  gridmet <- lapply(
    X   = seq_len(nrow(shp)),
    FUN = function(x) {
      raster::mask(gridmet, shp[x, ])
    }
  )

  stack_list     <- lapply(X = gridmet, FUN = raster::stack)       # stack list of masked rasterstacks 
  cell_names     <- paste0(shp$cell_id)                        # district number
  stack_list     <- setNames(stack_list, nm = cell_names)   # add district names to stacks
  
  # mapview(stack_list[[1]]$X2005.06.01) +stack_list[[2]]$X2005.06.01 + grid2
  
  # create tidy tibbles from each raster stack in list, then wrangle date columns
  tidy_gridmet <- lapply(X = stack_list, FUN = tidy_raster) 
  
  tidy_gridmet <- lapply(
    X = names(tidy_gridmet),
    FUN = function(x) {
      dplyr::mutate(
        tidy_gridmet[[x]],
        cell_id = x
      )
    }
  ) %>%
    dplyr::bind_rows()
}


get_terra <- function(
                    shp, 
                    param, 
                    start_date, 
                    end_date = NULL
                    ) {
  
  # tile       <- c(220361, 220363)
  # param      <- "prcp"
  # start_date <- "2005-01-01"
  # end_date   <- "2005-02-01"
  
    shp <- 
      shp %>% 
      # dplyr::filter(CELL_ID %in% !!tile) %>%
      sf::st_transform(4326) %>%
      sf::st_cast("MULTIPOLYGON")
    
    doParallel::stopImplicitCluster()
    
    terra <- climateR::getTerraClim(
      AOI       = sf::st_transform(shp, 4326),
      param     = param,
      startDate = start_date,
      endDate   = end_date
    ) %>% 
      stack()
    
    terra <- lapply(
      X   = seq_len(nrow(shp)),
      FUN = function(x) {
        raster::mask(terra, shp[x, ])
      }
    )
    
    # mapview(terra[[1]]$X2005.01) + shp + terra[[2]]$X2005.01
    
    stack_list     <- lapply(X = terra, FUN = raster::stack)       # stack list of masked rasterstacks 
    cell_names     <- paste0(shp$cell_id)                        # district number 
    stack_list     <- setNames(stack_list, nm = cell_names)   # add district names to stacks
    
    # create tidy tibbles from each raster stack in list, then wrangle date columns
    tidy_terra <- lapply(X = stack_list, FUN = tidy_terra_raster)
    
    tidy_terra <- lapply(
      X = names(tidy_terra),
      FUN = function(x) {
        dplyr::mutate(
          tidy_terra[[x]],
          cell_id = x
        )
      }
    ) %>%
      bind_rows()
    
}
# tidy each raster stack in terraclim list of Raster stacks into a tidy tibble
tidy_terra_raster <- function(raster, mask = NULL) {
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        paste0(stringr::str_sub(colnames(.)[-(1:2)], start = 2L), ".1")
      )) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  rtable
}

# # tidy each raster stack in ridMET's PDSI and getEDDI() list of Raster stacks into a tidy tibble
tidy_drought_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_drought_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             # p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
}

# tidy each raster stack in gridMET (exluding PDSI) list of Raster stacks into a tidy tibble
tidy_raster <- function(raster, mask = NULL) {
  # rtable <- stack_list[[1]] %>%
  rtable <- raster %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
  return(rtable)
}

# tidy each raster stack in elevation list of Raster stacks into a tidy tibble
tidy_elev_raster <- function(raster) {
  
  rtable <-
    raster %>% 
    terra::rast() %>% 
    terra::as.points() %>% 
    tibble::as_tibble() %>% 
    setNames(
      .,
      c("elevation")
    ) 
  return(rtable)
  # rtable <-
  #   raster %>%
  #   raster::rasterToPoints() %>%
  #   tibble::as_tibble() %>%
  #   dplyr::relocate(x, y) %>%
  #   setNames(
  #     .,
  #     c("lon",
  #       "lat",
  #       "elevation"
  #     )
  #   )
  # return(rtable)
}

raster_table <- function(r) {
  rtable <- r %>%
    raster::rasterToPoints() %>%
    tibble::as_tibble() %>%
    dplyr::relocate(x, y) %>%
    setNames(
      .,
      c("lon",
        "lat",
        stringr::str_sub(colnames(.)[-(1:2)], 16, 23))
    ) %>%
    tidyr::pivot_longer(
      cols = c(tidyselect::everything(), -(1:2)),
      names_to = "date"
    ) %>%
    dplyr::mutate(date = lubridate::ymd(date)) %>%
    dplyr::relocate(lon, lat, value)
}

tidy_stack <- function(raster_list, as_sf = FALSE) {
  param_names <- names(raster_list)
  tidy_stacks <- lapply(X = raster_list, FUN = tidy_raster)
  p <- progressr::progressor(along = param_names)
  tidy_data <-
    lapply(X = param_names,
           FUN = function(rname) {
             # p(paste0("Transforming ", rname, "..."))
             setNames(
               tidy_stacks[[rname]],
               c("lon", "lat", rname, "date")
             )
           }
    ) %>%
    purrr::reduce(dplyr::left_join, by = c("date", "lon", "lat")) %>%
    dplyr::relocate(lon, lat, date)
  if (as_sf) {
    tidy_data <-
      tidy_data %>%
      sf::st_as_sf(coords = c("lon", "lat")) %>%
      sf::st_set_crs(4326)
  }
  tidy_data
}

tidy_to_raster <- function(data, x, y, z) {
  xyz <- data %>%
    dplyr::select({{ x }}, {{ y }}, {{ z }}) %>%
    dplyr::rename(
      x = {{ x }},
      y = {{ y }},
      z = {{ z }}
    )
  raster::rasterFromXYZ(
    xyz = xyz,
    crs = sf::st_crs(4326)$proj4string
  )
}

# get_eddi <- function(district, start_date, end_date = NULL, 
#                      shp_path,  basin = NULL, timestep = 1) {
#   if(is.null(basin)) {
#     
#     if(!is.null(end_date)) {
#       # dates for EDDI 
#       time = seq(
#         ymd(start_date),
#         ymd(end_date),
#         by = '1 month'
#       ) 
#     } else {  
#       time = seq(
#         ymd(start_date),
#         ymd(start_date),
#         by = '1 month'
#       )
#     } 
#     
#     # read shapefile, filter to a basin, cast to a multipolygon for masking later on
#     shp <- sf::st_read(shp_path, quiet = TRUE) %>%
#       dplyr::filter(DISTRICT %in% !!district) %>%
#       # dplyr::filter(BASIN %in% !!basin) %>%
#       sf::st_transform(4326) %>%
#       sf::st_cast("MULTIPOLYGON")
#     
#     doParallel::stopImplicitCluster()
#     
#     # pull EDDI data for each month looking at the 1 month prior, iterate over each date and input into getEDDI()
#     eddi <- lapply(X = time, function(x)
#       climateR::getEDDI(
#         AOI         = shp,
#         startDate   = x,
#         timestep    = timestep,
#         timescale   = "month"
#       )
#     ) %>%
#       raster::stack()
#     
#     # mask stacks to district boundaries
#     eddi <- lapply(
#       X   = seq_len(nrow(shp)),
#       FUN = function(x) {
#         raster::mask(eddi, shp[x, ])
#       }
#     )
#     
#     stack_list     <- lapply(X = eddi, FUN = raster::stack)       # stack list of masked rasterstacks 
#     district_names <- paste0(shp$DISTRICT)                        # district number 
#     stack_list     <- setNames(stack_list, nm = district_names)   # add district names to stacks
#     
#     # create tidy tibbles from each raster stack in list, then wrangle date columns
#     tidy_eddi <- lapply(X = stack_list, FUN = tidy_drought_raster) %>% 
#       lapply(FUN = function(x) {
#         dplyr::mutate(x,
#                       year   = stringr::str_sub(date, 15, 18),
#                       month = stringr::str_sub(date, 19, 20),
#                       doy    = stringr::str_sub(date, 21, -1),
#                       date = as.Date(with(x, paste(year,month,doy,sep="-")),"%Y-%m-%d")
#         )
#       })
#     tidy_eddi <- lapply(
#       X = names(tidy_eddi),
#       FUN = function(x) {
#         dplyr::mutate(
#           tidy_eddi[[x]],
#           district = x
#         )
#       }
#     ) %>%
#       bind_rows() %>%
#       dplyr::select(lon, lat, date, district, eddi = value)
#     
#   } else if(!is.null(basin)) {
#     
#     if(!is.null(end_date)) {
#       # dates for EDDI 
#       time = seq(
#         ymd(start_date),
#         ymd(end_date),
#         by = '1 month'
#       ) 
#     } else {  
#       time = seq(
#         ymd(start_date),
#         ymd(start_date),
#         by = '1 month'
#       )
#     } 
#     
#     
#     doParallel::stopImplicitCluster()
#     
#     # pull EDDI data for each month looking at the 1 month prior, iterate over each date and input into getEDDI()
#     eddi <- lapply(X = time, function(x)
#       climateR::getEDDI(
#         AOI         = basin,
#         startDate   = x,
#         timestep    = timestep,
#         timescale   = "month"
#       )
#     ) %>%
#       raster::stack()
#     
#     # mask stacks to district boundaries
#     eddi <- lapply(
#       X   = seq_len(nrow(basin)),
#       FUN = function(x) {
#         raster::mask(eddi, basin[x, ])
#       }
#     )
#     
#     stack_list     <- lapply(X = eddi, FUN = raster::stack)       # stack list of masked rasterstacks 
#     basin_names    <- paste0(basin$BASIN)                        # district number 
#     stack_list     <- setNames(stack_list, nm = basin_names)   # add district names to stacks
#     
#     # create tidy tibbles from each raster stack in list, then wrangle date columns
#     tidy_eddi <- lapply(X = stack_list, FUN = tidy_drought_raster) %>% 
#       lapply(FUN = function(x) {
#         dplyr::mutate(x,
#                       year   = stringr::str_sub(date, 15, 18),
#                       month = stringr::str_sub(date, 19, 20),
#                       doy    = stringr::str_sub(date, 21, -1),
#                       date = as.Date(with(x, paste(year,month,doy,sep="-")),"%Y-%m-%d")
#         )
#       })
#     
#     tidy_eddi <- lapply(
#       X = names(tidy_eddi),
#       FUN = function(x) {
#         dplyr::mutate(
#           tidy_eddi[[x]],
#           basin = x
#         )
#       }
#     ) %>%
#       bind_rows() %>%
#       dplyr::select(lon, lat, date, basin, eddi = value)
#   }
# }


# get_maca <- function(district, param, start_date, end_date = NULL,
#                      shp_path) {
#   # load shapefiles as spatial polygon object
#   shp <- sf::st_read(shp_path, quiet = TRUE) %>%
#     # dplyr::filter(DISTRICT %in% c(5)) %>%
#     dplyr::filter(DISTRICT %in% !!district) %>%
#     sf::st_transform(4326) %>%
#     sf::st_cast("MULTIPOLYGON") 
#   
#   doParallel::stopImplicitCluster()
#   
#   maca <- climateR::getMACA(
#     shp,
#     param     = param,
#     scenario  = "rcp45",
#     startDate = start_date,
#     # endDate   = end_date,
#     timeRes   = "monthly"
#     # timeRes   = "daily"
#     # param     = "tmax",
#     # startDate = "2091-01-01",
#     # endDate   = "2092-01-01"
#   ) 
#   
#   
#   maca <- raster::stack(maca)
#   
#   # mask stacks to districts 
#   maca_mask <- lapply(
#     X   = seq_len(nrow(shp)),
#     FUN = function(x) {
#       raster::mask(maca, shp[x, ])
#     }
#   )  
#   
#   
#   stack_list     <- lapply(X = maca_mask, FUN = raster::stack)       # stack list of masked rasterstacks 
#   district_names <- paste0(shp$DISTRICT)                        # district number
#   stack_list     <- setNames(stack_list, nm = district_names) 
#   
#   # create tidy tibbles from each raster stack in list, then wrangle date columns
#   tidy_maca <- lapply(X = stack_list, FUN = tidy_terra_raster)
#   # tidy_maca <- lapply(X = stack_list, FUN = tidy_bcca_raster)
#   
#   tidy_maca <- lapply(
#     X = names(tidy_maca),
#     FUN = function(x) {
#       dplyr::mutate(
#         tidy_maca[[x]],
#         district = x
#       )
#     }
#   ) %>%
#     bind_rows()
#   return(tidy_maca)
# }
# 
# # loads climateR BCCA data for desired paramter rasters, masks to the specified basin, seperates data by district, and aggregates to a tidy tibble
# get_bcca <- function(basin, param, start_date, end_date = NULL,
#                      shp_path, rcp = "rcp45") {
#   # read shapefile, filter to a basin, cast to a multipolygon for masking later on
#   shp <- sf::st_read(shp_path, quiet = TRUE) %>%
#     dplyr::filter(BASIN %in% !!basin) %>%
#     sf::st_transform(4326) %>%
#     sf::st_cast("MULTIPOLYGON")
#   # dplyr::filter(DISTRICT %in% c(64))
#   
#   # pull gridmet data from climateR gridMET
#   bcca <- climateR::getBCCA(
#     AOI       = shp,
#     param     = param,
#     startDate = start_date,
#     endDate   = end_date,
#     scenario  = rcp
#   )
#   # param     = "prcp",
#   # startDate = as.Date("2010-01-01")
#   # endDate = "2010-02-01",
#   # scenario = "rcp85") 
#   bcca <- raster::stack(bcca[1])
#   
#   # mask stacks to districts 
#   bcca_mask <- lapply(
#     X   = seq_len(nrow(shp)),
#     FUN = function(x) {
#       raster::mask(bcca, shp[x, ])
#     }
#   )  
#   
#   stack_list     <- lapply(X = bcca_mask, FUN = raster::stack)       # stack list of masked rasterstacks 
#   district_names <- paste0(shp$DISTRICT)                        # district number
#   stack_list     <- setNames(stack_list, nm = district_names)   # add district names to stacks
#   
#   # create tidy tibbles from each raster stack in list, then wrangle date columns
#   tidy_bcca <- lapply(X = stack_list, FUN = tidy_bcca_raster) 
#   
#   tidy_bcca <- lapply(
#     X = names(tidy_bcca),
#     FUN = function(x) {
#       dplyr::mutate(
#         tidy_bcca[[x]],
#         district = x
#       )
#     }
#   ) %>%
#     bind_rows()
# }
# 
# 
# get_loca <- function(basin, param, start_date, end_date = NULL,
#                      shp_path) {
#   # load shapefiles as spatial polygon object
#   shp <- sf::st_read(shp_path, quiet = TRUE) %>%
#     # dplyr::filter(BASIN %in% !!basin) %>%
#     sf::st_transform(4326) %>%
#     sf::st_cast("MULTIPOLYGON") %>% 
#     dplyr::filter(DISTRICT %in% distr_num)
#   
#   
#   loca <- climateR::getLOCA(
#     shp,
#     param     = param,
#     startDate = start_date,
#     endDate   = end_date
#   ) 
#   loca <- raster::stack(loca)
#   
#   # mask stacks to districts 
#   loca_mask <- lapply(
#     X   = seq_len(nrow(shp)),
#     FUN = function(x) {
#       raster::mask(loca, shp[x, ])
#     }
#   )  
#   
#   
#   # loca_disagg <- lapply(
#   #   X   = seq_len(length(loca_mask)),
#   #   FUN = function(x) {
#   #     raster::disaggregate(loca_mask[[x]], fact = 4)
#   #   }
#   # )
#   
#   stack_list     <- lapply(X = loca_mask, FUN = raster::stack)       # stack list of masked rasterstacks 
#   district_names <- paste0(shp$DISTRICT)                        # district number
#   stack_list     <- setNames(stack_list, nm = district_names) 
#   
#   # create tidy tibbles from each raster stack in list, then wrangle date columns
#   tidy_loca <- lapply(X = stack_list, FUN = tidy_bcca_raster) 
#   
#   tidy_loca <- lapply(
#     X = names(tidy_loca),
#     FUN = function(x) {
#       dplyr::mutate(
#         tidy_loca[[x]],
#         district = x
#       )
#     }
#   ) %>%
#     bind_rows()
# }
# 
# get_spi <- function(prcp, start_year, end_year,  timescale = 1) {
#   
#   prcp_ts <- prcp %>%
#     mutate(
#       YEAR      = as.integer(lubridate::year(date)),
#       MONTH     = as.integer(lubridate::month(date)),
#       PRCP      = prcp
#     ) %>%
#     ungroup() %>%
#     dplyr::select(YEAR, MONTH, PRCP)
#   
#   # add a date column from YEAR and MONTH cols
#   prcp_ts$date <- zoo::as.yearmon(paste(prcp_ts$YEAR, prcp_ts$MONTH), "%Y %m")
#   
#   # make YEAR & MONTH columns integers, required for SPI function
#   prcp_ts$YEAR <- as.integer(prcp_ts$YEAR)
#   prcp_ts$MONTH <- as.integer(prcp_ts$MONTH)
#   
#   # SPI dataframe a timeseries
#   prcp_ts <- ts(
#     prcp_ts,
#     start      = c(start_year, 1),
#     end        = c(end_year, 12),
#     frequency  = 12
#   )
#   
#   # SPI column name
#   spi_name <- paste0("spi", timescale)
#   
#   # calculate SPI 
#   spi <- SPEI::spi(prcp_ts[,3], timescale)
#   spi <- tsbox::ts_data.frame(spi$fitted) %>% 
#     setNames(c("date", spi_name))
#   
#   return(spi)
#   
#   rm(prcp_ts, spi_name)
#   
# }

# tidy_bcca_raster <- function(raster) {
#   rtable <- raster %>%
#     raster::rasterToPoints() %>%
#     tibble::as_tibble() %>%
#     dplyr::relocate(x, y) %>%
#     setNames(
#       .,
#       c("lon",
#         "lat",
#         stringr::str_sub(colnames(.)[-(1:2)], start = 2L, end = 11))
#     ) %>%
#     tidyr::pivot_longer(
#       cols = c(tidyselect::everything(), -(1:2)),
#       names_to = "date"
#     ) %>%
#     dplyr::mutate(date = lubridate::ymd(date)) %>%
#     dplyr::relocate(lon, lat, value)
#   rtable
# }
# 
# tidy_drought_raster <- function(raster, mask = NULL) {
#   rtable <- raster %>%
#     raster::rasterToPoints() %>%
#     tibble::as_tibble() %>%
#     dplyr::relocate(x, y) %>%
#     setNames(
#       .,
#       c("lon",
#         "lat",
#         stringr::str_sub(colnames(.)[-(1:2)], start = 2L))
#     ) %>%
#     tidyr::pivot_longer(
#       cols = c(tidyselect::everything(), -(1:2)),
#       names_to = "date"
#     ) %>%
#     # dplyr::mutate(date = lubridate::ymd(date)) %>%
#     dplyr::relocate(lon, lat, value)
#   rtable
# }


# # add elevation data to climate lat/lon data
# add_elev <- function(df, subdate) {
#   clim <- df %>% 
#     dplyr::mutate(
#       year = lubridate::year(date),
#       month = lubridate::month(date)
#     ) %>% 
#     dplyr::filter(date == subdate) 
#   
#   clim2 <- clim %>% 
#     dplyr::select(x = lon, y = lat)
#   
#   pt_prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#   sp::coordinates(clim2) <- ~x+y
#   sp::gridded(clim2) <- TRUE
#   
#   clim_sp <- sp::SpatialPoints(
#     sp::coordinates(clim2),
#     proj4string = sp::CRS(pt_prj)
#   )
#   
#   elev <- elevatr::get_elev_point(
#     locations = clim_sp,
#     prj = pt_prj,
#     src = "aws"
#   ) %>% 
#     sf::st_as_sf() %>% 
#     dplyr::mutate(
#       lon = st_coordinates(.)[,1],
#       lat = st_coordinates(.)[,2]
#     ) %>% 
#     sf::st_drop_geometry() %>% 
#     dplyr::select(-elev_units)
#   
#   clim_join <- dplyr::left_join(df, elev, by = c("lon", "lat"))
# }

# normalize <- function(x) {
#   return ((x - min(x)) / (max(x) - min(x)))
# }
# 
# stdize = function(x, ...) {(x - min(x, ...)) / (max(x, ...) - min(x, ...))}
# 
# # Robust scalar normalization
# robust_scalar<- function(x){
#   (x- median(x)) /(quantile(x,probs = .75)-quantile(x,probs = .25))
# }
# 
# # Min-Max Normalization
# norm_minmax <- function(x){
#   (x- min(x)) /(max(x)-min(x))
# }
# 
# # Mean Normalization
# mean_norm_minmax <- function(x){
#   (x- mean(x)) /(max(x)-min(x))
# }
# aggregate_maca <- function(aoi, start_date, end_date = NULL, as_sf = FALSE) {
#   p <- progressr::progressor(steps = 3L)
#   p("Getting MACA data...")
#   climate_data <- climateR::getMACA(
#     AOI       = aoi,
#     param     = common_params(),
#     startDate = start_date,
#     endDate   = end_date,
#     model     = "BNU-ESM"
#   )
#   p("Tidying MACA data...")
#   tidy_clim <-
#     tidy_stack(
#       c(climate_data),
#       as_sf = as_sf
#     ) 
#   # dplyr::rename(
#   #   prcp  = tidyselect::contains("prcp"),
#   #   rhmax = tidyselect::contains("rhmax"),
#   #   rhmin = tidyselect::contains("rhmin"),
#   #   shum  = tidyselect::contains("shum"),
#   #   srad  = tidyselect::contains("srad"),
#   #   tmin  = tidyselect::contains("tmin"),
#   #   tmax  = tidyselect::contains("tmax")
#   # ) %>%
#   # dplyr::mutate(
#   #   rhavg = (rhmax + rhmin) / 2,
#   #   tavg  = (tmax + tmin) / 2,
#   #   cbi_rhmax_tmax = chandler_bi(rhmax, tmax),
#   #   cbi_rhmin_tmax = chandler_bi(rhmin, tmax),
#   #   cbi_rhavg_tmax = chandler_bi(rhavg, tmax),
#   #   cbi_rhmax_tmin = chandler_bi(rhmax, tmin),
#   #   cbi_rhmin_tmin = chandler_bi(rhmin, tmin),
#   #   cbi_rhavg_tmin = chandler_bi(rhavg, tmin),
#   #   cbi_rhmax_tavg = chandler_bi(rhmax, tavg),
#   #   cbi_rhmin_tavg = chandler_bi(rhmin, tavg),
#   #   cbi_rhavg_tavg = chandler_bi(rhavg, tavg),
#   #   burn_index = (
#   #     cbi_rhmax_tmax + cbi_rhmin_tmax + cbi_rhavg_tmax +
#   #       cbi_rhmax_tmin + cbi_rhmin_tmin + cbi_rhavg_tmin +
#   #       cbi_rhmax_tavg + cbi_rhmin_tavg + cbi_rhavg_tavg
#   #   ) / 9
#   # ) %>%
#   # dplyr::select(lat, lon, date, prcp, rhmax, rhmin, shum,
#   #               srad, tmin, tmax, burn_index)
#   p("Tidied!")
#   tidy_clim
# }
# # generic gridmet aggregation, use get_pdsi() if you want PDSI values
# aggregate_gridmet <- function(aoi, params, start_date, end_date = NULL, as_sf = FALSE) {
#   p <- progressr::progressor(steps = 3L)
#   p("Getting GridMET data...")
#   climate_data <- climateR::getGridMET(
#     AOI       = sf::st_transform(aoi, 4326),
#     param     = params,
#     startDate = start_date,
#     endDate   = end_date
#   )
#   p("Tidying GridMET data...")
#   tidy_clim <-
#     tidy_stack(
#       c(climate_data),
#       as_sf = as_sf
#     ) %>%
#     dplyr::rename(
#       prcp       = tidyselect::contains("prcp"),
#       rhmax      = tidyselect::contains("rhmax"),
#       rhmin      = tidyselect::contains("rhmin"),
#       shum       = tidyselect::contains("shum"),
#       srad       = tidyselect::contains("srad"),
#       tmin       = tidyselect::contains("tmin"),
#       tmax       = tidyselect::contains("tmax"),
#     )
#   p("Tidied!")
#   tidy_clim
# }
# 
# aggregate_terraclim <- function(aoi, params, start_date, end_date = NULL, as_sf = FALSE) {
#   p <- progressr::progressor(steps = 3L)
#   p("Getting Terraclim data...")
#   climate_data <- climateR::getTerraClim(
#     AOI       = sf::st_transform(aoi, 4326),
#     param     = params,
#     startDate = start_date,
#     endDate   = end_date
#   )
#   p("Tidying Terraclim data...")
#   tidy_clim <-
#     tidy_stack(
#       c(climate_data),
#       as_sf = as_sf
#     ) %>%
#     dplyr::rename(
#       water_deficit = tidyselect::contains("water_deficit"),
#       palmer        = tidyselect::contains("palmer"),
#       prcp          = tidyselect::contains("prcp"),
#       rhmax         = tidyselect::contains("rhmax"),
#       soilm         = tidyselect::contains("soilm"),
#       aet           = tidyselect::contains("aet"),
#       srad          = tidyselect::contains("srad"),
#       runoff        = tidyselect::contains("q"),
#       swe           = tidyselect::contains("swe"),
#       tmin          = tidyselect::contains("tmin"),
#       tmax          = tidyselect::contains("tmax"),
#     )
#   p("Tidied!")
#   tidy_clim
# }