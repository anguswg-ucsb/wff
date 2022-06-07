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

doParallel::stopImplicitCluster()
parallel::stopCluster(cl)
# **********************************************************

# *******************************
# ---- Locate state coverage ----
# *******************************

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

doParallel::stopImplicitCluster()
parallel::stopCluster(cl)
# *******************************
# ---- Extract raster values ----
# *******************************

new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"

states <- state.abb[c(3, 5, 6, 28, 31, 44, 50)]

state_shp <- USAboundaries::us_states() %>% 
  filter(stusps %in% states) %>% 
  terra::vect() %>% 
  terra::project(new_crs)
# Save

wl_geom_path <- paste0("data/wetlands/simple_geoms/", list.files("data/wetlands/simple_geoms/"))

r_path       <- paste0("data/wetlands/raster_subset/", list.files("data/wetlands/raster_subset/"))
# # NAIP Grid
grid <- readRDS("data/grid/naip_huc_grid.rds") %>%
  dplyr::mutate(
    cell_id    = as.character(CELL_ID),
    naip_entit = as.character(NAIP_ENTIT)
    ) %>%
  mutate(new_id = 1:n()) %>% 
  dplyr::select(new_id, cell_id, naip_entit) %>% 
  sf::st_transform(new_crs) 


i <- 7

for (i in 1:length(wl_geom_path)) {

  state_abb   <- substr(wl_geom_path[i], 28, 29)

  aoi   <- terra::vect(readRDS(wl_geom_path[i]))
  # %>% 
  #   sf::st_make_valid()
  
  # aoi_t <- 
  #   aoi %>%
  #   terra::vect()
  
  subset_path <- grep(
                  state_abb,
                  r_path,
                  value = T
                  )
  
  logger::log_info("Extracting raster values...\n State: {state_abb}")
  
  # state rasters
  state_subs_path <- paste0(subset_path, "/", list.files(subset_path))

  # State shape
  shp <- 
    state_shp %>% 
    sf::st_as_sf() %>% 
    dplyr::filter(stusps == state_abb) 
  # %>% terra::vect()

  # filter grid to state of interest
  aoi_grid <- 
    grid %>%
    sf::st_filter(shp)
  
  # Calculate area and tidy up
  intersect_pct <- 
    aoi_grid %>% 
    sf::st_intersection(shp) %>% 
    dplyr::mutate(
      intersect_area = sf::st_area(.)
      ) %>%  
    dplyr::select(cell_id, intersect_area) %>%   
    sf::st_drop_geometry()  
  
  # Create a fresh area variable for counties
  aoi_grid <- 
    aoi_grid %>% 
    dplyr::mutate(naip_tile_area = sf::st_area(aoi_grid)) %>% 
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
    dplyr::filter(coverage > 0.50)

  # mapview(aoi_grid) + shp
  
  
  for (z in 1:length(state_subs_path)) {
    
    z <- 15
    
    logger::log_info("{z} of {length(state_subs_path)}\nState: {state_abb}\n Raster path: {state_subs_path[z]}")
    
    # rsub <- terra::rast(state_subs_path[z])
    rsub <- raster::raster(state_subs_path[z])
    mapview(naip_tiles, color = "red") + aoi_grid + rsub
    # f <- list.files(path = subset_path, pattern = ".tif$", full.names = TRUE)
    # rl <- lapply(f, raster)
    
    # do.call(merge, c(rl, tolerance = 1))
    r_bb <- 
      rsub %>% 
      sf::st_bbox() %>% 
      sf::st_as_sfc() %>%
      sf::st_as_sf()
    
    naip_tiles <-
      aoi_grid %>% 
      sf::st_filter(r_bb) %>% 
      dplyr::mutate(tmp_id = 1:n())
    
    r_pct_cover <- exactextractr::exact_extract(
                            rsub,
                            naip_tiles,
                            coverage_area = T,
                            fun           = "count",
                            force_df      = T
                          ) %>% 
      dplyr::mutate(tmp_id = 1:n()) %>% 
      stats::setNames(c("cover_area", "tmp_id"))
    
    naip_cover <- 
      naip_tiles %>% 
      dplyr::left_join(
        r_pct_cover, 
        by = "tmp_id"
        ) %>%  # %>% dplyr::left_join(r_pct_cover2, by = "tmp_id") 
      dplyr::relocate(
        new_id, cell_id, naip_entit, tmp_id, naip_tile_area,
        intersect_area, coverage, cover_area, geometry) 
    
    rb_mask <- 
      rsub %>% 
      mask(naip_tiles) 
    
    # empty raster
    rtemp <- raster::raster(
      ext = extent(naip_tiles),
      res = c(500, 500),
      crs = crs(naip_tiles)
    )
    
    # Create raster of HUC12s
    naip_r <- fasterize::fasterize(
      naip_tiles,
      raster = rtemp, 
      field  = "new_id"
    )
    
    naip_mask <- 
      naip_r %>% 
      raster::mask(r_bb) %>% 
      terra::rast() %>%
      as.polygons() %>% 
      st_as_sf()
    
    #Make a copy of the input raster to collect the outputs
    output <- brick(rsub)
    for(i in 1:nrow(naip_mask)){
      
      logger::log_info("{i} of {nrow(naip_mask)}")
      polygon <- 
        naip_mask[i,] 
      
      r_mask <- 
        rsub %>%
        raster::crop(polygon) %>%
        raster::mask(polygon)
      
      bb <-
        r_mask %>% 
        # extent() %>% 
        sf::st_bbox() %>% 
        sf::st_as_sfc() %>%
        sf::st_as_sf() %>% 
        terra::vect()
 
      aoi_t <- vect(aoi)
      
      aoi_crop <- 
        aoi_t %>% 
        terra::crop(
                                    terra::ext(
                                      terra::rast(r_mask) 
                                      )
                                    ) %>% 
        sf::st_as_sf()
      
      extr <- exactextractr::exact_extract(
        r_mask,
        aoi_crop,
        fun      = "majority",
        force_df = T
      ) %>% 
        dplyr::
      
      
      
      
      
      # r_crop <-
      #   aoi_t %>% 
      #   terra::crop(
      #     terra::ext(
      #      terra::rast(rsub) 
      #      )
      #     ) %>% 
      #   st_as_sf() %>% 
      #   st_cast("MULTIPOLYGON")
      
      # aoi_sf <-
      #   aoi_crop %>% 
      #   st_as_sf()
      # 
      
      mapview(bb) + aoi_crop + r_mask 
      # %>%
        # st_as_sf(coords = c("x", "y"), crs = crs(r)) %>%
        # dplyr::summarise(geometry = sf::st_combine(geometry)) 
      # %>%
      #   st_cast("POINT") %>% 
      #   st_buffer(dist=200)
      # 
      #You can't crop here because the extents of the outputs need to be the same
      m <- rsub %>% raster::mask(polygon)
      # mapview(m) + naip_mask
      #Add the new raster to the output brick
      output <- raster::addLayer(output, m)
    }

      # terra::vect()
      mapview(r_bb) + naip_tiles + naip_r + naip_mask
      length(unique(values(naip_mask$layer)))
      length(unique(values(naip_r$layer)))
      unique(values(naip_r$layer)) %in% unique(naip_tiles$new_id)
    # r_bb_sf <- 
    #   rsub %>% 
    #   sf::st_bbox() %>%
    #   sf::st_as_sfc() %>%
    #   sf::st_as_sf() %>% 
    #   terra::vect() %>%     
    #   terra::ext()
    # 
      # CHeck which polygons overlaps with my buffer
      out_overlap = st_overlaps(r_bb, aoi)
      nbrs_buff <- aoi[st_overlaps(r_bb,aoi)[[1]],]
      u <- st_union(st_geometry(nbrs_buff), st_geometry(r_bb), by_feature = T)
      mapview(u)
      int_bb = st_difference(r_bb, u) 
  aoi_crop <- 
    aoi %>% 
    sf::st_crop(r_bb)  
    
  mapview(aoi_crop$geometry)+ r_bb
  
    terra::crop(r_bb) %>% 
    terra::erase()
  
  aoi_erase <- aoi_t %>% terra::erase(r_bb_sf)
  
  mask(uk, lnd, inverse = TRUE)
  
    aoi_int <- r_bb %>% 
      terra::erase(aoi_t)
    
    # terra::erase()
    aoi_int <-
      aoi %>% 
      sf::st_filter(r_bb, .predicate = st_intersects) %>% 
      mutate(new_id = 1:n())
    f <- system.file("ex/lux.shp", package="terra")
    v <- vect(f)
    e <- ext(5.6, 6, 49.55, 49.7)
    x <- erase(v, e)
    plot(v)
    plot(e, col = "red", add = T)
    rf <- fasterize::fasterize(aoi_int, raster = rsub, field = "new_id")
    plot(rf)
    aoi_within <- 
      aoi %>% 
      st_filter(r_bb, .predicate = st_covered_by)

    mapview(aoi_int, color = "red") + r_bb + aoi_within  + rsub + rf
    plot(rsub$Band_1)
  system.time(
    # extract most frequent buffer value in cells in NAIP tiles
    wl_stats <- exactextractr::exact_extract(
      rsub,
      aoi, 
      fun      = "majority",
      force_df = T
    ) %>% 
      setNames(c("valley_bottom"))
  )
    }
}

wl_final <- bind_cols(poly_simple_sf, wl_stats)

# *******************************
# ---- next ----
# *******************************

# wl_simple_path <-    paste0("da ta/wetlands/sim ple_geoms/", list.files("data/wetlands/simple_geoms/"))
# i <- 3
for (i in 1:length(wl_simple_path)) {
  
  aoi <- states[i] 
  
  logger::log_info("State: {aoi}")  
  
  wl_path <- paste0("data/wetlands/", aoi, "_geodatabase_wetlands.gdb")
  



  
  # State wetlands coverage (terra)
  wl <- terra::vect(
    wl_path,  
    layer = paste0(aoi, "_Wetlands")
    )

  # State wetlands coverage (sf)
  # wl <- sf::read_sf( wl_path, layer = paste0(aoi, "_Wetlands") )
  

  # State Wetlands_Project_Metadata (terra)
  wl_meta <- terra::vect(
    wl_path,  
    layer = paste0(aoi, "_Wetlands_Project_Metadata")
  )
  # State Wetlands_Project_Metadata (sf)
  # wl_meta <-  sf::read_sf(wl_path, layer = paste0(aoi, "_Wetlands_Project_Metadata"))
  
  # Join Meta data w/ wetland polys
  # wl_join <- st_join(wl, wl_meta)
  
  # Wetland polygons (terra)
  # polys <- wl[c(1:20000), ]
  # Wetland polygons (sf) 
  # polys <- terra::vect(wl[c(1:20000), ])

  
  # Buffer raster (terra)
  rs    <- terra::rast(buffer_path[1])
  # Buffer raster (raster)
  # r1 <- raster::raster(buffer_path[1])
  # rs    <- terra::rast(r1)

  # rm(polys2, poly_simple, poly_simple_sf, wl_stats, wl_final,
  # r2, r3, tmp_shp, r, ex, ex_max, ex_mean, grid, poly_sf, w)
  
  # CRS to change polygons too
  # new_crs <- terra::crs(rs, proj = T)
  new_crs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
  system.time(
  # Project polygons to raster CRS
  poly_simple <-
    polys %>% 
    terra::project(new_crs) %>% 
    terra::simplifyGeom(20) %>% 
    sf::st_as_sf()
  )
  
  
  # Difference in number of points after simplifying
  # simple_npts <- mapview::npts(poly_simple)
  # orig_npts   <- mapview::npts(sf::st_as_sf(polys))
  # logger::log_info("Simplifying geometries\n-->  {round(simple_npts/orig_npts * 100, 2)} % of original geometry points")

  system.time(
  # extract most frequent buffer value in cells in NAIP tiles
  wl_stats <- exactextractr::exact_extract(
    rs,
    poly_simple, 
    fun      = "majority",
    force_df = T
  ) %>% 
    setNames(c("valley_bottom"))
  )
  
  wl_final <- bind_cols(poly_simple_sf, wl_stats)
  
  
  # mapview(poly_simple_sf, color = "red") + r1
  
  # terra extract
  # system.time(
  #   ex_max <- terra::extract(rs, polys2, max, na.rm=TRUE)
  # )
  
  plot(wl_stat_df$buff_value)
  plot(ex_max$buff_31108)
  wl_stat_df <- tibble(buff_value = wl_stats)
  # terra::extract(rs, polys2, mean, na.rm=TRUE)
  ex_mean <- ex %>% 
    group_by(ID) %>% 
    setNames(c("ID", "buff_value")) %>% 
    summarize(mean = mean(buff_value, na.rm = F))
  
  unique(ex$ID)
  st_crs(wl) 
  wl2 <- wl %>% st_transform(5070)
  polys <- terra::vect(wl[c(1:100), ])
  rm(r3)
  new_crs <- crs(rs, proj = T)
  crs(rs, proj = T)
  w <- project(rs, new_crs)
  
  p2 <- project(polys, new_crs2)
  
  rs    <- terra::rast(r2)
  # tmp_shp <- wl[c(1:100), ]
  # mapview(tmp_shp) + r3 + r1 + r2
  ex <- terra::extract(rs, polys)
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
  mapview(r1) + r2 + r3
  r1 <- raster("Top1_raster.tif")
  r2 <- raster("Top2_raster.tif")
  
  # Resample
  x1 <- resample(r1, crop(x, r1))
  x2 <- resample(r2, crop(x, r2))
  
  # Merge rasters. Make sure to use the right order
  m <- merge(merge(x1, x2), x)

  # wl_join <- st_join(wl, wl_meta)
}
wetlands_path <- list.files("data/wetlands/CO_shapefile_wetlands/")

wy_wetlands_path <- list.files("data/wetlands/WY_shapefile_wetlands/")
wy_east <- sf::read_sf(paste0("data/wetlands/WY_shapefile_wetlands/", wy_wetlands_path[22]))
wy_wl2 <- sf::read_sf("data/wetlands/WY_geodatabase_wetlands.gdb", layer = "WY_Wetlands", METHOD= "ONLY_CCW" )

wy_west <- sf::read_sf(paste0("data/wetlands/WY_shapefile_wetlands/", wy_wetlands_path[46]))
east <- sf::read_sf(paste0("data/wetlands/CO_shapefile_wetlands/", wetlands_path[54]))
west <- sf::read_sf(paste0("data/wetlands/CO_shapefile_wetlands/", wetlands_path[38]))
co <- sf::read_sf(paste0("data/wetlands/CO_shapefile_wetlands/", wetlands_path[30]))
# grid_stats <- readRDS( "data/final/naip_summary_stats.rds")
wetlands <- sf::read_sf("data/wetlands/CO_geodatabase_wetlands.gdb")

wetlands <- sf::st_read("data/wetlands/CO_geodatabase_wetlands.gdb")
wetlands <- sf::read_sf("data/wetlands/CO_shapefile_wetlands/CO_Wetlands.shp")

wetlands <- terra::vect("data/wetlands/CO_geodatabase_wetlands.gdb", layer = "CO_Wetlands")
wy_wl <- terra::vect("data/wetlands/WY_geodatabase_wetlands.gdb", layer = "WY_Wetlands")
wy_wl2 <- sf::read_sf("data/wetlands/WY_geodatabase_wetlands.gdb", layer = "WY_Wetlands")
wl_sf <- st_as_sf(wetlands)

library(rmapshaper)
wl_simple <- wl_sf %>% 
  rmapshaper::ms_simplify(keep = 0.5)
mapview::npts(wl_sf)

# rm(wetlands)

