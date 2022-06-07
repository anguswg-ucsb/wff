# Angus Watters
# Join climate means, elevation, NWM, and HUC12 information w/ NAIP tiles

# notes:
# - results generate should be summarized on the basis of "NAIP_Entity" (eg 3610745_nw) and year of NAIP flight.
# - For NWM Flow, results are on a HUC12 basis, identify HUC12 w/ most area within each NAIP cell & link the NAIP grid to HUC12

rm(list = ls())

library(terra) 
# library(raster) 
library(sf)
library(dplyr)
library(foreign)
# library(ggplot2)
# library(mapview)
# library(SpaDES)

source("utils/utils.R")

# ***************************
# ---- Raster to polygon ----
# ***************************
state_dirs <- list.files(here::here("data", "nass", "rasters"), recursive = F)
nass_path  <- paste0(here::here("data", "nass", "rasters"), "/", state_dirs)
# nass_path  <- paste0(here::here("data", "nass", "rasters"), "/", state_dirs)[4]

# VAT DBF for Rasters
vat_path <- here::here("data", "nass", "rasters")
vat_state_paths <- paste0(vat_path, "/", list.files(vat_path, recursive = T, pattern = ".vat.dbf"))
r_class_path    <- here::here("data", "nass", "raster_classes")
vat_class_path  <- paste0(r_class_path, "/", list.files(r_class_path, recursive = T, pattern = "raster_vat_flagged"))

vat_classes <- 
  readr::read_csv(vat_class_path) %>% 
  setNames(c("VALUE", "croptype", "keep_remove")) %>% 
  dplyr::mutate(
    croptype    = snakecase::to_snake_case(as.character(croptype)),
    croptype    = gsub("dbl_", "", croptype),
    croptype    = gsub("developed_", "dev_", croptype),
    croptype    = gsub("crop_", "", croptype),
    keep_remove = tolower(keep_remove)
  ) %>% 
  dplyr::mutate(
    keep_remove = case_when(
      croptype == "clover_wildflowers" ~ "keep",
      croptype == "sod_grass_seed"     ~ "keep",
      TRUE                             ~ keep_remove
    )
  ) 

# classes to remove
rm_classes <-
  vat_classes %>% 
  dplyr::filter(keep_remove == "remove")

# classes to keep
keep_classes <-
  vat_classes %>% 
  dplyr::filter(keep_remove == "keep")
# st <- lapply(r_path, rast) %>%  
#   terra::rast() 
# library(USAboundaries)
# 
# counties <- us_counties()

# rm(buffer_path)
# states <- state.abb[c(3, 28, 31, 44)]
i <- 1
# rm(i)
# tmp <- readRDS("data/nass/shp/AZ/AZ_nass_geom_2015.rds")
# plot(tmp[c(1:3),])
for (i in 1:length(nass_path)) {
  
  state_abb <- state_dirs[i]
  
  logger::log_info("\n\n{state_abb} NASS rasters")
  
  if (state_abb == "UT") {
    
    r_path <- paste0(
      nass_path[i], "/",
      grep(
        ".tif$",
        list.files(nass_path[i]),
        value = T
      )
    )
    
    vat_path <- paste0(
      nass_path[i], "/",
      grep(
        ".tif.vat",
        list.files(nass_path[i]),
        value = T
      )
    )
    
    r_path   <- r_path[5]
    vat_path <- vat_path[5]
    
  } else {
    r_path <- paste0(
      nass_path[i], "/",
      grep(
        ".tif$",
        list.files(nass_path[i]),
        value = T
      )
    )
    
    vat_path <- paste0(
      nass_path[i], "/",
      grep(
        ".tif.vat",
        list.files(nass_path[i]),
        value = T
      )
    )
    
  }
  #import all raster files in folder using lapply
  # st <- lapply(r_path, raster) %>% 
  #   raster::stack()
  # st <- lapply(r_path, rast) %>%  
  #   terra::rast() # terra::aggregate(fact = 50)
  # 
  # years <- 2013:2020
  
  # if (state_abb == "NM") {
  #   years <- 2014:2020
  #   st <- st[[c(2:8)]]
  # } else {
  #   years <- 2013:2020
  # }

  z = 1

  # for (z in 1:raster::nlayers(st)) {
  # for (z in 1:terra::nlyr(st)) {
  
  for (z in 1:length(r_path)) {
    
    year <- substr(r_path[z], 69, 72)
    # year
    
    logger::log_info("\n\n{state_abb} - {year}")
    
    nass <- terra::rast(r_path[z]) 
    
    # unique(values(nass))
    nass_class <- terra::ifel(nass %in% keep_classes$VALUE, 1, NA)
    
    system.time(
    nass_class2 <- 
      nass_class %>% 
      terra::mask(
        nwi, inverse = T
        )
    )
    plot(nass_class2$CLASS_NAME)
    # raster to polygon
    nass_poly <-
      nass_class %>% 
      terra::as.polygons() %>% 
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON")
    
    # nass_poly2 <- n
    # st[[z]] %>%
    # terra::as.polygons() %>% 
    # sf::st_as_sf() %>%
    tile_poly <- 
      tile_poly %>% 
      setNames(c("VALUE", "geometry")) %>%
      dplyr::left_join(
        dplyr::select(data_dbf, VALUE, CLASS_NAME),
        by = "VALUE"
      ) %>%
      sf::st_cast("MULTIPOLYGON")
    # unique(values(nass_class))
    # crs(nass, proj = T)
    # plot(nass)
    # rm(nass)
    # %>%  terra::aggregate(fact = 75)
    
    # classify 0 as NA (background)
    nass <- terra::classify(
        nass,
        cbind(0, NA)
        ) 
    # plot(nass_class$CLASS_NAME)
    # plot(nass$CLASS_NAME)
    # nass$CLASS_NAME
    # plot(nass)
    # unique(values(nass_rc))
    # length(unique(values(nass_rc)))
    
    data_dbf <- foreign::read.dbf(vat_path[z]) %>%
      tibble::tibble()
    
    # Add value codes to leveled raster
    # levels(st[[z]]) <- data_dbf$VALUE
    
    # # classify 0 as NA (background)
    # st[[z]] <- terra::classify(
    #   st[[z]],
    #   cbind(0, NA)
    # )
    
    logger::log_info("\n\nConverting raster to polygons - {state_abb}")
    
    # raster to polygon
    tile_poly <-
      nass %>% 
      terra::as.polygons() %>% 
      sf::st_as_sf() %>%
      sf::st_cast("MULTIPOLYGON")
    
      # st[[z]] %>%
      # terra::as.polygons() %>% 
      # sf::st_as_sf() %>%
    tile_poly <- 
      tile_poly %>% 
      setNames(c("VALUE", "geometry")) %>%
      dplyr::left_join(
        dplyr::select(data_dbf, VALUE, CLASS_NAME),
        by = "VALUE"
      ) %>%
      sf::st_cast("MULTIPOLYGON")
    
    
    logger::log_info("\n\nSaving {state_abb} NASS polygons")
    
    saveRDS(
      tile_poly,
      paste0(
        here::here("data/nass/shp/"), "/", state_abb, "/", state_abb, 
        "_nass_geom_", year, ".rds"
      )
    )
      # paste0(here::here("data/nass/shp/"), "/", state_abb, "/",
      #        state_abb,  "_nass_geom_", years[z], ".rds") )
    rm(tile_poly, nass)
    logger::log_info("\n\nCalling garbage collector...")
    gc()
    
  }
  
}
# ***************************
# ---- Save as shapefile ----
# ***************************
new_crs <- "PROJCRS[\"unnamed\",\n    BASEGEOGCRS[\"NAD83\",\n        DATUM[\"North American Datum 1983\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101004,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4269]],\n    CONVERSION[\"Albers Equal Area\",\n        METHOD[\"Albers Equal Area\",\n            ID[\"EPSG\",9822]],\n        PARAMETER[\"Latitude of false origin\",23,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8821]],\n        PARAMETER[\"Longitude of false origin\",-96,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8822]],\n        PARAMETER[\"Latitude of 1st standard parallel\",29.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8823]],\n        PARAMETER[\"Latitude of 2nd standard parallel\",45.5,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8824]],\n        PARAMETER[\"Easting at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8826]],\n        PARAMETER[\"Northing at false origin\",0,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8827]]],\n    CS[Cartesian,2],\n        AXIS[\"easting\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]],\n        AXIS[\"northing\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1,\n                ID[\"EPSG\",9001]]]]"
# new_crs <- "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# gc()
state_dirs <- list.files(here::here("data", "nass", "rasters"), recursive = F)
nass_path  <- paste0(here::here("data", "nass", "rasters"), "/", state_dirs)
# nass_path  <- paste0(here::here("data", "nass", "rasters"), "/", state_dirs)[2]

# Path to RDS shapes
rds_path   <-   paste0(
  here::here("data/nass/shp/"), "/",
  list.files(here::here("data/nass/shp/"), recursive = T)
)
# gc()
# VAT DBF for Rasters
vat_path <- here::here("data", "nass", "rasters")
vat_state_paths <- paste0(vat_path, "/", list.files(vat_path, recursive = T, pattern = ".vat.dbf"))
r_class_path    <- here::here("data", "nass", "raster_classes")
vat_class_path  <- paste0(r_class_path, "/", list.files(r_class_path, recursive = T, pattern = "raster_vat_flagged"))

vat_classes <- 
  readr::read_csv(vat_class_path) %>% 
  setNames(c("VALUE", "croptype", "keep_remove")) %>% 
  dplyr::mutate(
    croptype    = snakecase::to_snake_case(as.character(croptype)),
    croptype    = gsub("dbl_", "", croptype),
    croptype    = gsub("developed_", "dev_", croptype),
    croptype    = gsub("crop_", "", croptype),
    keep_remove = tolower(keep_remove)
  ) %>% 
  dplyr::mutate(
    keep_remove = case_when(
      croptype == "clover_wildflowers" ~ "keep",
      croptype == "sod_grass_seed"     ~ "keep",
      TRUE                             ~ keep_remove
      )
    ) 

# vat_lst  <- list() 
# for (k in 1:length(vat_state_paths)) {
#   logger::log_info("\n\n{k} of {length(vat_state_paths)}\n{vat_state_paths[k]}")
#   vat <- foreign::read.dbf(vat_state_paths[k]) %>% 
#     tibble::tibble()
#   vat_lst[[k]] <- vat
# }
# vat_dist <- 
#   dplyr::bind_rows(vat_lst) %>% 
#   dplyr::select(VALUE, CLASS_NAME)%>% 
    # na.omit() %>%
    # distinct() %>% 
    # arrange(VALUE)
# readr::write_csv(vat_dist, "raster_vat.csv")

save_nass_path <- "D:/wff/nass/shp"

# nass_gj <- sf::st_read("D:/wff/nass/geojson/AZ_nass_2013.geojson")

i <- 1

for (i in 1:length(rds_path)) {
  
  state_abb <- substr(rds_path[i], 58, 59)
  year      <- substr(rds_path[i], 74, 77)
  
  logger::log_info("\n\n{i} of {length(rds_path)}\n{state_abb} - {year}")
  
  nass_shp <- readRDS(rds_path[i]) 

  # nass_shp2 <- st_as_sfc(nass_shp)

  # sf::st_crs(nass_shp) <- sf::st_crs(new_crs)
  # nass_tmp <- st_drop_geometry(nass_shp)
  
  vat_df <- 
    foreign::read.dbf(vat_state_paths[i]) %>% 
    tibble::tibble() %>% 
    dplyr::mutate(VALUE = as.integer(VALUE)) %>%
    dplyr::select(VALUE, CLASS_NAME)
  
  vat_keep <- 
    vat_df %>% 
    dplyr::mutate(
      croptype = snakecase::to_snake_case(as.character(CLASS_NAME)),
      croptype = gsub("dbl_", "", croptype),
      croptype = gsub("developed_", "dev_", croptype),
      croptype = gsub("crop_", "", croptype)
    ) %>% 
    dplyr::inner_join( 
      vat_classes, 
      by = c("croptype", "VALUE")
    )
  
  # dplyr::select(CLASS_NAME = VALUE, VALUE = CLASS_NAME)
  
  # 3. Calculate polygon areas and remove any polygons from each year with areas < 50,000 square meters
  # rm(nass_shp, nass_shp2)
  if(ncol(nass_shp) == 2) {
    
    vat_keep <- 
      vat_df %>% 
      dplyr::mutate(
        croptype = snakecase::to_snake_case(as.character(CLASS_NAME)),
        croptype = gsub("dbl_", "", croptype),
        croptype = gsub("developed_", "dev_", croptype),
        croptype = gsub("crop_", "", croptype)
      ) %>% 
      # dplyr::select(VALUE, croptype) %>% 
      dplyr::inner_join( 
        vat_classes, 
        by = c("croptype", "VALUE")
      ) 
    
    logger::log_info("\n\nJoining CLASS_NAME column...")
  
    # VALUE (num)
    # CLASS_NAME (name)
    # geometry (sfc multipolygon)
    # vat_df2 <- vat_df %>% rename(VALUE = CLASS_NAME, CLASS_NAME = VALUE)

    nass_shp <-
      nass_shp %>%
      # setNames(c("CLASS_NAME", "geometry")) %>% 
      setNames(c("VALUE", "geometry")) %>% 
      dplyr::left_join(
      vat_df,
      by = "VALUE"
      # by = "CLASS_NAME"
      ) %>% 
      dplyr::relocate(VALUE, croptype = CLASS_NAME) %>%
      dplyr::mutate(
        croptype = snakecase::to_snake_case(as.character(croptype)),
        croptype = gsub("dbl_", "", croptype),
        croptype = gsub("developed_", "dev_", croptype),
        croptype = gsub("crop_", "", croptype)
      ) %>% 
      dplyr::left_join(
        dplyr::select(vat_keep, VALUE, croptype, keep_remove),
        by = c("croptype", "VALUE")
      ) %>% 
      dplyr::filter(keep_remove == "keep") %>%
      # dplyr::filter(keep_remove == "keep" | croptype %in% c("fallow_idle_cropland", "other_tree_crops")) %>%
      dplyr::select(-keep_remove) %>%
      sf::st_cast("POLYGON") %>%
      dplyr::mutate(
        class_area = as.numeric(sf::st_area(.))
      ) %>%
      sf::st_transform(4326)
    
    # nass_shp <- nass_shp %>%
    #   sf::st_transform(4326)
    # length(unique(nass_table$croptype))
    # nass_table <- sf::st_drop_geometry(nass_shp)
    nass_table <- 
      nass_shp %>% 
      sf::st_drop_geometry() %>% 
      tibble::tibble() 
    # 
    # length(unique(dplyr::filter(nass_table, class_area >= 50000)$croptype))
    # length(unique(nass_table$croptype))
    
 
    logger::log_info("\n\nMax area (sq. meters): {max(nass_shp$class_area)}\nMin area (sq. meters): {min(nass_shp$class_area)}")
    
    # logger::log_info("\n\nMax area (sq. meters): {max(nass_shp2$class_area)}\nMin area (sq. meters): {min(nass_shp2$class_area)}")
    
    # nass_shp3 <-  terra::vect(nass_shp2)
    # nass_shp3$area_sqkm <- terra::expanse(nass_shp3)
  
    
    # hist(nass_table2$area_sqkm, breaks = 5)

    nass_shp <- 
      nass_shp %>%
      dplyr::filter(class_area >= 50000) %>% 
      dplyr::select(-VALUE) %>% 
      sf::st_make_valid() %>%
      dplyr::group_by(croptype) %>%
      dplyr::summarise(geometry = st_combine(geometry)) %>% 
      dplyr::ungroup()
    
    nass_shp <- 
      nass_shp %>% 
      dplyr::summarise(geometry = st_combine(geometry)) %>% 
      dplyr::mutate(croptype = paste0(state_abb, "_nass_", year))
    
    # sf::write_sf(
    # sf::st_write(
    #   nass_shp,
    #   dsn   = paste0(save_nass_path, "3/", state_abb, "/", state_abb, "_nass_", year, "_union_v2.gpkg"),
    #   # dsn   = paste0(save_nass_path, "3/", state_abb, "/", state_abb, "_nass_", year, "_union.shp"),
    #   layer = paste0(state_abb, "_nass_", year, "_union")
    # )
  
      # dplyr::relocate(VALUE, croptype, class_area)
      # mapview::mapview(nass_shp2[1,]) +    mapview::mapview(nass_shp2[3,], col.regions = "red") + nass_shp2[7,] + nass_shp2[6,] + pt
    # 
   
    # nass_shp[37,]
    logger::log_info("\n\nSaving...")
   
    # sf::write_sf(
    sf::st_write(
      nass_shp,
      dsn   = paste0(save_nass_path, "4/", state_abb, "/", state_abb, "_nass_", year, ".shp"),
      # dsn   = paste0(save_nass_path, "4/", state_abb, "/", state_abb, "_nass_", year, ".gpkg"),
      layer = paste0(state_abb, "_nass_", year)
    )
    
    # Save table
    saveRDS(
      nass_table,
      paste0(save_nass_path, "_table/", state_abb, "_nass_table_", year, ".rds")
    )

    logger::log_info("\n\nRemoving nass shape")
    
    rm(nass_shp)
    
    logger::log_info("\n\nCalling garbage collector")
    
    gc()
    
  } else {
    
    vat_keep <- 
      vat_df %>% 
      dplyr::mutate(
        croptype = snakecase::to_snake_case(as.character(CLASS_NAME)),
        croptype = gsub("dbl_", "", croptype),
        croptype = gsub("developed_", "dev_", croptype),
        croptype = gsub("crop_", "", croptype)
      ) %>% 
      # dplyr::select(VALUE, croptype) %>% 
      dplyr::inner_join( 
        vat_classes, 
        by = c("croptype", "VALUE")
        ) 
    
    nass_shp <-
      nass_shp %>% 
      dplyr::relocate(VALUE, croptype = CLASS_NAME) %>%
      dplyr::mutate(
        croptype = snakecase::to_snake_case(as.character(croptype)),
        croptype = gsub("dbl_", "", croptype),
        croptype = gsub("developed_", "dev_", croptype),
        croptype = gsub("crop_", "", croptype)
      ) %>% 
      dplyr::left_join(
        dplyr::select(vat_keep, VALUE, croptype, keep_remove),
        by = c("croptype", "VALUE")
      ) %>% 
      dplyr::filter(keep_remove == "keep") %>% 
      dplyr::select(-keep_remove) %>% 
      sf::st_cast("POLYGON") %>%
      dplyr::mutate(
        class_area = as.numeric(sf::st_area(.))
      ) %>%
      sf::st_transform(4326)

    nass_table <- 
      nass_shp %>% 
      sf::st_drop_geometry() %>% 
      tibble::tibble()
    
    # hist(nass_table$class_area, breaks = 5)
    # sum(nass_table$class_area)
    # logger::log_info("\n\nMax area (sq. meters): {max(nass_shp2$class_area)}\nMin area (sq. meters): {min(nass_shp2$class_area)}")
    logger::log_info("\n\nMax area (sq. meters): {max(nass_shp$class_area)}\nMin area (sq. meters): {min(nass_shp$class_area)}")
    
    # length(unique(nass_table$croptype))
    # length(unique(dplyr::filter(nass_table, class_area >= 50000)$croptype))
    
    # hist(nass_shp$class_area, breaks = 100)
    nass_shp <- 
      nass_shp %>%
      dplyr::filter(class_area >= 50000) %>% 
      dplyr::select(-VALUE) %>% 
      sf::st_make_valid() %>%
      dplyr::group_by(croptype) %>%
      dplyr::summarise(geometry = sf::st_combine(geometry)) %>% 
      dplyr::ungroup()
    
    nass_shp <- 
      nass_shp %>% 
      dplyr::summarise(geometry = st_combine(geometry)) %>% 
      dplyr::mutate(croptype = paste0(state_abb, "_nass_", year))
    
    # nass_shp2 <- 
    #   nass_shp2 %>%
    #   dplyr::filter(class_area >= 50000) %>% 
    #   dplyr::select(croptype, geometry) 
    
    logger::log_info("\n\nSaving...")
    
    # sf::write_sf(
    sf::st_write(
      nass_shp,
      # dsn    = paste0(save_nass_path, "3/",state_abb, "/", state_abb, "_nass_", year, ".gpkg"),
      dsn   = paste0(save_nass_path, "4/", state_abb, "/", state_abb, "_nass_", year, ".shp"),
      layer  = paste0(state_abb, "_nass_", year)
    )
    # Save table
    saveRDS(
      nass_table,
      paste0(save_nass_path, "_table/", state_abb, "_nass_table_", year, ".rds")
    )
    
    logger::log_info("\n\nRemoving nass shape")
    
    rm(nass_shp)
    
    logger::log_info("\n\nCalling garbage collector")
    
    gc()
    # tmp <- sf::st_read("D:/wff/nass/shp2/AZ_nass_2013.gpkg")
    # sf::st_layers("D:/wff/nass/shp2/AZ_nass_2013.gpkg")
    # rm(tmp)
    # %>% dplyr::mutate(CLASS_NAME = as.factor(CLASS_NAME))
    
    # for (z in 1:nrow(nass_shp2)) {
    #   logger::log_info("\n\n{state_abb} - {year}\n{z} of {nrow(nass_shp2)}")
    #   shp <- nass_shp2[z, ]
    #   croptype_name <- shp$croptype[1]
    #   layer_opt <- c("RESIZE=yes")
    #   sf::st_write(
    #     shp,
    #     paste0("D:/wff/nass/individual_shp/", state_abb, "/", year, "/", 
    #            state_abb, "_nass_",croptype_name, "_",  year, ".shp"),
    #     layer_options = layer_opt,
    #     drivers= "ESRI Shapefile" )
    #   logger::log_info("\n\nCollecting garbage...")
    #   gc()
    # }
  }
  
  
}

# **********************************************************
# **********************************************************
nass <- sf::read_sf(paste0(save_nass_path, "3/NM/NM_nass_2018.gpkg"))
 # rm(st)
# st <- sf::read_sf(paste0(save_nass_path, "/", state_abb, "_nass_2018.gpkg"))
# **********************************************************
# **********************************************************
 # i <- 1

for (i in 1:length(nass_path)) {
  
  state_abb <- state_dirs[i]
  
  logger::log_info("\n\n{state_abb} NASS rasters")
  
  r_path <- paste0(
              nass_path[i], "/",
              grep(
                ".tif$",
                list.files(nass_path[i]),
                value = T
                )
            )
  
  vat_path <- paste0(
    nass_path[i], "/",
    grep(
      ".tif.vat",
      list.files(nass_path[i]),
      value = T
    )
  )
  
  #import all raster files in folder using lapply
  st <- lapply(r_path, raster) %>% 
    raster::stack()
  # st <- lapply(r_path, rast) %>%  terra::rast() %>% 
    # terra::rast() %>% 
    # terra::aggregate(fact = 50)
  
  years <- 2013:2020
  
  # z = 1
  
  for (z in 1:raster::nlayers(st)) {
  
      logger::log_info("\n\n{state_abb} - {years[z]}")

      data_dbf <- foreign::read.dbf(vat_path[z]) %>%
        tibble::tibble()
      
      r <- st[[z]]
      
      r[r == 0] <- NA
      
      # plot(r)
      nx <- 4
      ny <- 4
      
      # tmpdir <- file.path(tempdir(), "splitRaster-example")
      # SpaDES::split
      split_nass <- splitRaster(
        r,
        nx,
        ny
        # path = here::here("data/nass/tiles")
        ) # no buffer
      
      logger::log_info("\n\nConverting raster to polygons - {state_abb}")
      
      # k = 1
      for (k in 1:length(split_nass)) {
      # for (z in 1:terra::nlyr(st)) {
        
          # logger::log_info("\n\nState: {state_abb} \nYear:  {years[k]}")
          
          logger::log_info("\n\n{state_abb} - {years[z]}\nTile: {k}")
          
        r_tile <- terra::rast(split_nass[[k]])
          # data_dbf <- foreign::read.dbf(vat_path[z]) %>%
          #   tibble::tibble()
        
          # st[[z]]
          # levels(st[[z]]) <- data_dbf$VALUE
          # names(st[[z]])
          # st[[z]] <- round(st[[z]])
          
          tile_poly <-
            r_tile %>%
            terra::as.polygons() %>% 
            sf::st_as_sf() %>%
            setNames(c("VALUE", "geometry")) %>% 
            dplyr::left_join(
              dplyr::select(data_dbf, VALUE, CLASS_NAME),
              by = "VALUE"
            ) %>% 
            sf::st_cast("MULTIPOLYGON")
          
          saveRDS(
            tile_poly,
            paste0(
              here::here("data/nass/shp/"), "/", state_abb, "/", state_abb, 
              "_nass_geom_", k, "_", years[z], ".rds"
              )
            )
          # plot(tile_poly)
          # plot(st_poly)
          
          # sf_poly <-
          #   tile_poly %>% 
            # sf::st_as_sf() %>%
            # setNames(c("VALUE", "geometry")) %>% 
            # # sf::st_as_sf(crs = 5070) %>% 
            # # sf::st_transform(5070) %>% 
            # # dplyr::rename(VALUE = CLASS_NAME) %>% 
            # dplyr::left_join(
            #   dplyr::select(data_dbf, VALUE, CLASS_NAME),
            #   by = "VALUE"
            #   ) %>% 
            # sf::st_cast("MULTIPOLYGON")
          
          # ggplot() +
          #   geom_sf(data = sf_poly, aes(fill = CLASS_NAME)) + 
          #   theme(legend.position = "none")
      }
  }
  }


tile_paths <- paste0("data/nass/shp/AZ", "/", list.files("data/nass/shp/AZ", recursive = T))

nass_lst <- list()
# i <- 1
for (i in 1:length(tile_paths)) {
  
  logger::log_info("\n\n{i} of {length(tile_paths)}\n{tile_paths[i]}")
  
  # nass_poly <- readRDS(tile_paths[i])
  nass_lst[[i]] <- readRDS(tile_paths[i])
}

nass_df <- bind_rows(nass_lst)

nass_df %>% 
  group_by(CLASS_NAME) %>% 
  st_make_valid() %>% 
  summarize()






  data_dbf <- foreign::read.dbf(vat_path[i]) %>%
    tibble::tibble()
  30*15
  
  levels(st[[1]]) <- data_dbf$VALUE
  
  st1 <- st[[1]] %>% 
    terra::aggregate(fact = 50)
  plot(st[[1]])
  plot(st1)
  unique(terra::values(st[[1]]))
  unique(terra::values(st1))
  levels(st[[1]]) <- data_dbf$VALUE
  st_poly <-
    st1 %>%
    terra::as.polygons()
  
  sf_poly <-
    st_poly %>% 
    st_as_sf()
  plot(st_poly)
  plot(st_poly)
  plot(st[[1]])
  plot(st[[3]])
  rnass <- terra::rast()

  # st1 <- raster::raster(r_path[1], RAT = F)

  # unique(data_dbf$VALUE)
  # library(foreign)
  # st1 <- raster::raster(r_path[1], RAT = F)
  # data_dbf <- foreign::read.dbf(vat_path[1]) %>% 
  #   tibble::tibble()
  #import all raster files in folder using lapply
  # st <- lapply(r_path, rast) %>% 
  #   terra::rast()
  # 
  # rnass <- terra::rast()
  
  
# }
  # nass_no_geom <- sf::st_drop_geometry(nass_shp)
  # 
  # nass_shp_val <- sf::st_make_valid(nass_shp[37,])
  # sf::st_write(
  #   nass_shp_val,
  #   # paste0(save_nass_path, "/", state_abb, "_nass_", year, ".shp")
  #   dsn   = paste0(save_nass_path, "/", state_abb, "_nass_", year, "_layer37.gpkg"),
  #   layer = paste0(state_abb, "_nass_", year, "_layer37")
  #   # dsn   = paste0(save_nass_path, "/", state_abb, "_nass_", year, ".gpkg"),
  #   # layer = paste0(state_abb, "_nass_", year)
  # )
  # nass_shp_rm <-
  #   nass_shp %>% 
  #   dplyr::mutate(
  #     CLASS_NAME = case_when(
  #       is.na(CLASS_NAME) ~ "no_class_name",
  #       TRUE ~ CLASS_NAME
  #     )
  #   ) 
  # # j <- 14
  # # rm(j)
  # for (j in 1:nrow(nass_shp_rm)) {
  #   
  #   logger::log_info("\n\nSaving    --->    layer {j} of {nrow(nass_shp_rm)}")
  #   
  #   tmp_shp   <- nass_shp_rm[j,]
  #   shp_val   <- tmp_shp$VALUE
  #   
  #   shp_class <- gsub(
  #     "/",
  #     "_",
  #     gsub(" ", "_", tolower(tmp_shp$CLASS_NAME))
  #   )
  #   
  #   logger::log_info("\n\nCLASS_NAME: {shp_class}")
  #   logger::log_info("\n\nCLASS_NAME --->  {shp_class}\nVALUE      --->  {shp_val}")
  #   tmp_shp <- sf::st_make_valid(tmp_shp)
  #   sf::st_write(
  #     # nass_shp,
  #     tmp_shp,
  #     # paste0(save_nass_path, "/", state_abb, "_nass_", year, ".shp")
  #     dsn   = paste0(save_nass_path, "/", state_abb, "_nass_", year, "_", shp_class,".gpkg"),
  #     layer = paste0(state_abb, "_nass_", year, "_", shp_class)
  #   )
  #   
  #   rm(tmp_shp)
  #   
  #   
  # }
  # %>%
  #   dplyr::filter(CLASS_NAME != "Mixed Forest" )
  # nass_no_geom2 <- sf::st_drop_geometry(nass_shp_rm)


