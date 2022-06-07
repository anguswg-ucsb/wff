
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
# gc()
# *******************************
# ---- Union irrigated lands ----
# *******************************

# UCBR irrigated lands path
irrig_land_path <- "D:/wff/irrig_lands/sir2014_5039_UCRBAgriculture_v2.shp"

# UCBR irrigated lands
irrig_land <- sf::read_sf(irrig_land_path)

# Union gfeometries
irrig_land_u <-
  irrig_land %>%
  dplyr::summarise(geometry = sf::st_union(geometry))

# Save RDS 
saveRDS(irrig_land_u, "D:/wff/irrig_lands/irrig_land_union.rds")

rm(irrig_land, irrig_land_u)

# *********************
# ---- NASS - NWI  ----
# *********************

save_nass_path <- "D:/wff/nass/shp"

base_nwi_path <- "data/wetlands/buffer/final/"
nwi_path <-  paste0(base_nwi_path, list.files(base_nwi_path, pattern = ".shp"))[c(6)]

state_dirs <- list.files(here::here("data", "nass", "rasters"), recursive = F)[4]
nass_path  <- paste0(here::here("data", "nass", "rasters"), "/", state_dirs)
nass_path
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
new_crs <-  "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# st <- lapply(r_path, rast) %>%  
#   terra::rast() 
# library(USAboundaries)
# 
# counties <- us_counties()

# rm(buffer_path)
# states <- state.abb[c(3, 28, 31, 44)]
# i <- 1
# rm(i)
# tmp <- readRDS("data/nass/shp/AZ/AZ_nass_geom_2015.rds")
# plot(tmp[c(1:3),])

for (i in 1:length(nass_path)) {
  
  state_abb <- state_dirs[i]
  
  logger::log_info("\n\n{state_abb} NASS rasters")
  
  if (state_abb == "UT") {
    
    r_path <- paste0( nass_path[i], "/", grep( ".tif$",
        list.files(nass_path[i]),
        value = T) )
    vat_path <- paste0(
      nass_path[i], "/",grep( ".tif.vat",
        list.files(nass_path[i]),   value = T  ) )
    r_path   <- r_path[8]
    vat_path <- vat_path[8]
    
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
    # rm(nwi2, nwi, irrig_land)
    nwi <- terra::vect(nwi_path[i]) %>% 
      terra::project(new_crs)
    
    # irrig_land <- terra::erase(irrig_land_u, nwi)
    # nwi2 <- st_as_sf(nwi) %>% 
    #   mutate(area = st_area(.))
    # plot(irrig_land)
    # plot(irrig_land_u)
    # plot(nwi2$geometry, col = "red", add = T)
    # z <- 1
    for (z in 1:length(r_path)) {
        
        year <- substr(r_path[z], 69, 72)
        # year
        
        logger::log_info("\n\n{state_abb} - {year}")
        
        nass_class <- terra::rast(r_path[z]) 
        # nass_class2 <- terra::rast(r_path[2]) 
        
        logger::log_info("\n\nRemoving raster classes...")
        # unique(values(nass))
        nass_class  <- terra::ifel(nass_class %in% keep_classes$VALUE, 1, NA) %>% 
          terra::mask(
            nwi, inverse = T
          )

        # logger::log_info("\n\nMasking NWI veg buffer...")
        
        # nass_class <- 
        #   nass_class %>% 
        #   terra::mask(
        #     nwi, inverse = T
        #     )
        
        # plot(nass_class2$CLASS_NAME)
        logger::log_info("\n\nConverting NASS raster to polygon...")
        
        # raster to polygon
        nass_poly <-
          nass_class %>% 
          terra::as.polygons() %>% 
          sf::st_as_sf() %>%
          sf::st_cast("POLYGON") %>% 
          dplyr::mutate(class_area = as.numeric(sf::st_area(.))) %>%
          dplyr::filter(class_area >= 50000) %>%
          dplyr::select(-class_area) %>% 
          sf::st_make_valid() %>%
          dplyr::summarise(geometry = st_union(geometry)) %>% 
          sf::st_transform(4326) %>% 
          dplyr::mutate(nass_year = paste0(state_abb, "_nass_", year))

        
        logger::log_info("\n\nSaving {state_abb} NASS polygons")
        
        # sf::write_sf(
        sf::st_write(
          nass_poly,
          dsn   = paste0(save_nass_path, "_erase/", state_abb, "/", state_abb, "_nass_", year, "_erase.shp"),
          # dsn   = paste0(save_nass_path, "4/", state_abb, "/", state_abb, "_nass_", year, ".gpkg"),
          layer = paste0(state_abb, "_nass_", year, "_erase")
        ) 
        
        rm(nass_poly, nass_class)
        
        logger::log_info("\n\nCalling garbage collector...")
        
        gc()
    
    }
  
}

# ***************************
# ---- Irrig land - NWI  ----
# ***************************

# new CRS to project to
new_crs <- "epsg:4326"
# new_crs <-  "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

save_nass_path <- "D:/wff/nass/shp"

base_nwi_path <- "data/wetlands/buffer/final/"
nwi_path <-  paste0(base_nwi_path, list.files(base_nwi_path, pattern = ".shp"))
# nwi_path <-  paste0(base_nwi_path, list.files(base_nwi_path, pattern = ".shp"))[c(6)]

# UCBR Irrigated lands
irrig_land_path <- "D:/wff/irrig_lands/irrig_land_union.rds"
irrig_land <- terra::vect(readRDS(irrig_land_path)) %>% 
  terra::project(new_crs)

# state_dirs <- list.files(here::here("data", "nass", "rasters"), recursive = F)
# nass_path  <- paste0(here::here("data", "nass", "rasters"), "/", state_dirs)
# nass_path

# az_nwi <- terra::vect(nwi_path[1]) %>% 
#   terra::project(new_crs)
# ca_nwi <- terra::vect(nwi_path[2]) %>% 
#   terra::project(new_crs)
# co_nwi <- terra::vect(nwi_path[3]) %>% 
#   terra::project(new_crs)
# nm_nwi <- terra::vect(nwi_path[4]) %>% 
#   terra::project(new_crs)
# nv_nwi <- terra::vect(nwi_path[5]) %>% 
#   terra::project(new_crs)
# ut_nwi <- terra::vect(nwi_path[6]) %>% 
#   terra::project(new_crs)
# wy_nwi <- terra::vect(nwi_path[7]) %>% 
#   terra::project(new_crs)

# nwi_df <- rbind(az_nwi, ca_nwi, co_nwi, nm_nwi, nv_nwi, ut_nwi, wy_nwi)
# rm(az_nwi, ca_nwi, co_nwi, nm_nwi, nv_nwi, ut_nwi, wy_nwi)

# Erase NWI buffered polygons from UCBR irrigated lands polygon
# irrig_land <- terra::erase(irrig_land_u, nwi_df)

# i <- 1
# rm(nwi, i)
# nwi_lst <- list()

for (i in 1:length(nwi_path)) {
  
  state_abb <- substr(nwi_path[i], 28, 29)
  
  logger::log_info("\n\nNWI {state_abb} - {i} of {length(nwi_path)}")
  
  # rm(nwi2, nwi, irrig_land)
  nwi <- terra::vect(nwi_path[i]) %>% 
    terra::project(new_crs)
  
  logger::log_info("\n\nErasing NWI from Irrigated lands...")
  
  irrig_land <- terra::erase(irrig_land, nwi)
  # nwi_df <- rbind(nwi, nwi2)
  # nwi_lst[[i]] <- nwi
  
  rm(nwi)
  
  
}


# Save RDS & shapefile

saveRDS(sf::st_as_sf(irrig_land), "D:/wff/irrig_lands/irrig_land_erase.rds")

terra::writeVector(
  irrig_land, 
  "D:/wff/irrig_lands/irrig_land_erase.shp",
  filetype  = "ESRI Shapefile",
  overwrite = TRUE
  )

# *********************************
# ---- Check erased geometries ----
# *********************************
# New CRS 
new_crs <-  "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# NWI layer
base_nwi_path <- "data/wetlands/buffer/final/"
nwi_path <-  paste0(base_nwi_path, list.files(base_nwi_path, pattern = ".shp"))

nwi <- sf::read_sf(nwi_path[5])
nwi <- 
  nwi %>% 
  dplyr::mutate(area = sf::st_area(.))
sum(nwi$area)

plot(irrig_land_u$geometry,  col = "black", add = F)
plot(nwi$geometry,  col = "red",  add = T)
# nwi <- terra::vect(nwi_path[5]) %>% 
#   terra::project(new_crs)
# terra::erase()

# Irrigated lands (no erase)
irrig_land_u  <- sf::st_as_sf(readRDS(irrig_land_path))

# st_crs(irrig_land_u)
# irrig_land_u <- terra::vect(readRDS(irrig_land_path))
irrig_land_u <-
  irrig_land_u %>% 
  dplyr::mutate(area = sf::st_area(.))
irrig_land_u$area
# Irrigated lands (erase)
# irrig_land    <- sf::read_sf("D:/wff/irrig_lands/irrig_land_erase.shp") %>% 
#   terra::vect()
irrig_land    <- sf::read_sf("D:/wff/irrig_lands/irrig_land_erase.shp") %>% 
  sf::st_as_sf()

irrig_land <-
  irrig_land %>% 
  dplyr::mutate(area = sf::st_area(.))

sum(irrig_land$area)
sum(irrig_land_u$area)
plot(irrig_land_u$geometry)
plot(irrig_land$geometry,  col = "red", add = T)
plot(irrig_land$geometry)
plot(irrig_land_u$geometry,  col = "red", alpha = 0.5, add = T)
library(ggplot2)
ggplot() + 
  geom_sf(data = irrig_land_u, aes(fill = "black")) +
  geom_sf(data = irrig_land, aes(fill = "red"), alpha = 0.7) +
  theme(legend.position = "none")
irrig_land$area
plot(irrig_land)
plot(irrig_land_u$geometry,  col = "red", alpha = 0.4, add = T)
# plot(irrig_land_u)
# plot(irrig_land$geometry,  col = "black", add = F)
nwi <- terra::vect(nwi_path[5]) %>% 
  terra::project(new_crs)

plot(irrig_land)
plot(nwi, col = "red", add = T)
# gc()

# nwi_df <- bind_rows(nwi_lst)
  # irrig_land <- terra::erase(irrig_land_u, nwi)
  # nwi2 <- st_as_sf(nwi) %>% 
  #   mutate(area = st_area(.))
  # plot(irrig_land)
  # plot(irrig_land_u)
  # plot(nwi2$geometry, col = "red", add = T)
  # z <- 1
  for (z in 1:length(r_path)) {
    
    year <- substr(r_path[z], 69, 72)
    # year
    
    logger::log_info("\n\n{state_abb} - {year}")
    
    nass_class <- terra::rast(r_path[z]) 
    # nass_class2 <- terra::rast(r_path[2]) 
    
    logger::log_info("\n\nRemoving raster classes...")
    # unique(values(nass))
    nass_class  <- terra::ifel(nass_class %in% keep_classes$VALUE, 1, NA) %>% 
      terra::mask(
        nwi, inverse = T
      )
    
    # logger::log_info("\n\nMasking NWI veg buffer...")
    
    # nass_class <- 
    #   nass_class %>% 
    #   terra::mask(
    #     nwi, inverse = T
    #     )
    
    # plot(nass_class2$CLASS_NAME)
    logger::log_info("\n\nConverting NASS raster to polygon...")
    
    # raster to polygon
    nass_poly <-
      nass_class %>% 
      terra::as.polygons() %>% 
      sf::st_as_sf() %>%
      sf::st_cast("POLYGON") %>% 
      dplyr::mutate(class_area = as.numeric(sf::st_area(.))) %>%
      dplyr::filter(class_area >= 50000) %>%
      dplyr::select(-class_area) %>% 
      sf::st_make_valid() %>%
      dplyr::summarise(geometry = st_union(geometry)) %>% 
      sf::st_transform(4326) %>% 
      dplyr::mutate(nass_year = paste0(state_abb, "_nass_", year))
    
    
    logger::log_info("\n\nSaving {state_abb} NASS polygons")
    
    # sf::write_sf(
    sf::st_write(
      nass_poly,
      dsn   = paste0(save_nass_path, "_erase/", state_abb, "/", state_abb, "_nass_", year, "_erase.shp"),
      # dsn   = paste0(save_nass_path, "4/", state_abb, "/", state_abb, "_nass_", year, ".gpkg"),
      layer = paste0(state_abb, "_nass_", year, "_erase")
    ) 
    # saveRDS(
    #   tile_poly,
    #   paste0(
    #     here::here("data/nass/shp/"), "/", state_abb, "/", state_abb, 
    #     "_nass_geom_", year, ".rds"
    #   )
    # )
    # paste0(here::here("data/nass/shp/"), "/", state_abb, "/",
    #        state_abb,  "_nass_geom_", years[z], ".rds") )
    
    rm(nass_poly, nass_class)
    
    logger::log_info("\n\nCalling garbage collector...")
    
    gc()
    
  }
  
}
