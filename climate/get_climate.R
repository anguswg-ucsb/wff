# Angus Watters
# Retrieve climate data for NAIP tiles

rm(list = ls())

# Libraries
library(sf)
library(raster)
library(tidyverse)
library(mapview)
library(climateR)
library(AOI)
library(lubridate)
library(foreach)
library(doParallel)

# remotes::install_github("mikejohnson51/AOI") # suggested!
# remotes::install_github("mikejohnson51/climateR")
# .libPaths()   

source("utils/utils.R")

# NAIP Grid
grid <- sf::read_sf("data/grid/NAIP_Grid_CRB.shp") %>% 
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  # sf::st_transform(5070) %>%
  janitor::clean_names()

# grid Bounding box
bb_sf <-
  grid %>%
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf()
# mapview(bb_sf)

# empty raster
r <- raster(
  nrow = 8,
  ncol = 8,
  ext  = extent(bb_sf),
  crs  = 4326
)

# assign values 
values(r) <- 1:ncell(r)

# plot(r)
# mapview(r) + grid$geometry

# create polygon tiles to subset naip tiles
r_grid <- 
  r %>% 
  terra::rast() %>% 
  terra::as.polygons() %>% 
  sf::st_as_sf() %>% 
  sf::st_filter(grid)

# tmp <- r_grid[13,]
# mapview(r) + r_grid + grid

# NAIP info, Drop geometry
naip_table <-
  grid %>%
  sf::st_drop_geometry()

# NAIP Date Analysis
# naip_dates2 <- readxl::read_xlsx("data/naip_dates/NAIP Date Analysis.xlsx") %>%
#   janitor::clean_names()
# Save as RDS
# saveRDS(naip_dates, "data/naip_dates/naip_date_analysis.rds")

# Naip dates RDS
naip_dates <- readRDS("data/naip_dates/naip_date_analysis.rds") 

# Clean NAIP date info
clean_dates <- 
  naip_dates %>%
  filter(entity %in% naip_table$naip_entit) %>% 
  mutate(
    date       = as.Date(acquisition_date),
    month_date = lubridate::ceiling_date(date,'month')
    # month_date = lubridate::floor_date(date,'month')
    ) 

# join acquisition dates w/ NAIP grid
grid_dates <-
  grid %>%
  left_join(
    dplyr::select(clean_dates, entity,acquisition_date, date, month_date),
    by = c("naip_entit" = "entity")
  ) %>% 
  replace_na(
    list(
      acquisition_date = as.Date("2018-09-13 UTC"),
      date             = as.Date("2018-09-13"),
      month_date       = as.Date("2018-10-01")
      )
    ) 

# Unique shapes to iterate over
u_shp <- unique(r_grid$layer)

clim_params <- c("prcp", "tmax", "tmin")

# i = 3
# z = 1
# k = 1

subset_lst <- list()

for (i in 1:length(u_shp)) {
  
  shp_id <- r_grid[i,]$layer
  
  logger::log_info("Shape ID\n {shp_id}")
  
  shp <- r_grid[i,]
  
  # # NAIP tiles in polygon
  subset_naip <-
    grid %>%
    sf::st_filter(shp) %>%
    dplyr::select(cell_id, cell_map_1, naip_entit) %>% 
    dplyr::mutate(cell_id = as.character(cell_id))
  
  # logger::log_info("NAIP Entity\nCell map ID - {cell_map_id}")

  # Get acquisition dates for naip entities in subset
  subset_dates <-
    grid_dates %>% 
    dplyr::filter(naip_entit %in% subset_naip$naip_entit) %>% 
    dplyr::mutate(cell_id = as.character(cell_id))
  
  # Create starting dates 12 months back from acquistion dates (end_dates)
  
  # End dates
  end_dates    <- unique(lubridate::ceiling_date(unique(subset_dates$date), 'month'))
  # end_dates    <- lubridate::ceiling_date(unique(subset_dates$date), 'month')
  end_dates    <- na.omit(end_dates)
  
  # Start dates
  start_dates  <- end_dates %m-% months(13)
  start_dates  <- na.omit(start_dates)
  
  
  # date range to cover
  min_date <- min(start_dates)
  max_date <- max(end_dates)
  
  # date_table <- tibble(
  #   # aquisition_date = acq_dates, 
  #   start_dates     = start_dates,
  #   end_dates       = end_dates
  #   ) %>%
  #   na.omit()
  # acq_dates    <- unique(subset_dates$date)
  # acquisition_table <- tibble(
  #   aquisition_date = acq_dates,
  #   start_dates     = lubridate::ceiling_date(unique(subset_dates$date), 'month')  %m-% months(13),
  #   end_dates       = lubridate::ceiling_date(unique(subset_dates$date), 'month'))
  
  clim_lst <- list()
  
  for (z in 1:length(clim_params)) {
    
    param <- clim_params[z]
    
    logger::log_info("Climate variable --> {param}")
    
    # dates_lst <- list()
    # 
    # for (k in 1:length(end_dates)) {
    # 
    #   end   <- max_date 
    #   start <- min_date
      # end   <- end_dates[k]  
      # start <- start_dates[k]  
      
    # logger::log_info("Pulling {param} data... \nStart date: {start}\nEnd date: {end}")
    logger::log_info("Downloading {param} data... \nDate range: {min_date} --> {max_date}")
    
    terra <- get_terra(
      # shp       = naip_cells,
      shp        = subset_naip,
      param      = param,
      start_date = min_date,
      end_date   = max_date
    )
    # start_date = start, end_date = end )
      
      # mapview(subset_naip)
      # system.time(gridmet <- get_gridmet(  grid = subset_naip,
      #   param      = param,  start_date = start, end_date   = end))
      
      logger::log_info("Summarizing {param}...")
      # length(unique(subset_naip$cell_id))
      
     terra <-
       terra %>%
        dplyr::group_by(date, cell_id) %>%
        dplyr::mutate(
          cell_count = 1:n(),
          cell_count = max(cell_count),
          vmean      = mean(value, na.rm = T),
          # month      = lubridate::month(date),
          # year       = lubridate::year(date)
          ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(date, cell_id) %>%
        # dplyr::group_by(cell_id, month, year) %>%
        dplyr::summarize(
          value        = mean(vmean,  na.rm = T),
          cell_count   = mean(cell_count, na.rm = T)
        ) %>%
        dplyr::ungroup() %>%
        # dplyr::mutate(
        #   date = as.Date(paste0(year, "-", month, "-01"))
        # ) %>%
        dplyr::select(date, cell_id, cell_count, value) %>%
        setNames(c("date", "cell_id", "cell_count", paste0(param))) %>%
        tidyr::pivot_longer(cols = c(paste0(param))) %>%
        # dplyr::mutate(acq_period = k) %>%
        dplyr::left_join(
          sf::st_drop_geometry(subset_naip),
          by = "cell_id"
          ) %>%
       dplyr::relocate(date, cell_id, cell_count, naip_entit, cell_map_1, name, value)
        # dplyr::relocate(date, acq_period, cell_id, cell_count, naip_entit, cell_map_1, name, value)
     #  
     #  dates_lst[[k]] <- terra
    # }
     
    clim_lst[[z]] <-  terra
  }
  
  logger::log_info("Binding rows...")
  logger::log_info("Shape {shp_id} done!")
  
  cell_clim <-
    clim_lst %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(layer = shp_id)
  
  subset_lst[[i]] <-  cell_clim
}
subset_df <- bind_rows(subset_lst)

saveRDS(subset_df, "data/climate/naip_climate.rds")

subset_df <- readRDS("data/climate/naip_climate.rds")

subset_wide <- 
  subset_df %>% 
  pivot_wider(
    values_from = value,
    names_from  = name
  )
subset_trim <-
  subset_wide %>% 
  group_by(cell_id, naip_entit, date) %>% 
  summarize(
    prcp = mean(prcp, na.rm =T), 
    tmax = mean(tmax, na.rm =T), 
    tmin = mean(tmin, na.rm =T)
  )

# rm(subset_wide)

dates_df <- 
  grid_dates %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(naip_entit, acquisition_date, date, month_date) 
# %>% # group_by(naip_entit)%>%  group_split()

# naip_df <- bind_rows(naip_lst)

# "3611512_se"
# naip_lst <- list()

# Remove NA and gemetry from NAIP Grid
sub_grid <- 
  grid_dates %>% 
  dplyr::filter(!is.na(naip_entit)) %>% 
  sf::st_drop_geometry() 
# %>% dplyr::slice(1:57)

# Remove NA from climate variables
sub_naip <- 
  subset_trim %>% 
  dplyr::filter(!is.na(naip_entit))

# %>%  # dplyr::filter(naip_entit %in% sub_grid$naip_entit)

u_id <- unique(sub_grid$naip_entit)

# unique(sub_naip$naip_entit)
# unique(sub_grid$naip_entit)
# unique(sub_naip$naip_entit) %in% unique(sub_grid$naip_entit)

naip_lst <- list()
cores = parallel::detectCores()
cl    <- parallel::makeCluster(cores[1]-4) #not to overload your computer
doParallel::registerDoParallel(cl)

# for (i in 1:length(u_id)) {
naip_loop <- foreach(i = 1:length(u_id), .combine = "rbind", .packages = c("dplyr", "lfstat", "lubridate")) %dopar% {
  
  cell <- u_id[i]
  
  # logger::log_info("Cell ID: {cell}\n Calculating 3, 6, 9, 12 month and water year means for precip, tmax, and tmin")
  # # 
  # ipct <- round(100*(i/length(u_id)), 2)
  # logger::log_info("{ipct} %")
  
  climate_df <- 
    sub_naip %>% 
    dplyr::filter(naip_entit == cell)
  
  # subset_trim %>% 
  

  # climate_df <- tmp 
  acq_dates <- 
    sub_grid %>% 
    dplyr::filter(naip_entit == cell) %>%
    dplyr::select(naip_entit, acquisition_date, date, month_date) 
  
  u_date     <- sort(unique(acq_dates$month_date))
  u_acq_date <- sort(unique(acq_dates$acquisition_date))
  
  cell_lst <- list()
  
  for (z in 1:length(u_date)) {

    # end <- date_table$end[z]
    
    end        <- u_date[z]
    start      <- end  %m-% months(13)
    a_date     <- u_acq_date[z]
    water_year <- as.character(lfstat::water_year(end))

    # zpct <- round(100*(z/length(u_date)), 2)
    # logger::log_info("Acquisition date: {a_date} - {z} of {length(u_date)}")
    
    df <-
      climate_df %>% 
      dplyr::filter(date <= end, date >= start) %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::mutate(
        prcp_csum  = cumsum(prcp),
        tmax_csum  = cumsum(tmax),
        tmin_csum  = cumsum(tmin),
        m          = 1:dplyr::n()
        ) %>% 
      dplyr::mutate(
        prcp = prcp_csum/m,
        tmax = tmax_csum/m,
        tmin = tmin_csum/m
          ) %>% 
      dplyr::slice(c(4, 7, 10, 13)) %>% 
      dplyr::mutate(
        acquisition_date = a_date,
        period           = c("month_3", "month_6", "month_9", "month_12")
        ) %>% 
      dplyr::mutate(acq_date_id = z) %>% 
      dplyr::select(cell_id, naip_entit, acq_date_id, acquisition_date, date, period, prcp, tmax, tmin) 
    
    wyear_df <-
      climate_df %>% 
      dplyr::filter(date <= end, date >= start) %>% 
      dplyr::arrange(dplyr::desc(date)) %>% 
      dplyr::mutate(
        wyear      = lfstat::water_year(date)
      ) %>% 
      dplyr::filter(wyear == water_year) %>% 
      dplyr::group_by(cell_id, naip_entit) %>% 
      dplyr::summarize(
        prcp = mean(prcp, na.rm = T),
        tmax = mean(tmax, na.rm = T),
        tmin = mean(tmin, na.rm = T)
        ) %>% 
      dplyr::mutate(
        acquisition_date = a_date,
        date             = end,
        period           = c("water_year")
        ) %>% 
      dplyr::mutate(acq_date_id = z) %>% 
      dplyr::select(cell_id, naip_entit, acq_date_id, acquisition_date, date, period, prcp, tmax, tmin) 
      
    # Cell averages
      cell_avgs <- dplyr::bind_rows(df, wyear_df)
    
    cell_lst[[z]] <- cell_avgs  
  }
  
  final <- dplyr::bind_rows(cell_lst)
  final
  
  # naip_lst[[i]] <- dplyr::bind_rows(cell_lst)
  
  # logger::log_info("Cell ID: {cell}\n Done!")
  
  
}

saveRDS(naip_loop, "data/final/naip_tile_means.rds")

doParallel::stopImplicitCluster()
parallel::stopCluster(cl)

naip_loop <- readRDS("data/final/naip_tile_means.rds")

naip_period <-
  naip_loop %>%
  dplyr::select(-date) %>% 
  ungroup() %>% 
  # filter(acq_date_id == 1) %>% 
  pivot_longer(cols = c(prcp, tmax, tmin))

naip_wide <-
  naip_period %>% 
  pivot_wider(
    # id_cols     = c(-naip_entit),
    names_from  = c(period, name),
    names_glue  = "{name}_{period}",
    values_from = c(value)
  )


saveRDS(naip_wide, "data/final/naip_climate_wide.rds")
naip_wide


# rm(naip_loop, final, z, i, cell_avgs, cell_lst, bf)
# length(unique(naip_loop$naip_entit))
# unique(naip_loop$naip_entit) %in% unique(sub_naip$naip_entit)
    # arrange(cell_count, .by_group = T)
# shp_id <- naip_polys[i,]$layer
# Cell map 1 column for grouping 4 NAIP tiles
# cell_map_id <- grid_split[[i]]$cell_map_1[1]
# logger::log_info("NAIP Entity\nCell map ID - {cell_map_id}")
# filter grid to NAIP entities in each cell map ID
# naip_cells <-     
#   grid %>% 
#   filter(cell_map_1 == cell_map_id)
# # filter(cell_map_1 == "31109-G2")
# 
# acq_dates <- grid_split[[i]]
# 
# # Create starting dates 12 months back from acquistion dates (end_dates)
# end_dates   <- unique(acq_dates$month_date)
# start_dates <- end_dates %m-% months(12)

# date_table <- tibble(
#   start_dates = start_dates,
#   end_dates   = end_dates
# )

library(terra)

grid <- sf::read_sf("data/grid/NAIP_Grid_CRB.shp") %>% 
  sf::st_transform(4326) %>%
  sf::st_cast("MULTIPOLYGON") %>%
  # sf::st_transform(5070) %>%
  janitor::clean_names()

bb_sf <-
  grid %>%
  sf::st_bbox() %>% 
  sf::st_as_sfc() %>%
  sf::st_as_sf()

mapview(bb_sf)

climateR::getGridMET()
get_gridmet()
ext <- extent(c(-1808423, -809561.1,  1109339,  2012884 ))
r <- raster(
  # values = 1:nrow(grid),
  nrow = 8, ncol = 8,
  # res = c(100000, 100000),
  # ext = ext,
  ext = extent(bb_sf),
  crs = 4326
)

values(r) <- 1:ncell(r)

plot(r)
mapview(r) + grid$geometry

# create polygon tiles to subset naip tiles
r_grid <- 
  r %>% 
  terra::rast() %>% 
  terra::as.polygons() %>% 
  sf::st_as_sf()

  # st_as_sf(crs = 5070)
tmp <- r_grid[13,]
# mapview(r) + r_grid + tmp
subset_naip <- 
  grid %>% 
  sf::st_filter(tmp) %>% 
  dplyr::select(cell_id, cell_map_1, naip_entit)



# matrix(rnorm(400),20,20)
r
6*3
18*3
54*4536
244944/60
4082/60
ind_grid <- 
  grid %>% 
  filter(cell_map_1 == "31109-G2")
start_date <- min(grid_dates$month_date) %m-% months(12)
# end_date   <- max(state_grid$month_date)
end_date <- min(grid_dates$month_date) %m-% months(11)
# tools::package_dependencies(recursive = TRUE)$climateR
clim_params <- c("prcp", "tmax", "tmin")
length(unique(grid_dates$cell_map_1))
# tile_ids    <- grid$CELL_ID
# climate_lst <- list()
tile_lst <- list()

z <- 2
i <- 1
grid_pts <- 
  grid %>% 
  group_by(naip_entit) %>% 
  st_centroid() %>% 
  group_split()
# for (z in 1:length(tile_ids)) {
for (z in 1:length(grid2)) {
  
  # grid2[z]
  
  # climate_lst <- list()
  
  # state_name <- grid2[[z]]$PRIMARY_ST[1]
  state_grid <-
    grid %>%
    left_join(
      dplyr::select(clean_dates, entity,acquisition_date, date, month_date),
      by = c("naip_entit" = "entity")
    ) %>% 
    filter(primary_st == "Colorado")
  
  start_date <- min(state_grid$month_date) %m-% months(12)
  # end_date   <- max(state_grid$month_date)
  end_date <- min(state_grid$month_date) %m-% months(11)
  # end_date   <- max(state_grid$month_date)
  
  logger::log_info("Downloading GridMET data\nArea: {state_name}")
  
  for (i in 1:length(clim_params)) {
    
    logger::log_info("{clim_params[i]}")
    
    gridmet <- get_gridmet(
      grid       = state_grid, 
      # grid       = grid2[[z]], 
      # tile       = tile_ids[z],
      param      = clim_params[i],
      start_date = start_date,
      end_date   = end_date
    )  
    
    vals <- gridmet %>%
      group_by(date, cell_id) %>% 
      summarize(val = mean(value, na.rm = T))
    
    gridmet <-
      gridmet %>% 
      dplyr::mutate(
        PRIMARY_ST       = state_name,
        climate_variable = clim_params[i]
        )
    
    tile_summary <- 
    length(unique(gridmet$CELL_ID))
    
    climate_lst[[i]] <- gridmet
    
  }

  tile_lst[[z]] <- bind_rows(climate_lst)
  
}
 
df <- tibble(
  x = 1:100
  ) %>% 
  mutate(
    cuts =  as.numeric(cut(x, breaks = seq(1, 100, by = 10), right=F))
    ) %>% 
  group_by(cuts) %>% 
  group_split()
df[[5]]
  group_by(gr = cut(x, breaks = seq(1, 1000, by = 100), right=F)) 
  tmp <- cut(df$x, breaks = seq(0, 1000, by = 100), right=T)
  tmp <- as.numeric(tmp)
  prcp <- 
  climate_lst %>% 
  bind_rows() %>% 
  filter(climate_variable == "prcp")










