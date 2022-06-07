# Angus Watters
# Get mean elevation data for each NAIP tile

# notes:
# - results generate should be summarized on the basis of "NAIP_Entity" (eg 3610745_nw) and year of NAIP flight.
# - For NWM Flow, results are on a HUC12 basis, identify HUC12 w/ most area within each NAIP cell & link the NAIP grid to HUC12

rm(list = ls())

# Libraries
library(sf)
library(tidyverse)
library(mapview)
library(nhdplusTools)
library(lubridate)
library(foreach)

source("utils/utils.R")

# NAIP Grid
# grid <- sf::read_sf("data/grid/NAIP_Grid_CRB.shp") %>% 
#   sf::st_transform(4326) %>%
#   sf::st_cast("MULTIPOLYGON") %>% 
#   janitor::clean_names()

# NAIP Grid
# grid <- readRDS("data/grid/naip_huc_grid.rds") %>% 
#   dplyr::mutate(cell_id = as.character(CELL_ID)) %>% 
#   dplyr::select(-CELL_ID)
  
grid <- readRDS("data/final/naip_grid_stats.rds")

nwm <- readr::read_csv("data/nwm/NWM_month_max.csv") %>% 
  dplyr::mutate(huc12 = as.character(huc12)) %>% 
  mutate(
    date = lubridate::floor_date(time, "month")
  )

huc_dates <- 
  grid %>% 
  # group_by(huc12, acquisition_date) %>%
  group_by(huc12, acquisition_date, acq_date_id) %>%
  summarise() %>% 
  ungroup() %>% 
  mutate(
    date = lubridate::floor_date(acquisition_date, "month"),
    # date = lubridate::ceiling_date(acquisition_date,'month')
  ) 
# %>% 
#   slice(1:100)
# Start dates
# start_dates  <- end_dates %m-% months(13)
# start_dates  <- na.omit(start_dates)


# date range to cover
# min_date <- min(start_dates)
# max_date <- max(end_dates)

u_huc <- unique(huc_dates$huc12)

length(unique(huc_dates$huc12))
length(unique(grid$huc12))

# i <- 3
# Setup parallel processing
cores <-  parallel::detectCores()
cl    <- parallel::makeCluster(cores[1]-4) #not to overload your computer
doParallel::registerDoParallel(cl)


# huc_stats_lst <- list()
# i <- 1
# z <- 1

huc_loop <- foreach::foreach(i = 1:length(u_huc), .combine = "rbind", .packages = c("dplyr", "lfstat", "lubridate")) %dopar% {
# for (i in 1:length(u_huc)) {
  
  huc <- u_huc[i]

  # logger::log_info("Calculating HUC12 Max flow monthly means and water year means")
  # logger::log_info("HUC12: {huc} - {i} of {length(u_huc)}")

  # Subset NWM flows to HUC12
  nwm_huc <- 
    nwm %>%
    dplyr::filter(huc12 == u_huc[i])
  
  # Acquisition dates for HUC12 subset
  times <- 
    huc_dates %>%
    dplyr::filter(huc12 == u_huc[i])
  
  # Start dates
  acq_dates    <- sort(unique(times$date))
  
  # Empty list to iteratively add HUC12 stats too
  huc_lst <- list()
  
  # logger::log_info("Acquisition date:")
  
  for (z in 1:length(acq_dates)) {
    
    # End dates
    end_dates    <- acq_dates[z]
    
    # end_dates    <- lubridate::ceiling_date(unique(subset_dates$date), 'month')
    end_dates    <- na.omit(end_dates)

    start_dates  <- end_dates %m-% months(13)
    start_dates  <- na.omit(start_dates)
    
    water_year <- as.character(lfstat::water_year(end_dates))
    
    lookback <-
      nwm_huc %>% 
      dplyr::filter(date <= end_dates, date >= start_dates)
      # filter(date < end_dates, date >= start_dates)
    
    # Start dates
    a_date    <- sort(unique(times$acquisition_date))[z]
    
    # zpct <- round(100*(z/length(acq_dates)), 2)
    # logger::log_info("{z} of {length(acq_dates)}")
    
    df <-
      lookback %>% 
      dplyr::arrange(desc(date)) %>% 
      dplyr::mutate(
        max_flow_csum  = cumsum(monthly_max_flow),
        m              = 1:dplyr::n()
      ) %>% 
      dplyr::mutate(
        max_flow = max_flow_csum/m
      ) %>%
      dplyr::slice(c(3, 6, 9, 12)) %>%
      # dplyr::slice(c(4, 7, 10, 13)) %>%
      dplyr::mutate(
        acquisition_date = a_date,
        period           = c("month_3", "month_6", "month_9", "month_12")
      ) %>% 
      dplyr::mutate(acq_date_id = z) %>%
      dplyr::select(huc12, acq_date_id, acquisition_date, date, period, max_flow)
    
    
    wyear_df <-
      lookback %>% 
      # dplyr::filter(date <= end, date >= start) %>% 
      dplyr::arrange(dplyr::desc(date)) %>% 
      dplyr::mutate(
        wyear      = lfstat::water_year(date)
      ) %>% 
      dplyr::filter(wyear == water_year) %>% 
      dplyr::group_by(huc12) %>%
      dplyr::summarize(
        max_flow = mean(monthly_max_flow, na.rm = T)
      ) %>% 
      dplyr::mutate(
        acquisition_date = a_date,
        date             = end_dates,
        period           = c("water_year")
      ) %>% 
      dplyr::mutate(
        acq_date_id = z
        ) %>% 
      dplyr::select(huc12, acq_date_id, acquisition_date, date, period, max_flow) 
    
    # Cell averages
    huc_avgs <- dplyr::bind_rows(df, wyear_df)
    
    huc_lst[[z]] <- huc_avgs  
    
    
  }
  # huc_stats_lst[[i]] <- dplyr::bind_rows(huc_lst)
  final <- dplyr::bind_rows(huc_lst)
  final
}

# Save HUC12 means
saveRDS(huc_loop, "data/final/huc_flow_means.rds")

# bf <- bind_rows(huc_stats_lst)

doParallel::stopImplicitCluster()
parallel::stopCluster(cl)

huc_period <-
  huc_loop %>%
  dplyr::select(-date) %>% 
  ungroup() %>% 
  # filter(acq_date_id == 1) %>% 
  pivot_longer(cols = c(max_flow))

huc_wide <-
  huc_period %>% 
  pivot_wider(
    # id_cols     = c(-naip_entit),
    names_from  = c(period, name),
    names_glue  = "{name}_{period}",
    values_from = c(value)
  )

# Save HUC12 means
saveRDS(huc_wide, "data/final/huc_flow_means_wide.rds")












