# Angus Watters
# Join climate means, elevation, NWM, and HUC12 information w/ NAIP tiles

# notes:
# - results generate should be summarized on the basis of "NAIP_Entity" (eg 3610745_nw) and year of NAIP flight.
# - For NWM Flow, results are on a HUC12 basis, identify HUC12 w/ most area within each NAIP cell & link the NAIP grid to HUC12

rm(list = ls())

# Libraries
library(sf)
library(raster)
library(tidyverse)
library(mapview)
library(climateR)
library(zonal)
library(elevatr)
library(nhdplusTools)

source("utils/utils.R")

# NAIP Grid
grid <- readRDS("data/grid/naip_huc_grid.rds") %>% 
  dplyr::mutate(cell_id = as.character(CELL_ID)) %>% 
  dplyr::select(-CELL_ID)

# Elevation 
elev <- readRDS("data/elevation/naip_elevation.rds")

# climate
climate <- readRDS("data/final/naip_climate_wide.rds")

# Join climate, huc12, elevation to same grid
grid_join <- 
  climate %>% 
  dplyr::left_join(
    elev, 
    by = "cell_id"
  ) %>% 
  dplyr::left_join(
    dplyr::select(sf::st_drop_geometry(grid), cell_id, huc12), 
    by = "cell_id"
    # by = c("CELL_ID" = "cell_id")
  ) %>% 
  dplyr::relocate(cell_id, naip_entit, huc12, elevation, acquisition_date, acq_date_id)

# Save Joined climate, elevation NAIP grid
saveRDS(grid_join, "data/final/naip_grid_stats.rds")

# HUC flow summaries
grid_flow <- readRDS("data/final/huc_flow_means_wide.rds")

# length(unique(grid_flow$huc12))
# length(unique(grid_join$huc12))
# length(unique(grid_flow$acquisition_date))
# length(unique(grid_join$acquisition_date))

# Join climate, elevation and HUC flow summaries
final_grid <-
  grid_join %>%  
  dplyr::left_join(
    dplyr::select(grid_flow, -acq_date_id),
    by = c("huc12", "acquisition_date")
    ) %>% 
  dplyr::relocate(
    CELL_ID = cell_id, NAIP_ENTIT = naip_entit, huc12,   acquisition_date,  acq_date_id,   elevation,
    prcp_month_3,     prcp_month_6,      prcp_month_9,     prcp_month_12,     prcp_water_year, 
    tmax_month_3,     tmax_month_6,      tmax_month_9,     tmax_month_12,     tmax_water_year, 
    tmin_month_3,     tmin_month_6,      tmin_month_9,     tmin_month_12,     tmin_water_year, 
    max_flow_month_3, max_flow_month_6,  max_flow_month_9, max_flow_month_12, max_flow_water_year
    ) %>% 
  dplyr::mutate(dplyr::across(where(is.numeric), round, 4))


# rm(grid, climate, elev, grid_flow)

# Save Joined climate, elevation NAIP grid
saveRDS(final_grid, "data/final/naip_summary_stats.rds")
# (final_grid, "data/final/final_naip_grid.rds")

readr::write_csv(final_grid, "data/final/naip_summary_stats.csv")

tmp <- readr::read_csv( "data/final/naip_summary_stats.csv")




