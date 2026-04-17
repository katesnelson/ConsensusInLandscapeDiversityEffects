
## Process Item #4
# Author: Kate Nelson
# Last Modified Date: 08/07/2023
# Description: Script to read in landscape metric data based on reclassified land use and build national panel dataset.

####################
### I don't think I need this 
#######################


library(pacman)
p_load(tidyverse, sf, raster, terra, landscapemetrics, foreach, parallel, doParallel)

#################
### Get data ###
################

reclassed_files <- list.files("Metrics/", pattern = "reclass_[0-9][0-9]")


# Check a few files for completeness
d1 <- readRDS('Metrics/reclass_01_bbox_all_2008.rds')
  glimpse(d1)
d2 <- readRDS('Metrics/reclass_56_bbox_all_2008.rds')
  glimpse(d2)
d3 <- readRDS('Metrics/reclass_48_bbox_all_2008.rds')
  glimpse(d3)
d4 <- readRDS('Metrics/reclass_bdry_ag_58.rds')
glimpse(d4)
length(unique(d4$year))

# Pick a version of metric and loop through files and combine
  
# keep_files <- list.files("Metrics/", pattern = "bdry")
# reclassed_files <- reclassed_files[reclassed_files %in% keep_files]   

output <- foreach(i=1:length(reclassed_files), .combine = 'rbind') %do% {
    
    add_dat <- readRDS(paste0("Metrics/",reclassed_files[i]))  # read in ith file
    add_dat <- add_dat %>%
      rename(CNTY = cty_list.k.) %>%
      mutate(STATE = substr(reclassed_files[i], 9,10),
             BNDRY = substr(reclassed_files[i], 12,15),
             EXTENT = substr(reclassed_files[i], 17,19))
 
}

saveRDS(output, "StartingData/all_reclassed_metrics.rds")
    
   
#######################################################
### Merge with original crop-specific metric panels ###
######################################################

#prep output for "wide" panel format

new_df <- output %>%
  pivot_wider(names_from = c(BNDRY:EXTENT), values_from = ed:prd)

new_df %>% 
   summarise_all(., ~sum(is.na(.)))


saveRDS(new_df, "StartingData/reclassed_metrics_panel.rds")




crop_panels <- c("corn", "soy","wwheat", "alfalfa","hay")

full <- foreach (i=1:length(crop_panels), .combine = 'bind_rows')%do%{
  
  df <- readRDS(paste0("StartingData/",crop_panels[i],".RDS")) 
  df <- df %>%
    mutate(GEOID = as.character(GEOID))
  
  # full <- bind_rows(full, df)
  
}

glimpse(full)

glimpse(new_df)



