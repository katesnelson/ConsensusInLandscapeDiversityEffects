## Process Item #4
# Description: Script to prep the panel data for INLA before running sensitivity analysis models
# Author: Kate Nelson
# Date updated: 01/09/2023


library(pacman)
p_load(tidyverse, sf, foreach, tigris, INLA, spdep) #spdep, rgeos,
wd<-getwd()
source(paste0(wd,'/Scripts/SA_functions_02122024-0.R'))




# Stuff needed for data prep ----

    projection <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs" #102003 epsg code for USA albers equal area conic
    
    
    crop_panels <- c("corn", "soy","wwheat", "alfalfa",  "hay")
    
    
    counties <- tigris::counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL)
    
        data(fips_codes)
        
        fc <- as.data.frame(fips_codes) %>%
          dplyr::filter(!state_name %in% c("Alaska", "Hawaii","Puerto Rico", "American Samoa",
                                           "Guam", "Northern Mariana Islands", "U.S. Minor Outlying Islands",
                                           "U.S. Virgin Islands" )) 
        
        counties <- counties %>%
          filter(STATEFP %in% fc$state_code) #get rid of places not in the lower 48 states
    
    
        counties <- st_transform(counties, projection)
        
        
    
    # AE <- readRDS(paste0(wd,"/StartingData/us_eco_l3.rds"))
    
        cnty_AER <- readRDS("D:/StartingData/cnty_AER.RDS") #created using area.overlay(counties, AE, projection, "cnty_AER")
        
        cnty_AER <- st_transform(cnty_AER, projection)
    
        
    metrics <- c(# Metrics using original CDL
                "RICH_CDL_BBOX_AG",   "RICH_CDL_BBOX_ALL",  "RICH_CDL_AG",        "RICH_CDL_ALL",
                 "SDI_CDL_BBOX_AG",     "SDI_CDL_BBOX_ALL",  "SDI_CDL_AG",         "SDI_CDL_ALL",
                 "SIDI_CDL_BBOX_AG",   "SIDI_CDL_BBOX_ALL" , "SIDI_CDL_AG",        "SIDI_CDL_ALL",      
                 "RPR_CDL_BBOX_AG",    "RPR_CDL_BBOX_ALL",   "RPR_CDL_AG",         "RPR_CDL_ALL",
                 "PRD_CDL_BBOX_AG",    "PRD_CDL_BBOX_ALL",   "PRD_CDL_AG",         "PRD_CDL_ALL",
                "D_CDL_BBOX_AG",    "D_CDL_BBOX_ALL",   "D_CDL_AG",         "D_CDL_ALL",
                 #Config metrics 
                 "ED_CDL_BBOX_AG",    "ED_CDL_BBOX_ALL",   "ED_CDL_AG",         "ED_CDL_ALL",
                 "CONTAG_CDL_BBOX_AG",    "CONTAG_CDL_BBOX_ALL",   "CONTAG_CDL_AG",         "CONTAG_CDL_ALL",
                 "MA_CDL_BBOX_AG",    "MA_CDL_BBOX_ALL",   "MA_CDL_AG",         "MA_CDL_ALL",
                 "LPI_CDL_BBOX_AG",    "LPI_CDL_BBOX_ALL",   "LPI_CDL_AG",         "LPI_CDL_ALL",
                  #Reclassified land cover metrics
                 "RICH_RC_BBOX_AG",   "RICH_RC_BBOX_ALL",  "RICH_RC_AG",        "RICH_RC_ALL",
                 "SDI_RC_BBOX_AG",     "SDI_RC_BBOX_ALL",  "SDI_RC_AG",         "SDI_RC_ALL",
                 "SIDI_RC_BBOX_AG",   "SIDI_RC_BBOX_ALL" , "SIDI_RC_AG",        "SIDI_RC_ALL",      
                 "RPR_RC_BBOX_AG",    "RPR_RC_BBOX_ALL",   "RPR_RC_AG",         "RPR_RC_ALL",
                 "PRD_RC_BBOX_AG",    "PRD_RC_BBOX_ALL",   "PRD_RC_AG",         "PRD_RC_ALL",
                "D_RC_BBOX_AG",    "D_RC_BBOX_ALL",   "D_RC_AG",         "D_RC_ALL", 
                #Config metrics 
                 "ED_RC_BBOX_AG",    "ED_RC_BBOX_ALL",   "ED_RC_AG",         "ED_RC_ALL",
                 "CONTAG_RC_BBOX_AG",    "CONTAG_RC_BBOX_ALL",   "CONTAG_RC_AG",         "CONTAG_RC_ALL",
                 "MA_RC_BBOX_AG",    "MA_RC_BBOX_ALL",   "MA_RC_AG",         "MA_RC_ALL",
                 "LPI_RC_BBOX_AG",    "LPI_RC_BBOX_ALL",   "LPI_RC_AG",         "LPI_RC_ALL")
    
    
  
    master_lm <- readRDS("D:/StartingData/master_lm_04032024.rds")



# Prep full dataset of each crop for INLA ----
    
    
    lm <- master_lm %>%
      distinct() %>% 
      mutate(YEAR = as.numeric(YEAR))
    
    
    
    foreach (i=1:length(crop_panels))%do%{
      
      df <- readRDS(paste0(wd,"/StartingData/",crop_panels[i],".RDS")) %>%  #read in the data with yield, weather, control, and landscape metrics
        mutate(CNTY = as.character(GEOID),
               CROPAREA = .[,52]) #get the crop-specific area column and give it a name consistent across panels
      
      
      df_new_met <- df %>%
        select(-(RICH_CDL_BBOX_AG:RPR_CDL_ALL)) %>%
        left_join(., lm, by = c("CNTY", "YEAR"))
      
      df_spat <- counties %>%
        left_join(. , df_new_met, by = c("GEOID" = "CNTY"))
      
      df_aer <- df_spat %>%
        left_join(.,st_set_geometry(cnty_AER[,c(1,2)], NULL), by='GEOID')
      
      saveRDS(df_aer, paste0(wd, "/StartingData/",crop_panels[i],"_full_raw.RDS")) # save the full crop specific data with all metrics and spatial info 
    
    
      prep.data.std.new(data = df_aer, metrics = metrics, 
                        savename=paste0(crop_panels[i],"_ntiles_04032024"),
                        projection="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m no_defs") #102003 epsg code for USA albers equal area conic

    }  
    
    
    # r <- readRDS("PreppedData/corn_ntiles_02142024.rds") #check data



