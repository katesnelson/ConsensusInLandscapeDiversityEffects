## Process Item #3
# Description: Script to prep the final landscape metric data for sensitivity analysis models
# Author: Kate Nelson
# Date updated: 02/09/2024



library(pacman)
p_load(tidyverse, sf, foreach, tigris, INLA) #spdep, rgeos,
wd<-getwd()
source(paste0(wd,'/Scripts/SA_functions.R'))



#Combine Landscape Metric Data ----

#original data 
  files<-list.files("Metrics/", pattern = "CDL.+bdry_ag")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_CDL_AG", "CONTAG_CDL_AG", "MA_CDL_AG", "LPI_CDL_AG", 
               "SDI_CDL_AG", "SIDI_CDL_AG", "RICH_CDL_AG", "RPR_CDL_AG", 
               "PRD_CDL_AG", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/orig_bdryag.rds")
  
  
  
  files<-list.files("Metrics/", pattern = "CDL.+bbox_ag")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_CDL_BBOX_AG", "CONTAG_CDL_BBOX_AG", "MA_CDL_BBOX_AG", "LPI_CDL_BBOX_AG", 
                   "SDI_CDL_BBOX_AG", "SIDI_CDL_BBOX_AG", "RICH_CDL_BBOX_AG", "RPR_CDL_BBOX_AG", 
                   "PRD_CDL_BBOX_AG", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/orig_bboxag.rds")
  
  
  
  files<-list.files("Metrics/", pattern = "CDL.+bdry_all")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_CDL_ALL", "CONTAG_CDL_ALL", "MA_CDL_ALL", "LPI_CDL_ALL", 
                   "SDI_CDL_ALL", "SIDI_CDL_ALL", "RICH_CDL_ALL", "RPR_CDL_ALL", 
                   "PRD_CDL_ALL", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/orig_bdryall.rds")
  
  
  files<-list.files("Metrics/", pattern = "CDL.+bbox_all")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_CDL_BBOX_ALL", "CONTAG_CDL_BBOX_ALL", "MA_CDL_BBOX_ALL", "LPI_CDL_BBOX_ALL", 
                   "SDI_CDL_BBOX_ALL", "SIDI_CDL_BBOX_ALL", "RICH_CDL_BBOX_ALL", "RPR_CDL_BBOX_ALL", 
                   "PRD_CDL_BBOX_ALL", "CNTY", "YEAR")) %>%
    mutate(YEAR = as.character(YEAR))  %>%
    saveRDS("Metrics/orig_bboxall.rds")


# reclassed data

  files<-list.files("Metrics/", pattern = "reclass.+bdry_ag")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_AG", "CONTAG_RC_AG", "MA_RC_AG", "LPI_RC_AG", 
                   "SDI_RC_AG", "SIDI_RC_AG", "RICH_RC_AG", "RPR_RC_AG", 
                   "PRD_RC_AG", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bdryag.rds")
  
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bbox_ag")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_BBOX_AG", "CONTAG_RC_BBOX_AG", "MA_RC_BBOX_AG", "LPI_RC_BBOX_AG", 
                   "SDI_RC_BBOX_AG", "SIDI_RC_BBOX_AG", "RICH_RC_BBOX_AG", "RPR_RC_BBOX_AG", 
                   "PRD_RC_BBOX_AG", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bboxag.rds")
  
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bdry_all")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_ALL", "CONTAG_RC_ALL", "MA_RC_ALL", "LPI_RC_ALL", 
                   "SDI_RC_ALL", "SIDI_RC_ALL", "RICH_RC_ALL", "RPR_RC_ALL", 
                   "PRD_RC_ALL", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bdryall.rds")
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bbox_all")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_BBOX_ALL", "CONTAG_RC_BBOX_ALL", "MA_RC_BBOX_ALL", "LPI_RC_BBOX_ALL", 
                   "SDI_RC_BBOX_ALL", "SIDI_RC_BBOX_ALL", "RICH_RC_BBOX_ALL", "RPR_RC_BBOX_ALL", 
                   "PRD_RC_BBOX_ALL", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bboxall.rds")



  
  
  
  # Merge all for one master landscape metric file ----
  
  filelist <- c("Metrics/orig_bdryag.rds", "Metrics/orig_bboxag.rds", "Metrics/orig_bdryall.rds", "Metrics/orig_bboxall.rds")
  
  master_lm <- filelist %>% 
    map(~readRDS(.)) %>%
    reduce(left_join, by = c("CNTY", "YEAR"))
  
  colSums(is.na(master_lm))
  
  
  saveRDS(master_lm, "master_lm_02092024.rds")
  
  
  # Build D (Hijmans) Diversity Measure ----
  
  lm <- readRDS("master_lm_02092024.rds")
  
  lm <- lm %>%
    mutate(D_CDL_BBOX_ALL = exp(SDI_CDL_BBOX_ALL), #exponent of SDI https://nsojournals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.2006.0030-1299.14714.x
           D_RC_BBOX_ALL = exp(SDI_RC_BBOX_ALL),
           D_CDL_BBOX_AG = exp(SDI_CDL_BBOX_AG),
           D_RC_BBOX_AG = exp(SDI_RC_BBOX_AG),
           D_CDL_ALL = exp(SDI_CDL_ALL),
           D_RC_ALL = exp(SDI_RC_ALL),
           D_CDL_AG = exp(SDI_CDL_AG),
           D_RC_AG = exp(SDI_RC_AG) )
  
  
  saveRDS(lm, "master_lm_04032024.rds")

  
