## Process Item #3
# Description: Script to prep the final landscape metric data for sensitivity analysis models
# Author: Kate Nelson
# Date updated: 02/09/2024



library(pacman)
p_load(tidyverse, sf, foreach, tigris, INLA) #spdep, rgeos,
wd<-getwd()
source(paste0(wd,'/Scripts/SA_functions.R'))

####################################
### Combine Landscape Metric Data ##
####################################

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



  # Merge all for one master landscape metric file
  
  filelist <- c("Metrics/orig_bdryag.rds", "Metrics/orig_bboxag.rds", "Metrics/orig_bdryall.rds", "Metrics/orig_bboxall.rds", 
                "Metrics/reclass_bdryag.rds", "Metrics/reclass_bboxag.rds", "Metrics/reclass_bdryall.rds", "Metrics/reclass_bboxall.rds")
  
  master_lm <- filelist %>% 
    map(~readRDS(.)) %>%
    reduce(left_join, by = c("CNTY", "YEAR"))

colSums(is.na(master_lm))

# missinglist <- unique(master_lm[rowSums(is.na(master_lm)) > 0,]$CNTY )

missinglist <- distinct(master_lm[rowSums(is.na(master_lm)) > 0,], CNTY, YEAR )


##################################
### Fill Missings ################
#################################

counties <- tigris::counties(state = NULL, cb = FALSE, resolution = "500k", year = NULL)
# counties <- st_read("tl_2019_us_county.shp")

mask <- c(0, 81, -128) #mask only background, cloud, and missing data

ag_mask <- c(0, 61:65, 81:83, 87:88, 92, 111:112, 121:124,
             131, 141:143, 152, 176, 190, 195)

missinglist <- distinct(master_lm[rowSums(is.na(master_lm)) > 0,], CNTY, YEAR )

missinglist$filename <- paste0("reclass_", substr(missinglist$CNTY,1,2), "_", missinglist$YEAR, "yr.rds")

files <- list.files("ReclassedCDL1/", pattern = "reclass_")

missing_rasters <- unique(missinglist$filename[!(missinglist$filename %in% files)]) #rerun these in reclass_spatial.R script




mdat <- foreach(i = 1:nrow(missinglist), .combine = 'rbind') %do% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel)
    
    cty <- missinglist[i, "CNTY"]
    state_fips <- substr(cty,1,2)
    year <- missinglist[i, "YEAR"]
    
    ras <- readRDS(paste0("ReclassedCDL1/reclass_", state_fips, "_", year, "yr.rds"))[[1]]
    
    c <- counties[counties$GEOID == cty, ] # this subsets the county boundaries to the bounding box of one county
    c <- st_transform(c, st_crs(ras))
    r <- raster::crop(ras, c) # now crop the raster to the county bounding box
    r <- raster::mask(r, c) # now crop the raster to the county bounding box
    r[r %in% ag_mask] <- NA # set mask values to null so they don't influence landscape metric calculations
    
    ed <- lsm_l_ed(r)$value 
    contag <- lsm_l_contag(r)$value 
    mean_area <- lsm_l_area_mn(r)$value
    lpi <- lsm_l_lpi(r)$value
    shdi <- lsm_l_shdi(r)$value
    sidi <- lsm_l_sidi(r)$value
    rich <- lsm_l_pr(r)$value
    rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
    prd <- lsm_l_prd(r)$value
    
    
    final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty, year) #combines it with the name of the polygon
    return(final)
  }
  saveRDS(mdat, paste0("Metrics/reclass_","_bdry_ag_missings.rds"))
  

  mdat <- foreach(i = 1:nrow(missinglist), .combine = 'rbind') %do% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel)
    
    cty <- missinglist[i, "CNTY"]
    state_fips <- substr(cty,1,2)
    year <- missinglist[i, "YEAR"]
    
    ras <- readRDS(paste0("ReclassedCDL1/reclass_", state_fips, "_", year, "yr.rds"))[[1]]
    
    c <- counties[counties$GEOID == cty, ] # this subsets the county boundaries to the bounding box of one county
    c <- st_transform(c, st_crs(ras))
    r <- raster::crop(ras, c) # now crop the raster to the county bounding box
    r <- raster::mask(r, c) # now crop the raster to the county bounding box
    r[r %in% mask] <- NA # set mask values to null so they don't influence landscape metric calculations
    
    ed <- lsm_l_ed(r)$value 
    contag <- lsm_l_contag(r)$value 
    mean_area <- lsm_l_area_mn(r)$value
    lpi <- lsm_l_lpi(r)$value
    shdi <- lsm_l_shdi(r)$value
    sidi <- lsm_l_sidi(r)$value
    rich <- lsm_l_pr(r)$value
    rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
    prd <- lsm_l_prd(r)$value
    
    
    final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty, year) #combines it with the name of the polygon
    return(final)
  }
  saveRDS(mdat, paste0("Metrics/reclass_","_bdry_all_missings.rds"))


  mdat <- foreach(i = 1:nrow(missinglist), .combine = 'rbind') %do% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel)
    
    cty <- missinglist[i, "CNTY"]
    state_fips <- substr(cty,1,2)
    year <- missinglist[i, "YEAR"]
    
    ras <- readRDS(paste0("ReclassedCDL1/reclass_", state_fips, "_", year, "yr.rds"))[[1]]
    
    c <- counties[counties$GEOID == cty, ] # this subsets the county boundaries to the bounding box of one county
    c <- st_transform(c, st_crs(ras))
    r <- raster::crop(ras, c) # now crop the raster to the county bounding box
    r[r %in% ag_mask] <- NA # set mask values to null so they don't influence landscape metric calculations
    
    ed <- lsm_l_ed(r)$value 
    contag <- lsm_l_contag(r)$value 
    mean_area <- lsm_l_area_mn(r)$value
    lpi <- lsm_l_lpi(r)$value
    shdi <- lsm_l_shdi(r)$value
    sidi <- lsm_l_sidi(r)$value
    rich <- lsm_l_pr(r)$value
    rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
    prd <- lsm_l_prd(r)$value
    
    
    final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty, year) #combines it with the name of the polygon
    return(final)
  }
  saveRDS(mdat, paste0("Metrics/reclass_","_bbox_ag_missings.rds"))
  
  
  mdat <- foreach(i = 1:nrow(missinglist), .combine = 'rbind') %do% {
    
    library(pacman)
    p_load(sf, raster, terra, landscapemetrics, foreach, parallel, doParallel)
    
    cty <- missinglist[i, "CNTY"]
    state_fips <- substr(cty,1,2)
    year <- missinglist[i, "YEAR"]
    
    ras <- readRDS(paste0("ReclassedCDL1/reclass_", state_fips, "_", year, "yr.rds"))[[1]]
    
    c <- counties[counties$GEOID == cty, ] # this subsets the county boundaries to the bounding box of one county
    c <- st_transform(c, st_crs(ras))
    r <- raster::crop(ras, c) # now crop the raster to the county bounding box
    r[r %in% mask] <- NA # set mask values to null so they don't influence landscape metric calculations
    
    ed <- lsm_l_ed(r)$value 
    contag <- lsm_l_contag(r)$value 
    mean_area <- lsm_l_area_mn(r)$value
    lpi <- lsm_l_lpi(r)$value
    shdi <- lsm_l_shdi(r)$value
    sidi <- lsm_l_sidi(r)$value
    rich <- lsm_l_pr(r)$value
    rpr <- lsm_l_rpr(r, classes_max = 38)$value #38 is the total number of unique classes possible in the reclassed data
    prd <- lsm_l_prd(r)$value
    
    
    final <- data.frame(ed, contag, mean_area, lpi, shdi, sidi, rich, rpr, prd, cty, year) #combines it with the name of the polygon
    return(final)
  }
  saveRDS(mdat, paste0("Metrics/reclass_","_bbox_all_missings.rds"))






###################################################
### Combine Landscape Metric Data with MISSINGS ##
##################################################
  
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
  
  
  
  files<-list.files("Metrics/", pattern = "missings")  #get list of filled missing metric files
  
  files <- paste0("Metrics/", files)
  
  foreach(i = 1:length(files)) %do% { #remove produced NA column from each file
    d <- readRDS(files[i]) 
    d <- d %>%
      select(1:11) %>%
      rename("cty_list.k." = "cty") 
    saveRDS(d, files[i])
  }
  
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bdry_ag")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_AG", "CONTAG_RC_AG", "MA_RC_AG", "LPI_RC_AG", 
                   "SDI_RC_AG", "SIDI_RC_AG", "RICH_RC_AG", "RPR_RC_AG", 
                   "PRD_RC_AG", "CNTY", "YEAR")) %>%
      saveRDS("Metrics/reclass_bdryag_mis.rds")
  
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bbox_ag")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_BBOX_AG", "CONTAG_RC_BBOX_AG", "MA_RC_BBOX_AG", "LPI_RC_BBOX_AG", 
                   "SDI_RC_BBOX_AG", "SIDI_RC_BBOX_AG", "RICH_RC_BBOX_AG", "RPR_RC_BBOX_AG", 
                   "PRD_RC_BBOX_AG", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bboxag_mis.rds")
  
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bdry_all")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_ALL", "CONTAG_RC_ALL", "MA_RC_ALL", "LPI_RC_ALL", 
                   "SDI_RC_ALL", "SIDI_RC_ALL", "RICH_RC_ALL", "RPR_RC_ALL", 
                   "PRD_RC_ALL", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bdryall_mis.rds")
  
  
  files<-list.files("Metrics/", pattern = "reclass.+bbox_all")  #get list of metric files
  
  files <- paste0("Metrics/", files)
  
  files %>% 
    map_df(~readRDS(.)) %>%
    `colnames<-`(c("ED_RC_BBOX_ALL", "CONTAG_RC_BBOX_ALL", "MA_RC_BBOX_ALL", "LPI_RC_BBOX_ALL", 
                   "SDI_RC_BBOX_ALL", "SIDI_RC_BBOX_ALL", "RICH_RC_BBOX_ALL", "RPR_RC_BBOX_ALL", 
                   "PRD_RC_BBOX_ALL", "CNTY", "YEAR")) %>%
    saveRDS("Metrics/reclass_bboxall_mis.rds")
  
  
  
  # Merge all for one master landscape metric file
  
  filelist <- c("Metrics/orig_bdryag.rds", "Metrics/orig_bboxag.rds", "Metrics/orig_bdryall.rds", "Metrics/orig_bboxall.rds", 
                "Metrics/reclass_bdryag_mis.rds", "Metrics/reclass_bboxag_mis.rds", "Metrics/reclass_bdryall_mis.rds", "Metrics/reclass_bboxall_mis.rds")
  
  master_lm <- filelist %>% 
    map(~readRDS(.)) %>%
    reduce(left_join, by = c("CNTY", "YEAR"))
  
  colSums(is.na(master_lm))
  
  # missinglist <- unique(master_lm[rowSums(is.na(master_lm)) > 0,]$CNTY )
  
  missinglist <- distinct(master_lm[rowSums(is.na(master_lm)) > 0,], CNTY, YEAR, .keep_all = T )


  saveRDS(master_lm, "master_lm_02092024.rds")


  
