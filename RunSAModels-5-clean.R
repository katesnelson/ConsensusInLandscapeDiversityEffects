## Process Item #5
# Description: Script to run the sensitivity analysis models
# Author: Kate Nelson
# Date updated: 01/09/2023



library(pacman)
wd<-getwd()
source(paste0(wd,'/Scripts/SA_functions_02122024-0.R'))

p_load(tidyverse, sf, spdep, foreach, tigris, doParallel, INLA)



## Define Data to use and Metrics to Evaluate ----


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







datafiles <- list.files(path = "PreppedData/", pattern = "_02142024")

  remove <- list.files(path = "PreppedData/", pattern = "H.adj")
  
  datafiles <- datafiles[!(datafiles %in% remove)]

crops <- c("alfalfa", "corn","hay", "soy","wwheat")



## Run the models ----
  #This works when you have one datafile for each crop


  cl <- makeCluster(length(crops))
  registerDoParallel(cl)
  
  foreach(i=1:length(crops), .packages=c("sf", "dplyr","INLA","doParallel","foreach")) %dopar% {
    tryCatch({
      foreach (j=1:length(metrics)) %do%{ #length(metrics)
        if(!(file.exists(paste0("Output/", metrics[j], datafiles[i])))){
         
        f1 <- build.formula(independent = metrics[j], data = datafiles[i], priors = c("default"))
        h <- readRDS(paste0(wd,"/PreppedData/H.adj.",datafiles[i]))
        run.model(f1, data = datafiles[i], priors ="default", savename=paste0(metrics[j], datafiles[i])) 
        
        }else{print ("File already found")}
      }
    }, error=function(cond) {
      message(paste("Encountered some issue for", metrics[j], "and", datafiles[i])) #report that there is an error and where it occurred, not working
      
    })
  }
  stopCluster(cl)
  
  
 
  