## Process Item #1
## Description: Script to reclassify CDL land use/land cover rasters.
  # Script was run on the Beocat HPC.
## Authors: Kate Nelson
## Date: Last updated 10/04/2022


library(cdlTools)
library(raster)
library(tidyverse) 
library(foreach)
library(tigris)
library(doParallel)


reclass <- readRDS("reclass_table.rds")



#Create a lookup table for reclassification ----


lookup <-reclass %>% 
  dplyr::select(Crop, New_Crop) 

colnames(lookup) <-c("replace","new")



# Parallel process across states and loop across years.----

    #implement this on Beocat HPC


data(fips_codes)

fc <- as.data.frame(fips_codes) %>%
   dplyr::filter(!state_name %in% c("Alaska", "Hawaii","Puerto Rico", "American Samoa",
                                    "Guam", "Northern Mariana Islands", "U.S. Minor Outlying Islands",
                                    "U.S. Virgin Islands" )) #get all state fips codes except for these places

sc <- unique(fc$state_code)



foreach(j=1:length(sc)) %dopar% {
  a <- getCDL(sc[j], c(2008:2020), location = "CDLdata")
  foreach(i=1:length(a)) %do%
    {matchvals<- lookup$new[match(as.vector(a[[i]]), lookup$replace, nomatch = "NULL")]
    values(a[[i]])<-matchvals}
  saveRDS(a, paste0("reclass_",sc[j],"_08-20.rds"))
}




# Inital Test: Just for one state-year ----

#Get CDL data for states
a <- getCDL(20, c(2008:2018), location = "CDLdata")

#Create a list of pixel values from original raster and replace them with values from the lookup table.
matchvals<- lookup$new[match(as.vector(a[[1]]), lookup$replace)]

#Set the values in the raster to the new list of pixel values
values(a[[1]])<-matchvals


plot(a[[1]])


# Now try looping across years for a single state. ----

#See how long it takes. --> not too shabby, downloading takes the most time


system.time({
  a <- getCDL(21, c(2008:2018), location = "CDLdata")
  foreach(i=1:length(a)) %do%
    {matchvals<- lookup$new[match(as.vector(a[[i]]), lookup$replace, nomatch = "NULL")]
    values(a[[i]])<-matchvals}
})

