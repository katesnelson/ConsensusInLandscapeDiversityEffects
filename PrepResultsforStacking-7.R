## Process Item #7
# Description: Script to extract results from sensitivity analysis models
# Author: Kate Nelson
# Date updated: 01/10/2023


# Setup data and functions ----

library(pacman)
# source('Scripts/SA_functions_02122024.R')

p_load(tidyverse, sf, spdep, foreach, tigris, INLA, doParallel)
wd<-getwd()

dup_missing_evals <- function(metric, crop, n){
  
  vals <- df_new_met %>%
    select({{metric}}) %>%
    as.vector()
  
  q <- vals[[1]] %>%
    quantile(., probs = seq(0.1,1, by = 100/n/100), na.rm =T)
  
  uq <- unique(q)
  
  
  ###Need to reconcile q with uq
  
  q_eval <- q[1:length(q)] 
  uq_eval <- uq[1:length(uq)]
  
  q_eval <- q_eval %>% 
    as.data.frame(.) %>%
    rename("Value" = ".") %>%
    mutate(ID = seq(1:length(q_eval)))
  
  uq_eval <- uq_eval %>% 
    as.data.frame(.) %>%
    rename("Value" = ".") %>%
    mutate(ID = seq(1:length(uq_eval)))
  
  
  counts_X <- q_eval %>%
    group_by(Value) %>%
    mutate(n = n())
  
  counts_Y <- uq_eval %>%
    group_by(Value) %>%
    mutate(n = n())
  
  merged_counts <- full_join(counts_Y, counts_X, by = "Value", suffix = c("_Y", "_X"))
  
  # Identify elements in Y that need to be repeated to reproduce X
  repeated_elements <- merged_counts %>%
    filter(n_Y < n_X)
  
  rep_these <- repeated_elements %>%
    # filter(ID_X != 1) %>%
    mutate(times = n_X - n_Y) %>%
    distinct(Value, ID_Y, .keep_all = TRUE) %>%
    ungroup()
  
  rep_IDs <- rep(rep_these$ID_Y, rep_these$times)
  
  # rep_IDs <- unique(rep_these$ID_Y) #The IDS that need to be repeated
  
  uq_eval <- uq[1:length(uq)]
  
  dup_evals <- sort(append(uq, uq[rep_IDs], after = max(rep_IDs))) #check to make sure it works before changing model output
  
  # Now duplicate elements in model output based on index of duplicate quantile values
 
  r <- readRDS(paste0("Output/", metric, crop, "_ntiles_02142024.rds"))
  rw.a <- r$marginals.random[[6]]
  
  rw.a.dup <-append(rw.a, rw.a[rep_IDs], after = max(rep_IDs))
  rw.a.dup <- rw.a.dup[order(names(rw.a.dup))]
  
  r$marginals.random[[6]] <- rw.a.dup
  
  saveRDS(r,paste0("Output/", metric, crop, "_ntiles_02142024.rds")) 
} #KN 02272024 - This version seems to work


# ALFALFA - Check for consistent number of eval points across all models  ----

  #Repeat this section and the subsequent subsection of code for each crop#

alfalfa_models <-list.files("Output/", pattern = ("alfalfa*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)]

  fixlist <- c()
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", model_results[i], "!"))
      fixthis <- model_results[i]
      fixlist <- append(fixlist, fixthis)
    }
    
    rm(results)
  }
  
  fixlist <- unique(fixlist)

## Identify and fix missing evaluation points ----

### Read in base data for calculating ntiles ----

  # crop_panel <- readRDS("StartingData/alfalfa.rds") %>%
  #   mutate(CNTY = as.character(GEOID))
  # 
  # master_lm <- readRDS("StartingData/master_lm_02092024.rds") %>%
  #   distinct() %>% 
  #   mutate(YEAR = as.numeric(YEAR))
  # 
  # df_new_met <- crop_panel %>%
  #   select(-(RICH_CDL_BBOX_AG:RPR_CDL_ALL)) %>%
  #   left_join(., master_lm, by = c("CNTY", "YEAR"))
  
  df_new_met <- readRDS("PreppedData/alfalfa_ntiles_02142024.rds")

### Set up loop to duplicate random marginals when ntiles were not unique ----

  n <- 10
  metric <- str_extract(fixlist[1],"^.*?(AG|ALL)")
  crop <- "alfalfa"
  
  foreach (i=1:length(fixlist)) %do% {
    n <- 10
    metric <- str_extract(fixlist[i],"^.*?(AG|ALL)")
    dup_missing_evals(metric, crop, n)
  }

  
#### Check to make sure it worked ----

 fixmore <- c()
  
  foreach (i=1:length(fixlist)) %do% {
    
    results <- readRDS(paste0("Output/",fixlist[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", fixlist[i], "!"))
      fixthis <- fixlist[i]
      fixmore <- append(fixmore, fixthis)
    }
    
    rm(results)
  }
  
  fixmore <- unique(fixmore)
  
  

# CORN - Check for consistent number of eval points across all models  ----

#Repeat this section and the subsequent subsection of code for each crop

corn_models <-list.files("Output/", pattern = ("corn*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- corn_models[!(corn_models %in% remove_datafiles)]

  fixlist <- c()
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", model_results[i], "!"))
      fixthis <- model_results[i]
      fixlist <- append(fixlist, fixthis)
    }
   
    rm(results)
  }
  
  fixlist <- unique(fixlist)

## Identify and fix missing evaluation points ----

### Read in base data for calculating ntiles ----

  # crop_panel <- readRDS("StartingData/corn.rds") %>%
  #   mutate(CNTY = as.character(GEOID))
  # 
  # master_lm <- readRDS("StartingData/master_lm_02092024.rds") %>%
  #   distinct() %>% 
  #   mutate(YEAR = as.numeric(YEAR))
  # 
  # df_new_met <- crop_panel %>%
  #   select(-(RICH_CDL_BBOX_AG:RPR_CDL_ALL)) %>%
  #   left_join(., master_lm, by = c("CNTY", "YEAR"))

  df_new_met <- readRDS("PreppedData/corn_ntiles_02142024.rds")
  
### Set up loop to duplicate random marginals when ntiles were not unique ----

  
  n <- 10
  metric <- str_extract(fixlist[1],"^.*?(AG|ALL)")
  crop <- "corn"
  
  foreach (i=1:length(fixlist)) %do% {
    n <- 10
    metric <- str_extract(fixlist[i],"^.*?(AG|ALL)")
    dup_missing_evals(metric, crop, n)
  }

  
#### Check to make sure it worked ----
  
  fixmore <- c()
  
  foreach (i=1:length(fixlist)) %do% {
    
    results <- readRDS(paste0("Output/",fixlist[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", fixlist[i], "!"))
      fixthis <- fixlist[i]
      fixmore <- append(fixmore, fixthis)
    }
    
    rm(results)
  }
  
  fixmore <- unique(fixmore)
  

  
# SOY - Check for consistent number of eval points across all models  ----

#Repeat this section and the subsequent subsection of code for each crop

soy_models <-list.files("Output/", pattern = ("soy*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- soy_models[!(soy_models %in% remove_datafiles)]

  fixlist <- c()
  
  foreach (i=56:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", model_results[i], "!"))
      fixthis <- model_results[i]
    }
    fixlist <- append(fixlist, fixthis)
    rm(results)
  }
  
  fixlist <- unique(fixlist)

## Identify and fix missing evaluation points ----

### Read in base data for calculating ntiles ----

  # crop_panel <- readRDS("StartingData/soy.rds") %>%
  #   mutate(CNTY = as.character(GEOID))
  # 
  # master_lm <- readRDS("StartingData/master_lm_02092024.rds") %>%
  #   distinct() %>% 
  #   mutate(YEAR = as.numeric(YEAR))
  # 
  # df_new_met <- crop_panel %>%
  #   select(-(RICH_CDL_BBOX_AG:RPR_CDL_ALL)) %>%
  #   left_join(., master_lm, by = c("CNTY", "YEAR"))
  
  df_new_met <- readRDS("PreppedData/soy_ntiles_02142024.rds")

### Set up loop to duplicate random marginals when ntiles were not unique ----


  n <- 10
  metric <- str_extract(fixlist[1],"^.*?(AG|ALL)")
  crop <- "soy"
  
  foreach (i=1:length(fixlist)) %do% {
    n <- 10
    metric <- str_extract(fixlist[i],"^.*?(AG|ALL)")
    dup_missing_evals(metric, crop, n)
  }

#### Check to make sure it worked ----
  
  fixmore <- c()
  
  foreach (i=1:length(fixlist)) %do% {
    
    results <- readRDS(paste0("Output/",fixlist[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", fixlist[i], "!"))
      fixthis <- fixlist[i]
      fixmore <- append(fixmore, fixthis)
    }
    
    rm(results)
  }
  
  fixmore <- unique(fixmore)
  
  


# WWHEAT - Check for consistent number of eval points across all models  ----

#Repeat this section and the subsequent subsection of code for each crop

wwheat_models <-list.files("Output/", pattern = ("wwheat*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- wwheat_models[!(wwheat_models %in% remove_datafiles)]

  fixlist <- c()
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", model_results[i], "!"))
      fixthis <- model_results[i]
    }
    fixlist <- append(fixlist, fixthis)
    rm(results)
  }
  
  fixlist <- unique(fixlist)

## Identify and fix missing evaluation points ----

### Read in base data for calculating ntiles ----

  # crop_panel <- readRDS("StartingData/wwheat.rds") %>%
  #   mutate(CNTY = as.character(GEOID))
  # 
  # master_lm <- readRDS("StartingData/master_lm_02092024.rds") %>%
  #   distinct() %>% 
  #   mutate(YEAR = as.numeric(YEAR))
  # 
  # df_new_met <- crop_panel %>%
  #   select(-(RICH_CDL_BBOX_AG:RPR_CDL_ALL)) %>%
  #   left_join(., master_lm, by = c("CNTY", "YEAR"))
  
  df_new_met <- readRDS("PreppedData/wwheat_ntiles_02142024.rds")

### Set up loop to duplicate random marginals when ntiles were not unique ----


  n <- 10
  metric <- str_extract(fixlist[1],"^.*?(AG|ALL)")
  crop <- "wwheat"
  
  foreach (i=1:length(fixlist)) %do% {
    n <- 10
    metric <- str_extract(fixlist[i],"^.*?(AG|ALL)")
    dup_missing_evals(metric, crop, n)
  }

#### Check to make sure it worked ----
  
  fixmore <- c()
  
  foreach (i=1:length(fixlist)) %do% {
    
    results <- readRDS(paste0("Output/",fixlist[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", fixlist[i], "!"))
      fixthis <- fixlist[i]
      fixmore <- append(fixmore, fixthis)
    }
    
    rm(results)
  }
  
  fixmore <- unique(fixmore)
  
  
  
# HAY - Check for consistent number of eval points across all models  ----

#Repeat this section and the subsequent subsection of code for each crop

hay_models <-list.files("Output/", pattern = ("hay*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- hay_models[!(hay_models %in% remove_datafiles)]

  fixlist <- c()
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", model_results[i], "!"))
      fixthis <- model_results[i]
    }
    fixlist <- append(fixlist, fixthis)
    rm(results)
  }

  fixlist <- unique(fixlist)

## Identify and fix missing evaluation points ----

### Read in base data for calculating ntiles ----

  # crop_panel <- readRDS("StartingData/hay.rds") %>%
  #   mutate(CNTY = as.character(GEOID))
  # 
  # master_lm <- readRDS("StartingData/master_lm_02092024.rds") %>%
  #   distinct() %>% 
  #   mutate(YEAR = as.numeric(YEAR))
  # 
  # df_new_met <- crop_panel %>%
  #   select(-(RICH_CDL_BBOX_AG:RPR_CDL_ALL)) %>%
  #   left_join(., master_lm, by = c("CNTY", "YEAR"))
  
  df_new_met <- readRDS("PreppedData/hay_ntiles_02142024.rds")

### Set up loop to duplicate random marginals when ntiles were not unique ----


  n <- 10
  metric <- str_extract(fixlist[1],"^.*?(AG|ALL)")
  crop <- "hay"
  
  foreach (i=1:length(fixlist)) %do% {
    n <- 10
    metric <- str_extract(fixlist[i],"^.*?(AG|ALL)")
    dup_missing_evals(metric, crop, n)
  }
  
  
#### Check to make sure it worked ----
  
  fixmore <- c()
  
  foreach (i=1:length(fixlist)) %do% {
    
    results <- readRDS(paste0("Output/",fixlist[i]))
    
    
    if(length(results$marginals.random[[6]]) != 10){
      print(paste0("ONLY ", length(results$marginals.random[[6]]),
                   " EVAL POINTS for ", fixlist[i], "!"))
      fixthis <- fixlist[i]
      fixmore <- append(fixmore, fixthis)
    }
    
    rm(results)
  }
  
  fixmore <- unique(fixmore)
  
  
  
  
  
  

# Return model fit statistics ----


all_datafiles<-list.files("PreppedData/")
remove_datafiles<-list.files("PreppedData/", pattern = ("^H.*"))
datafiles <- all_datafiles[!(all_datafiles %in% remove_datafiles)]


alfalfa_models <-list.files("Output/", pattern = ("alfalfa*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)]









model.results.summ(data="alfalfa_012023.rds", model_results=model_results)


model.results.summ(data="soyfinal", crop="soy", metrics)
model.results.summ(data="wwheatfinal", crop="wwheat", metrics)


########################################################################################
### Create simplified climate curve summary plot across all metric-crop combinations ###  WORKS
########################################################################################

metrics <- c("RICH_CDL_BBOX_AG", "RICH_CDL_BBOX_ALL",  "RICH_CDL_AG", "RICH_CDL_ALL")
crops<-c("corn","soy","wwheat", "alfalfa", "hay")
y.min<- c(-0.3, -0.3, -0.55, -0.1) #specific for each of 4 climate vars, in order TP, GDD, SDD, SOIL
y.max<-c(0.3, 0.3, 0.3, 0.3)
x.max<- c(3.5, 3.5, 3.5, 3.5)
x.min<- c(-2.5, -2.5, -2.5, -2.5)

p3<-plot.cl.summ.simple(metrics, crops, priors = "default", subset = "", y.min, y.max, x.min, x.max)

#build a 2x2 plot of climate curves
figure3<-ggarrange(p3[[2]] + theme( text=element_text(family="Helvetica", size=7), axis.title = element_text(face="bold"), legend.text=element_text(family="Helvetica", size=7)), p3[[3]] + theme( axis.title.y = element_blank(), text=element_text(family="Helvetica", size=7), axis.title = element_text(face="bold")), 
                   p3[[1]] + theme( text=element_text(family="Helvetica", size=7), axis.title = element_text(face="bold")) , p3[[4]] + theme( axis.title.y = element_blank(), text=element_text(family="Helvetica", size=7), axis.title = element_text(face="bold")), 
                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom", 
                   labels="auto", font.label = list(size = 10, face="bold", family="Helvetica"),hjust = c(-3, -1,-3,-1), vjust =c(23,23,23,23) )
annotate_figure(figure3)
# ggsave("climate_summary.png", figure3, width = 5, height = 4)
# ggsave("climate_summary_final.pdf", figure3, width = 166, height = 132, units="mm")

figure3<-ggarrange(p3[[2]] + theme( text=element_text(family="Helvetica", size=7), legend.text=element_text(family="Helvetica", size=7)), p3[[3]] + theme( axis.title.y = element_blank(), text=element_text(family="Helvetica", size=7)), 
                   p3[[1]] + theme( text=element_text(family="Helvetica", size=7)) , p3[[4]] + theme( axis.title.y = element_blank(), text=element_text(family="Helvetica", size=7)), 
                   ncol=2,nrow=2, common.legend = TRUE, legend = "bottom", 
                   labels="auto", font.label = list(size = 10, face="bold", family="Helvetica"),hjust = c(-3, -1,-3,-1), vjust =c(11,11,11,11) )
# 
# ggsave("climate_summary_final_2.pdf", figure3, width = 88, height = 72, units="mm")



#################################
### Find inflection points ### 
################################

#Identify the floor, ceiling, and bounds between which most change occurs  

#RICH




####BUILDING NEW FUNCTIONS #####



#########################################################################
### Combine posterior distributions of rw1 vars from multiple models ### NEW 01112023
########################################################################

results <- readRDS("Output/defaultRICH_CDL_AGalfalfa_012023.rds")
results2 <- readRDS("Output/defaultRICH_CDL_BBOX_AGalfalfa_012023.rds")
results3 <- readRDS("Output/defaultRICH_CDL_BBOX_ALLalfalfa_012023.rds")
results4 <- readRDS("Output/defaultRICH_CDL_ALLalfalfa_012023.rds")

rw.a <- results$marginals.random$RICH_CDL_AG
rw.b <- results2$marginals.random$RICH_CDL_BBOX_AG
rw.c <- results3$marginals.random$RICH_CDL_BBOX_ALL
rw.d <- results4$marginals.random$RICH_CDL_ALL

ggplot() +
  geom_point(data = as.data.frame(rw.a$index.2), aes(x=x, y=y)) +
  geom_point(data = as.data.frame(rw.b$index.2), aes(x=x, y=y), color = "blue") +
  geom_point(data = as.data.frame(rw.c$index.2), aes(x=x, y=y), color = "green") +
  geom_point(data = as.data.frame(rw.d$index.2), aes(x=x, y=y), color = "red") +
  theme_minimal()

# ggplot(data = as.data.frame(rw.b$index.2)) +
#   geom_point(aes(x=x, y=y)) +
#   theme_minimal()

#combine marginals for rw1 random effect, this operates on each rw evaluation point list separately

# 
# for (y in 1:length(ras)){
#   assign(paste("df",y,sep=""),ras[[y]])
# }
# 
# rm(ras)
# imap(ras, ~ set_names(tibble(.x), .y)) %>%
#   set_names(str_c("DF", 1:length(yrs))) %>% 
#   list2env(.GlobalEnv)

idlist <- rep(results$summary.random$RICH_CDL_AG$ID, each = length(rw.a$index.1[,1]))
names(idlist) <- c("ID")
t <- split(idlist, cut_number(idlist, length(rw.a)))
rw.a <- purrr::map2(rw.a, t, cbind)

idlist <- rep(results2$summary.random$RICH_CDL_BBOX_AG$ID, each = length(rw.b$index.1[,1]))
t <- split(idlist, cut_number(idlist, length(rw.b)))
rw.b <- purrr::map2(rw.b, t, cbind)

idlist <- rep(results3$summary.random$RICH_CDL_BBOX_ALL$ID, each = length(rw.c$index.1[,1]))
t <- split(idlist, cut_number(idlist, length(rw.c)))
rw.c <- purrr::map2(rw.c, t, cbind)

idlist <- rep(results4$summary.random$RICH_CDL_ALL$ID, each = length(rw.d$index.1[,1]))
t <- split(idlist, cut_number(idlist, length(rw.d)))
rw.d <- purrr::map2(rw.d, t, cbind)

combine.a <- do.call(bind_rows, rw.a)

combine.a <- purrr::map(rw.a, as.data.frame) #data.frame then bind rows?
ca <- bind_rows(combine.a, .id = "column_label") %>% rename(ID = V3)
rownames(ca) <- NULL

combine.b <- purrr::map(rw.b, as.data.frame)

rw.post <- purrr::pmap(list(rw.a, rw.b, rw.c, rw.d), bind_rows) %>%
  purrr::map(as.data.frame) %>%
  lapply(., function(df){arrange(df,x)}) #has to be sorted (by x) for inla.zmarginal to work


ggplot(data = as.data.frame(rw.post$index.2)) +
  geom_point(aes(x=x, y=y)) +
  theme_minimal()

#extract summary of combined posteriors - METHOD 1
# rw.summary_ex <- inla.zmarginal(marginal = rw.post$index.2)

rw.summary <- purrr::map(rw.post,inla.zmarginal )


#extract summary of combined posteriors - METHOD 2
# rw.random_ex <- inla.rmarginal(1000, marginal = rw.post$index.2)
# 
# ggplot(data = as.data.frame(rw.random_ex))+
#   geom_density(aes(x  = rw.marg_random)) +
#   theme_minimal()

rw.quants <- purrr::map(rw.post, ~inla.rmarginal(1000, .) %>% quantile(probs = c(0.025, 0.25, 0.5, 0.75, 0.975)))

#############################################
###Create plots of combined RW1 effects  ### NEW 01112023
############################################


# plot.lm.summ<-function(metrics, crops, priors, subset, y.min, y.max, x.min, x.max){ 
#   
#   
#   metric<-toupper(metrics)
#   metrics.labels <-c("Mean Patch Area","Contagion","Edge Density","Largest Patch Index",
#                      "Richness","Shannon Diversity Index","Shannon Evenness Index","Simpson Diversity Index","Simpson Evenness Index","Percent Natural Cover")
#   crops.labels<-crops
#   crops.labels[crops.labels == "wwheat"] <- "Winter wheat"
#   crops.labels[crops.labels == "corn"] <- "Corn"
#   crops.labels[crops.labels == "soy"] <- "Soy"
#   
#   
#   my.plots<-c()
#   
#   foreach (i=1:length(metrics))%do% {
#     
#     obs<-NA
#     
#     foreach (j=1:length(crops))%do% {
#       #pull the model output by metric for ag diversity
#       if (file.exists(paste0(wd,"/Output/", crops[j], "_",metrics[i],".rds"))){


results_1<- purrr::map(rw.summary, as.data.frame) %>% 
  do.call("rbind", .) %>%
  cbind(results2$summary.random$RICH_CDL_BBOX_AG$ID) %>% #Add the rw1 evaluation pt values
  rename(ID = `results2$summary.random$RICH_CDL_BBOX_AG$ID`)

# results_1<-get(metric[i],results_1$summary.random)[ ,c(1,4,5,6)] #pull the results for the effect of the metric for the crop
# results_1$Crop<-crops.labels[j]#add crop column
# obs<-c(obs, list(results_1))
#   }
# }


#combine data for a single plot
# library(data.table)
# 
# results_tot<-as.data.frame(rbindlist(obs[2:4]))
# mean_lm<-results_tot %>% group_by (Crop) %>% summarise_at('ID', mean)
# sd_lm<-results_tot %>% group_by (Crop) %>% summarise_at('ID', sd)
# sd_lm$ID<-sd_lm$ID+mean_lm$ID
# sdlow_lm<-results_tot %>% group_by (Crop) %>% summarise_at('ID', sd)
# sdlow_lm$ID<-mean_lm$ID-sdlow_lm$ID
# 

#build a plot
library("viridis")

ggplot(data=results_1) + geom_line(aes (x=ID, y=`quant0.5`), size=0.5) +
  # xlim(x.min[i], x.max[i]) + ylim(y.min,y.max) +
  # xlab(paste0(metrics.labels[i])) + ylab("Effect on log (Yield)") +   
  scale_x_continuous(breaks=c(-3,-2,-1, 0, 1,2,3)) + #, limits=c(x.min[i], x.max[i])) + #this overwrites the min-max limit which we want to be the same for all plots
  geom_ribbon(aes(x=ID, ymin = `quant0.025`, ymax = `quant0.975`), alpha=0.25) +
  scale_color_viridis_d( option = "D")+
  scale_fill_viridis_d( option = "D") +
  # geom_vline(aes(xintercept=ID, col=Crop), mean_lm) +
  # geom_vline(aes(xintercept=ID, col=Crop), sd_lm, linetype="dashed") +
  # geom_vline(aes(xintercept=ID, col=Crop), sdlow_lm, linetype="dashed") +
  theme_light() + theme(panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        # panel.grid.major = element_line(colour = "grey50"),
                        #   panel.grid.minor = element_line(colour = "grey50"),
                        # panel.background = element_rect(fill = NA),
                        panel.ontop = FALSE
  )    

my.plots[[i]]<-p1

# }
return(my.plots)
# }





