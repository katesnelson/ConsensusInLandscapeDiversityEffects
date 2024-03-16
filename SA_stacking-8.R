# Process Item #8
# Description: Script to stack results from Bayesian models
# Author: Kate Nelson
# Date updated: 03/06/2024



library(pacman)
p_load(tidyverse, INLA, doParallel, foreach, DescTools)


# ALFALFA: Get model list ----


alfalfa_models <-list.files("Output/", pattern = ("alfalfa*"))
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)]


alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH.*alfalfa*"))# get just RICH-alfalfa models
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)] 

alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH.*alfalfa*|alfalfa.*SDI|SDI.*alfalfa*|alfalfa.*SIDI|SIDI.*alfalfa*")) #get several diversity - alfalfa models
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)]

alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH.*alfalfa*|alfalfa.*SDI|SDI.*alfalfa*|alfalfa.*SIDI|SIDI.*alfalfa*|alfalfa.*RPR|RPR.*alfalfa*|alfalfa.*PRD|PRD.*alfalfa*")) #get several diversity - alfalfa models
remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
model_results <- alfalfa_models[!(alfalfa_models %in% remove_datafiles)]

# Extract and stack marginals for landscape diversity metric from each model ----


  #initialize the stacked marginals dataframe

    rw.allpost <- data.frame()

  #loop through each model

    foreach (i=1:length(model_results)) %do% {
      
      results <- readRDS(paste0("Output/",model_results[i]))
      
      full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
      metric <- str_extract(model_results[i],"^[^_]+")
      crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
      classification <- str_extract(model_results[i],"CDL|RC")
      boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
  #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
      rw.marg <- results$marginals.random[[6]] 
    
    
  ## Build out the marginals table ----  
  
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
      # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
  
  # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
  
  # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
  
  
    
    ## Change data structure ----
    
      # Now we get rid of the annoying list of matrices and create one large dataframe of all
      # posterior marginal distribution values for each model
      
        combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
        
        fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
          rename(Percentile = V3) %>% #, Value = V4) %>% 
          mutate(model = model_results[i], 
                 metric = metric, 
                 crop = crop,
                 classification = classification,
                 boundaries = boundaries)
      
      rownames(fullmarg.table) <- NULL
      
      
    ## Merge the dataframes for all models in list ----
      
      rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
      
      return(rw.allpost)
  
      rm(results)
      
}

    #Example plot of combined marginals at one evaluation point
    min <- min(rw.allpost$Percentile)
    
    ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
      geom_point(aes(x=x, y=y)) +
      theme_minimal()
    
    
    # Now split out by common ID value since we want to average the posterior 
    # marginal distribution separately at each evaluation point
    
    rw.post.list <- split(rw.allpost, ~ Percentile)
    
    ggplot(data = rw.post.list[[5]]) +
      geom_point(aes(x=x, y=y)) +
      theme_minimal()
    
    
    #############################################################################
    ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
    #############################################################################
    
    #Yay brute force! This gives results that make sense.
    
    rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
    #draw sample from the marginal density distributions
    
    rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
    #get quantiles from our resampled combined distributions
    
    Percentile <- names(rw.quants) %>% as.numeric()
    
    rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
    
    names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
    
    
    # Plot the combined functional relationship
    
    ribbon1 <- rw.quant.plot %>%
      dplyr::select(quant0.025:quant0.475, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
      arrange(name)%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) 
    
    ribbon2 <- rw.quant.plot %>%
      dplyr::select(quant0.525:quant0.975, Percentile) %>%
      pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
      arrange(desc(name))%>%
      group_by(name) %>%
      mutate(group =  cur_group_id()) %>%
      mutate(group = 20 - group)
    
    
    ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
    
    # calculate area of 95% credibility interval ----
    
    area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
      AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
    
    area
    
    # Plot ----
    
    len <- length(unique(rw.post.list[[1]]$model))
    metrics <- unique(rw.post.list[[1]]$metric)
  
    
    ggplot() + 
      xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
      ggtitle(paste0("Equal-weighted ensemble of ", len, " ", crop, " models")) +
      labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \nlandscape metrics: ", str_flatten(metrics), 
                            ". Area within the 95% credibility interval is ", round(area,2), "."))+
      geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
      geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
      theme_light() + theme(panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(),
                            panel.ontop = FALSE) +
      theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
      scale_alpha(range = c(0.05, 0.25))
    
    ggsave(paste0(str_flatten(metrics), crop, "03072024.jpg"), width = 6, height = 4, units = "in")
    
    
  
    
  # CORN: Get model list ----
  
  
  corn_models <-list.files("Output/", pattern = ("corn*"))
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- corn_models[!(corn_models %in% remove_datafiles)]
  
  
  corn_models <-list.files("Output/", pattern = ("corn.*RICH|RICH.*corn*"))# get just RICH-corn models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- corn_models[!(corn_models %in% remove_datafiles)] 
  
  corn_models <-list.files("Output/", pattern = ("corn.*RICH|RICH.*corn*|corn.*SDI|SDI.*corn*|corn.*SIDI|SIDI.*corn*")) #get several diversity - corn models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- corn_models[!(corn_models %in% remove_datafiles)]
  
  corn_models <-list.files("Output/", pattern = ("corn.*RICH|RICH.*corn*|corn.*SDI|SDI.*corn*|corn.*SIDI|SIDI.*corn*|corn.*RPR|RPR.*corn*|corn.*PRD|PRD.*corn*")) #get several diversity - corn models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- corn_models[!(corn_models %in% remove_datafiles)]
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  metrics <- unique(rw.post.list[[1]]$metric)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", crop, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \nlandscape metrics: ", str_flatten(metrics), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(metrics), crop, "03072024.jpg"), width = 6, height = 4, units = "in")
  
  
  
  
  # WHEAT: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat*"))
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- wwheat_models[!(wwheat_models %in% remove_datafiles)]
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH.*wwheat*"))# get just RICH-wwheat models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- wwheat_models[!(wwheat_models %in% remove_datafiles)] 
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH.*wwheat*|wwheat.*SDI|SDI.*wwheat*|wwheat.*SIDI|SIDI.*wwheat*")) #get several diversity - wwheat models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- wwheat_models[!(wwheat_models %in% remove_datafiles)]
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH.*wwheat*|wwheat.*SDI|SDI.*wwheat*|wwheat.*SIDI|SIDI.*wwheat*|wwheat.*RPR|RPR.*wwheat*|wwheat.*PRD|PRD.*wwheat*")) #get several diversity - wwheat models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- wwheat_models[!(wwheat_models %in% remove_datafiles)]
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  metrics <- unique(rw.post.list[[1]]$metric)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", crop, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \nlandscape metrics: ", str_flatten(metrics), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(metrics), crop, "03072024.jpg"), width = 6, height = 4, units = "in")
  

  # SOY: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy*"))
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- soy_models[!(soy_models %in% remove_datafiles)]
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH.*soy*"))# get just RICH-soy models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- soy_models[!(soy_models %in% remove_datafiles)] 
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH.*soy*|soy.*SDI|SDI.*soy*|soy.*SIDI|SIDI.*soy*")) #get several diversity - soy models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- soy_models[!(soy_models %in% remove_datafiles)]
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH.*soy*|soy.*SDI|SDI.*soy*|soy.*SIDI|SIDI.*soy*|soy.*RPR|RPR.*soy*|soy.*PRD|PRD.*soy*")) #get several diversity - soy models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- soy_models[!(soy_models %in% remove_datafiles)]
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  metrics <- unique(rw.post.list[[1]]$metric)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", crop, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \nlandscape metrics: ", str_flatten(metrics), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(metrics), crop, "03072024.jpg"), width = 6, height = 4, units = "in")
  
  # need to rerun this model: "RPR_CDL_BBOX_AGsoy_ntiles_02142024.rds"
  
  
  
  # HAY: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay*"))
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*"))# get just RICH-hay models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- hay_models[!(hay_models %in% remove_datafiles)] 
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*")) #get several diversity - hay models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*|hay.*RPR|RPR.*hay*|hay.*PRD|PRD.*hay*")) #get several diversity - hay models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  

  
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  metrics <- unique(rw.post.list[[1]]$metric)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", crop, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \nlandscape metrics: ", str_flatten(metrics), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(metrics), crop, "03072024.jpg"), width = 6, height = 4, units = "in")
  

  
# RICH: Get model list ----
  
  
 
  rich_models <-list.files("Output/", pattern = ("*RICH|RICH"))# get just RICH models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- rich_models[!(rich_models %in% remove_datafiles)] 
  
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  # 
  # 
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*|hay.*RPR|RPR.*hay*|hay.*PRD|PRD.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  
  
  
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  crops <- unique(rw.post.list[[1]]$crop)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", metric, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \ncrops: ", str_flatten(crops), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(crops), metric, "03072024.jpg"), width = 6, height = 4, units = "in")
  
  
  
# SDI: Get model list ----
  
  
  
  sdi_models <-list.files("Output/", pattern = ("*SDI|SDI"))# get just RICH models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- sdi_models[!(sdi_models %in% remove_datafiles)] 
  
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  # 
  # 
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*|hay.*RPR|RPR.*hay*|hay.*PRD|PRD.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  
  
  
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  crops <- unique(rw.post.list[[1]]$crop)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", metric, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \ncrops: ", str_flatten(crops), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(crops), metric, "03072024.jpg"), width = 6, height = 4, units = "in")
  
  
# SIDI: Get model list ----
  
  
  
  sidi_models <-list.files("Output/", pattern = ("*SIDI|SIDI"))# get just RICH models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- sidi_models[!(sidi_models %in% remove_datafiles)] 
  
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  # 
  # 
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*|hay.*RPR|RPR.*hay*|hay.*PRD|PRD.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  
  
  
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  crops <- unique(rw.post.list[[1]]$crop)
  
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", metric, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \ncrops: ", str_flatten(crops), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(crops), metric, "03072024.jpg"), width = 6, height = 4, units = "in")
  

# CDL RICH: Get model list ----
  
  
  
  rich_models <-list.files("Output/", pattern = ("RICH_CDL"))# get just RICH models
  remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  model_results <- rich_models[!(rich_models %in% remove_datafiles)] 
  
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  # 
  # 
  # hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*|hay.*RPR|RPR.*hay*|hay.*PRD|PRD.*hay*")) #get several diversity - hay models
  # remove_datafiles<-list.files("Output/", pattern = ("_02122024*"))
  # model_results <- hay_models[!(hay_models %in% remove_datafiles)]
  
  
  
  
  # Extract and stack marginals for landscape diversity metric from each model ----
  
  
  #initialize the stacked marginals dataframe
  
  rw.allpost <- data.frame()
  
  #loop through each model
  
  foreach (i=1:length(model_results)) %do% {
    
    results <- readRDS(paste0("Output/",model_results[i]))
    
    full_ML <- str_extract(model_results[i],"^.*?(AG|ALL)(?=[a-z])")
    metric <- str_extract(model_results[i],"^[^_]+")
    crop <- str_extract(model_results[i],"(?<=AG|ALL)[a-z]+")
    classification <- str_extract(model_results[i],"CDL|RC")
    boundaries <- str_extract(model_results[i],"(AG|ALL)(?=[a-z])")
    
    #extract the full random marginals for the diversity variable 
    # (which is always the 6th item in the random marginals list given the current model formulation script)
    
    rw.marg <- results$marginals.random[[6]] 
    
    
    ## Build out the marginals table ----  
    
    # create a list repeating each evaluation bin (percentile) from the random effect summary for each point in the posterior marginal distribution
    # also create a list that contains the true evaluation value (for the landscape metric)
    
    idlist <- rep(seq(from = 10, to = 100, by = 10), each = length(rw.marg$index.1[,1])) %>%
      as.numeric(.)
    
    # evallist <- rep(results$summary.random[[6]]$ID, each = length(rw.a$index.1[,1])) %>%
    #   as.numeric(.) # KN 03062024 - it may come in handy to know the eval values at some point --> but for some models summary.random still has fewer than 10 eval values so leave off
    
    # break these lists into a separate lists for each separate posterior marginal distribution evaluated for the RW1 effect
    
    id_split <- split(idlist, cut_number(idlist, length(rw.marg)))
    
    # eval_split <- split(evallist, cut_number(evallist, length(rw.marg)))
    
    # attach the ID and evaluation values to the marginal distribution lists
    
    rw.marg.table <- purrr::map2(rw.marg, id_split, cbind)
    
    # rw.marg.table <- purrr::map2(rw.marg.table, eval_split, cbind)
    
    
    
    ## Change data structure ----
    
    # Now we get rid of the annoying list of matrices and create one large dataframe of all
    # posterior marginal distribution values for each model
    
    combine.marg <- purrr::map(rw.marg.table, as.data.frame) #data.frame then bind rows
    
    fullmarg.table <- bind_rows(combine.marg, .id = "column_label") %>% 
      rename(Percentile = V3) %>% #, Value = V4) %>% 
      mutate(model = model_results[i], 
             metric = metric, 
             crop = crop,
             classification = classification,
             boundaries = boundaries)
    
    rownames(fullmarg.table) <- NULL
    
    
    ## Merge the dataframes for all models in list ----
    
    rw.allpost <- bind_rows(rw.allpost, fullmarg.table)
    
    return(rw.allpost)
    
    rm(results)
    
  }
  
  #Example plot of combined marginals at one evaluation point
  min <- min(rw.allpost$Percentile)
  
  ggplot(data = as.data.frame(rw.allpost %>% filter(Percentile == min))) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  # Now split out by common ID value since we want to average the posterior 
  # marginal distribution separately at each evaluation point
  
  rw.post.list <- split(rw.allpost, ~ Percentile)
  
  ggplot(data = rw.post.list[[5]]) +
    geom_point(aes(x=x, y=y)) +
    theme_minimal()
  
  
  #############################################################################
  ### Extract summary of EQUALLY combined posteriors - METHOD 3 - MANUAL!!! ### ----
  #############################################################################
  
  #Yay brute force! This gives results that make sense.
  
  rw.quants <- purrr::map(rw.post.list, ~sample(.$x, 1000, replace = TRUE, prob = .$y))
  #draw sample from the marginal density distributions
  
  rw.quants <- purrr::map(rw.quants, ~quantile(.,probs = seq(0.025,0.975,0.025)))
  #get quantiles from our resampled combined distributions
  
  Percentile <- names(rw.quants) %>% as.numeric()
  
  rw.quant.plot <- bind_rows(rw.quants) %>% cbind(Percentile) 
  
  names(rw.quant.plot) <- c(paste0("quant", seq(0.025,0.975,0.025)), "Percentile")
  
  
  # Plot the combined functional relationship
  
  ribbon1 <- rw.quant.plot %>%
    dplyr::select(quant0.025:quant0.475, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Min") %>%
    arrange(name)%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) 
  
  ribbon2 <- rw.quant.plot %>%
    dplyr::select(quant0.525:quant0.975, Percentile) %>%
    pivot_longer(cols = starts_with("quant"), values_to = "Max") %>%
    arrange(desc(name))%>%
    group_by(name) %>%
    mutate(group =  cur_group_id()) %>%
    mutate(group = 20 - group)
  
  
  ribbon <- left_join(ribbon1, ribbon2, by = c( "group","Percentile"))
  
  # calculate area of 95% credibility interval ----
  
  area <- AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Max[ribbon$name.y =="quant0.95"], method = "trapezoid") - 
    AUC(x = ribbon$Percentile[ribbon$name.y =="quant0.95"], y = ribbon$Min[ribbon$name.y =="quant0.95"], method = "trapezoid")
  
  area
  
  # Plot ----
  
  len <- length(unique(rw.post.list[[1]]$model))
  crops <- unique(rw.post.list[[1]]$crop)
  classification <- rw.post.list[[1]]$classification[1]
  
  ggplot() + 
    xlab("Landscape Complexity (Percentiles)") + ylab("Effect on log (Yield)") + 
    ggtitle(paste0("Equal-weighted ensemble of ", len, " ", classification, " ", metric, " models")) +
    labs(caption = paste0("Stacked posterior distributions for non-linear bayesian models with the \ncrops: ", str_flatten(crops), 
                          ". Area within the 95% credibility interval is ", round(area,2), "."))+
    geom_ribbon(data = ribbon, aes(x=Percentile, ymin = Min, ymax = Max, group = group, alpha = group), fill = "#9e9ac8") +
    geom_point(data=rw.quant.plot, aes(x=Percentile, y=`quant0.5`), color = "#756bb1") +
    theme_light() + theme(panel.grid.major.x = element_blank(),
                          panel.grid.minor.x = element_blank(),
                          panel.ontop = FALSE) +
    theme(legend.position = "none", plot.caption = element_text(hjust = 0, size = 8)) +
    scale_alpha(range = c(0.05, 0.25))
  
  ggsave(paste0(str_flatten(crops), metric, classification, "03072024.jpg"), width = 6, height = 4, units = "in")
  
  
  
  
  
  