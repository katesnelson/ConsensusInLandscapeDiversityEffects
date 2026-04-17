# Process Item #7
# Description: Script to create mixtures of results from Bayesian models. 
  # Combinations created via brute force.
# Author: Kate Nelson
# Date updated: 04/25/2024



library(pacman)
p_load(tidyverse, INLA, doParallel, foreach, DescTools, inflection)

source('Scripts/SA_functions_02122024-0.R') # use to get 'stackit' function for combining model results into a single dataframe

  
  # All MODELS: Get model list ----
  
  
  all_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|^D")) 
  model_results <- all_models
  
  stackit(model_results)
  
  
  ## ALL CDL MODELS: Get model list ----
  
  
  CDL_models <-list.files("Output/", pattern = ("*RICH_CDL|RICH_CDL.*|.*SDI_CDL|SDI_CDL.*|.*SIDI_CDL|SIDI_CDL.*|.*RPR_CDL|RPR_CDL.*|^D_CDL")) 
  model_results <- CDL_models
  
  
  stackit(model_results)
  
  ## ALL RC MODELS: Get model list ----
  
  
  RC_models <-list.files("Output/", pattern = ("*RICH_RC|RICH_RC.*|.*SDI_RC|SDI_RC.*|.*SIDI_RC|SIDI_RC.*|.*RPR_RC|RPR_RC.*|.*D_RC|D_RC.*")) 
  model_results <- RC_models
  
  
  stackit(model_results)
  
  ## ALL BBOX MODELS: Get model list ----
  
  
  BBOX_models <-list.files("Output/", pattern = ("BBOX.*RICH|RICH.*BBOX*|BBOX.*SDI|SDI.*BBOX*|BBOX.*SIDI|SIDI.*BBOX*|BBOX.*RPR|RPR.*BBOX*|BBOX.*D|D.*BBOX*")) 
  model_results <- BBOX_models
  
  
  stackit(model_results)
  
  
  ## ALL BNDRY MODELS: Get model list ----
  
  
  BNDRY_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|.*D|D.*"))
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- BNDRY_models[!(BNDRY_models %in% remove_datafiles)]
  
  stackit(model_results)
  

  ## ALL AG MODELS: Get model list ----
  
  
  AG_models <-list.files("Output/", pattern = ("RICH.*_AG|SDI.*_AG|SIDI.*_AG|RPR.*_AG|D.*_AG")) 
  model_results <- AG_models
  
  
  stackit(model_results)
  
  
  ## ALL ALL (Landscape) MODELS: Get model list ----
  
  
  ALL_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*|ALL.*SDI|SDI.*ALL*|ALL.*SIDI|SIDI.*ALL*|ALL.*RPR|RPR.*ALL*|ALL.*D|D.*ALL*")) 
  model_results <- ALL_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH|RICH.*corn*|corn.*SDI|SDI.*corn*|corn.*SIDI|SIDI.*corn*|corn.*RPR|RPR.*corn*|corn.*D|D.*corn*")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY MODELS: Get model list ----
  
  
 soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH.*soy*|soy.*SDI|SDI.*soy*|soy.*SIDI|SIDI.*soy*|soy.*RPR|RPR.*soy*|soy.*D|D.*soy*")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH.*wwheat*|wwheat.*SDI|SDI.*wwheat*|wwheat.*SIDI|SIDI.*wwheat*|wwheat.*RPR|RPR.*wwheat*|wwheat.*D|D.*wwheat*")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH.*alfalfa*|alfalfa.*SDI|SDI.*alfalfa*|alfalfa.*SIDI|SIDI.*alfalfa*|alfalfa.*RPR|RPR.*alfalfa*|alfalfa.*D|D.*alfalfa*")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
 
  ## ALL HAY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay*|hay.*SDI|SDI.*hay*|hay.*SIDI|SIDI.*hay*|hay.*RPR|RPR.*hay*|hay.*D|D.*hay*")) 
  model_results <- hay_models
  
  
  stackit(model_results) 
  
  
  ## ALL RICH MODELS: Get model list ----
  
  
  RICH_models <-list.files("Output/", pattern = ("*RICH|RICH*")) 
  model_results <- RICH_models
  
  
  stackit(model_results) 
  
  
  ## ALL SDI MODELS: Get model list ----
  
  
  SDI_models <-list.files("Output/", pattern = ("*SDI|SDI*")) 
  model_results <- SDI_models
  
  
  stackit(model_results) 
  
  
  ## ALL SIDI MODELS: Get model list ----
  
  
  SIDI_models <-list.files("Output/", pattern = ("*SIDI|SIDI*")) 
  model_results <- SIDI_models
  
  
  stackit(model_results) 
  
  
  ## ALL RPR MODELS: Get model list ----
  
  
  RPR_models <-list.files("Output/", pattern = ("*RPR|RPR*")) 
  model_results <- RPR_models
  
  
  stackit(model_results) 
  
  
  ## ALL D MODELS: Get model list ----
  
  
  D_models <-list.files("Output/", pattern = ("D_.*")) 
  model_results <- D_models
  
  
  stackit(model_results) 
  
  
  # CROP-CLASSIFICATION COMBOS ----
  
  ## ALL CORN-CDL MODELS: Get model list ----
  
  corncdl_models <-list.files("Output/", pattern = ("corn.*RICH_CDL|RICH_CDL.*corn*|corn.*SDI_CDL|SDI_CDL.*corn*|corn.*SIDI_CDL|SIDI_CDL.*corn*|corn.*RPR_CDL|RPR_CDL.*corn*|corn.*D_CDL|D_CDL.*corn*")) ####get several diversity - corn models
  model_results <- corncdl_models
  
  
  stackit(model_results) 
  
  ## ALL SOY-CDL MODELS: Get model list ----
  
  soycdl_models <-list.files("Output/", pattern = ("soy.*RICH_CDL|RICH_CDL.*soy*|soy.*SDI_CDL|SDI_CDL.*soy*|soy.*SIDI_CDL|SIDI_CDL.*soy*|soy.*RPR_CDL|RPR_CDL.*soy*|soy.*D_CDL|D_CDL.*soy*")) ##get several diversity - soy models
  model_results <- soycdl_models
  
  stackit(model_results) 
  
  ## ALL WHEAT-CDL MODELS: Get model list ----
  
  wwheatcdl_models <-list.files("Output/", pattern = ("wwheat.*RICH_CDL|RICH_CDL.*wwheat*|wwheat.*SDI_CDL|SDI_CDL.*wwheat*|wwheat.*SIDI_CDL|SIDI_CDL.*wwheat*|wwheat.*RPR_CDL|RPR_CDL.*wwheat*|wwheat.*D_CDL|D_CDL.*wwheat*")) ##get several diversity - wwheat models
  model_results <- wwheatcdl_models
  
  stackit(model_results) 
  
  
  ## ALL ALFALFA-CDL MODELS: Get model list ----
  
  alfalfacdl_models <-list.files("Output/", pattern = ("alfalfa.*RICH_CDL|RICH_CDL.*alfalfa*|alfalfa.*SDI_CDL|SDI_CDL.*alfalfa*|alfalfa.*SIDI_CDL|SIDI_CDL.*alfalfa*|alfalfa.*RPR_CDL|RPR_CDL.*alfalfa*|alfalfa.*D_CDL|D_CDL.*alfalfa*")) ##get several diversity - alfalfa models
  model_results <- alfalfacdl_models
  
  stackit(model_results) 
  
  
  
  ## ALL HAY-CDL MODELS: Get model list ----
  
  haycdl_models <-list.files("Output/", pattern = ("hay.*RICH_CDL|RICH_CDL.*hay*|hay.*SDI_CDL|SDI_CDL.*hay*|hay.*SIDI_CDL|SIDI_CDL.*hay*|hay.*RPR_CDL|RPR_CDL.*hay*|hay.*D_CDL|D_CDL.*hay*")) ##get several diversity - hay models
  model_results <- haycdl_models
  
  stackit(model_results) 
  
  
  
  ## ALL CORN-RC MODELS: Get model list ----
  
  cornrc_models <-list.files("Output/", pattern = ("corn.*RICH_RC|RICH_RC.*corn*|corn.*SDI_RC|SDI_RC.*corn*|corn.*SIDI_RC|SIDI_RC.*corn*|corn.*RPR_RC|RPR_RC.*corn*|corn.*D_RC|D_RC.*corn*")) ##get several diversity - corn models
  model_results <- cornrc_models
  
  stackit(model_results) 
  
  
  
  ## ALL SOY-RC MODELS: Get model list ----
  
  soyrc_models <-list.files("Output/", pattern = ("soy.*RICH_RC|RICH_RC.*soy*|soy.*SDI_RC|SDI_RC.*soy*|soy.*SIDI_RC|SIDI_RC.*soy*|soy.*RPR_RC|RPR_RC.*soy*|soy.*D_RC|D_RC.*soy*")) ##get several diversity - soy models
  model_results <- soyrc_models
  
  stackit(model_results) 
  
  
  
  ## ALL WHEAT-RC MODELS: Get model list ----
  
  wwheatrc_models <-list.files("Output/", pattern = ("wwheat.*RICH_RC|RICH_RC.*wwheat*|wwheat.*SDI_RC|SDI_RC.*wwheat*|wwheat.*SIDI_RC|SIDI_RC.*wwheat*|wwheat.*RPR_RC|RPR_RC.*wwheat*|wwheat.*D_RC|D_RC.*wwheat*")) ##get several diversity - wwheat models
  model_results <- wwheatrc_models
  
  stackit(model_results) 
  
  
  ## ALL ALFALFA-RC MODELS: Get model list ----
  
  alfalfarc_models <-list.files("Output/", pattern = ("alfalfa.*RICH_RC|RICH_RC.*alfalfa*|alfalfa.*SDI_RC|SDI_RC.*alfalfa*|alfalfa.*SIDI_RC|SIDI_RC.*alfalfa*|alfalfa.*RPR_RC|RPR_RC.*alfalfa*|alfalfa.*D_RC|D_RC.*alfalfa*")) ##get several diversity - alfalfa models
  model_results <- alfalfarc_models
  
  stackit(model_results) 
  
  
  ## ALL HAY-RC MODELS: Get model list ----
  
  hayrc_models <-list.files("Output/", pattern = ("hay.*RICH_RC|RICH_RC.*hay*|hay.*SDI_RC|SDI_RC.*hay*|hay.*SIDI_RC|SIDI_RC.*hay*|hay.*RPR_RC|RPR_RC.*hay*|hay.*D_RC|D_RC.*hay*")) ##get several diversity - hay models
  model_results <- hayrc_models
  
  
  stackit(model_results) 
  
  
  # CROP-SCOPE COMBOS ----
  
  ## ALL CORN-ALL MODELS: Get model list ----
  
  all_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*|ALL.*SDI|SDI.*ALL*|ALL.*SIDI|SIDI.*ALL*|ALL.*RPR|RPR.*ALL*|ALL.*D|D.*ALL*"))
  corn_datafiles<-list.files("Output/", pattern = ("corn*"))
  model_results <- all_models[(all_models %in% corn_datafiles)]
  
  
  stackit(model_results) 
  
  ## ALL SOY-ALL MODELS: Get model list ----
  
  all_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*|ALL.*SDI|SDI.*ALL*|ALL.*SIDI|SIDI.*ALL*|ALL.*RPR|RPR.*ALL*|ALL.*D|D.*ALL*"))
  soy_datafiles<-list.files("Output/", pattern = ("soy*"))
  model_results <- all_models[(all_models %in% soy_datafiles)]
  
  stackit(model_results) 
  
  ## ALL WHEAT-ALL MODELS: Get model list ----
  
  all_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*|ALL.*SDI|SDI.*ALL*|ALL.*SIDI|SIDI.*ALL*|ALL.*RPR|RPR.*ALL*|ALL.*D|D.*ALL*"))
  wwheat_datafiles<-list.files("Output/", pattern = ("wwheat*"))
  model_results <- all_models[(all_models %in% wwheat_datafiles)]
  
  stackit(model_results) 
  
  
  ## ALL ALFALFA-ALL MODELS: Get model list ----
  
  all_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*|ALL.*SDI|SDI.*ALL*|ALL.*SIDI|SIDI.*ALL*|ALL.*RPR|RPR.*ALL*|ALL.*D|D.*ALL*"))
  alfalfa_datafiles<-list.files("Output/", pattern = ("alfalfa*"))
  model_results <- all_models[(all_models %in% alfalfa_datafiles)]
  
  
  stackit(model_results) 
  
  
  
  ## ALL HAY-ALL MODELS: Get model list ----
  
  all_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*|ALL.*SDI|SDI.*ALL*|ALL.*SIDI|SIDI.*ALL*|ALL.*RPR|RPR.*ALL*|ALL.*D|D.*ALL*"))
  hay_datafiles<-list.files("Output/", pattern = ("hay*"))
  model_results <- all_models[(all_models %in% hay_datafiles)]
  
  stackit(model_results) 
  
  
  
  ## ALL CORN-AG MODELS: Get model list ----
  
  ag_models <-list.files("Output/", pattern = ("RICH.*_AG|SDI.*_AG|SIDI.*_AG|RPR.*_AG|D.*_AG"))
  corn_datafiles<-list.files("Output/", pattern = ("corn*"))
  model_results <- ag_models[(ag_models %in% corn_datafiles)]
  
  stackit(model_results) 
  
  
  
  ## ALL SOY-AG MODELS: Get model list ----
  
  ag_models <-list.files("Output/", pattern = ("RICH.*_AG|SDI.*_AG|SIDI.*_AG|RPR.*_AG|D.*_AG"))
  soy_datafiles<-list.files("Output/", pattern = ("soy*"))
  model_results <- ag_models[(ag_models %in% soy_datafiles)]
  
  stackit(model_results) 
  
  
  
  ## ALL WHEAT-AG MODELS: Get model list ----
  
  ag_models <-list.files("Output/", pattern = ("RICH.*_AG|SDI.*_AG|SIDI.*_AG|RPR.*_AG|D.*_AG"))
  wwheat_datafiles<-list.files("Output/", pattern = ("wwheat*"))
  model_results <- ag_models[(ag_models %in% wwheat_datafiles)]
  
  stackit(model_results) 
  
  
  ## ALL ALFALFA-AG MODELS: Get model list ----
  
  ag_models <-list.files("Output/", pattern = ("RICH.*_AG|SDI.*_AG|SIDI.*_AG|RPR.*_AG|D.*_AG"))
  alfalfa_datafiles<-list.files("Output/", pattern = ("alfalfa*"))
  model_results <- ag_models[(ag_models %in% alfalfa_datafiles)]
  
  stackit(model_results) 
  
  
  ## ALL HAY-AG MODELS: Get model list ----
  
  ag_models <-list.files("Output/", pattern = ("RICH.*_AG|SDI.*_AG|SIDI.*_AG|RPR.*_AG|D.*_AG"))
  hay_datafiles<-list.files("Output/", pattern = ("hay*"))
  model_results <- ag_models[(ag_models %in% hay_datafiles)]
  
  
  stackit(model_results) 
  
  
  
  # CROP-BOUNDARY COMBOS ----
  
  ## ALL CORN-BBOX MODELS: Get model list ----
  
  bbox_models <-list.files("Output/", pattern = ("BBOX.*RICH|RICH.*BBOX*|BBOX.*SDI|SDI.*BBOX*|BBOX.*SIDI|SIDI.*BBOX*|BBOX.*RPR|RPR.*BBOX*|BBOX.*D|D.*BBOX*"))
  corn_datafiles<-list.files("Output/", pattern = ("corn*"))
  model_results <- bbox_models[(bbox_models %in% corn_datafiles)]
  
  
  stackit(model_results) 
  
  ## ALL SOY-BBOX MODELS: Get model list ----
  
  bbox_models <-list.files("Output/", pattern = ("BBOX.*RICH|RICH.*BBOX*|BBOX.*SDI|SDI.*BBOX*|BBOX.*SIDI|SIDI.*BBOX*|BBOX.*RPR|RPR.*BBOX*|BBOX.*D|D.*BBOX*"))
  soy_datafiles<-list.files("Output/", pattern = ("soy*"))
  model_results <- bbox_models[(bbox_models %in% soy_datafiles)]
  
  stackit(model_results) 
  
  ## ALL WHEAT-BBOX MODELS: Get model list ----
  
  bbox_models <-list.files("Output/", pattern = ("BBOX.*RICH|RICH.*BBOX*|BBOX.*SDI|SDI.*BBOX*|BBOX.*SIDI|SIDI.*BBOX*|BBOX.*RPR|RPR.*BBOX*|BBOX.*D|D.*BBOX*"))
  wwheat_datafiles<-list.files("Output/", pattern = ("wwheat*"))
  model_results <- bbox_models[(bbox_models %in% wwheat_datafiles)]
  
  stackit(model_results) 
  
  
  ## ALL ALFALFA-BBOX MODELS: Get model list ----
  
  bbox_models <-list.files("Output/", pattern = ("BBOX.*RICH|RICH.*BBOX*|BBOX.*SDI|SDI.*BBOX*|BBOX.*SIDI|SIDI.*BBOX*|BBOX.*RPR|RPR.*BBOX*|BBOX.*D|D.*BBOX*"))
  alfalfa_datafiles<-list.files("Output/", pattern = ("alfalfa*"))
  model_results <- bbox_models[(bbox_models %in% alfalfa_datafiles)]
  
  
  stackit(model_results) 
  
  
  ## ALL HAY-BBOX MODELS: Get model list ----
  
  bbox_models <-list.files("Output/", pattern = ("BBOX.*RICH|RICH.*BBOX*|BBOX.*SDI|SDI.*BBOX*|BBOX.*SIDI|SIDI.*BBOX*|BBOX.*RPR|RPR.*BBOX*|BBOX.*D|D.*BBOX*"))
  hay_datafiles<-list.files("Output/", pattern = ("hay*"))
  model_results <- bbox_models[(bbox_models %in% hay_datafiles)]
  
  stackit(model_results) 
  
  
  
  ## ALL CORN-CNTY MODELS: Get model list ----
  
  cnty_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|.*D|D.*"))
  corn_datafiles<-list.files("Output/", pattern = ("corn*"))
  model_results <- cnty_models[(cnty_models %in% corn_datafiles)]
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- model_results[!(model_results %in% remove_datafiles)]
  
  stackit(model_results) 
  
  
  
  ## ALL SOY-CNTY MODELS: Get model list ----
  
  cnty_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|.*D|D.*"))
  soy_datafiles<-list.files("Output/", pattern = ("soy*"))
  model_results <- cnty_models[(cnty_models %in% soy_datafiles)]
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- model_results[!(model_results %in% remove_datafiles)]
  
  stackit(model_results) 
  
  
  
  ## ALL WHEAT-CNTY MODELS: Get model list ----
  
  cnty_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|.*D|D.*"))
  wwheat_datafiles<-list.files("Output/", pattern = ("wwheat*"))
  model_results <- cnty_models[(cnty_models %in% wwheat_datafiles)]
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- model_results[!(model_results %in% remove_datafiles)]
  
  stackit(model_results) 
  
  
  ## ALL ALFALFA-CNTY MODELS: Get model list ----
  
  cnty_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|.*D|D.*"))
  alfalfa_datafiles<-list.files("Output/", pattern = ("alfalfa*"))
  model_results <- cnty_models[(cnty_models %in% alfalfa_datafiles)]
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- model_results[!(model_results %in% remove_datafiles)]
  
  stackit(model_results) 
  
  
  ## ALL HAY-CNTY MODELS: Get model list ----
  
  cnty_models <-list.files("Output/", pattern = ("*RICH|RICH.*|.*SDI|SDI.*|.*SIDI|SIDI.*|.*RPR|RPR.*|.*D|D.*"))
  hay_datafiles<-list.files("Output/", pattern = ("hay*"))
  model_results <- cnty_models[(cnty_models %in% hay_datafiles)]
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- model_results[!(model_results %in% remove_datafiles)]
  
  
  stackit(model_results) 
  
  
  # METRIC-CLASSIFICATION COMBOS ----
  
  
  ## ALL RICH-CDL MODELS: Get model list ----
  
  
  RICHCDL_models <-list.files("Output/", pattern = ("RICH_CDL"))## get just RICH models
  model_results <- RICHCDL_models
  
  stackit(model_results) 
  
  
  ## ALL SDI-CDL MODELS: Get model list ----
  
  
  SDICDL_models <-list.files("Output/", pattern = ("SDI_CDL"))## get just SDI models
  model_results <- SDICDL_models
  
  stackit(model_results)
  
  
  ## ALL SIDI-CDL MODELS: Get model list ----
  
  
  SIDICDL_models <-list.files("Output/", pattern = ("SIDI_CDL"))## get just SIDI models
  model_results <- SIDICDL_models
  
  stackit(model_results)
  
  
  ## ALL RPR-CDL MODELS: Get model list ----
  
  
  RPRCDL_models <-list.files("Output/", pattern = ("RPR_CDL"))## get just RPR models
  model_results <- RPRCDL_models
  
  stackit(model_results)
  
  
  ## ALL D-CDL MODELS: Get model list ----
  
  
  DCDL_models <-list.files("Output/", pattern = ("D_CDL"))## get just D models
  model_results <- DCDL_models
  
  stackit(model_results)
  
  
  
  ## ALL RICH-RC MODELS: Get model list ----
  
  
  RICHrc_models <-list.files("Output/", pattern = ("RICH_RC"))## get just RICH models
  model_results <- RICHrc_models
  
  stackit(model_results) 
  
  
  ## ALL SDI-RC MODELS: Get model list ----
  
  
  SDIrc_models <-list.files("Output/", pattern = ("SDI_RC"))## get just SDI models
  model_results <- SDIrc_models
  
  stackit(model_results)
  
  
  ## ALL SIDI-RC MODELS: Get model list ----
  
  
  SIDIrc_models <-list.files("Output/", pattern = ("SIDI_RC"))## get just SIDI models
  model_results <- SIDIrc_models
  
  stackit(model_results)
  
  
  ## ALL RPR-RC MODELS: Get model list ----
  
  
  RPRrc_models <-list.files("Output/", pattern = ("RPR_RC"))## get just RPR models
  model_results <- RPRrc_models
  
  stackit(model_results)
  
  
  ## ALL D-RC MODELS: Get model list ----
  
  
  Drc_models <-list.files("Output/", pattern = ("D_RC"))## get just D models
  model_results <- Drc_models
  
  stackit(model_results)
  
  
  # METRIC-SCOPE COMBOS ----
  
  
  ## ALL RICH-ALL MODELS: Get model list ----
  
  
  RICHALL_models <-list.files("Output/", pattern = ("ALL.*RICH|RICH.*ALL*"))## get just RICH models
  model_results <- RICHALL_models
  
  stackit(model_results) 
  
  
  ## ALL SDI-ALL MODELS: Get model list ----
  
  
  SDIALL_models <-list.files("Output/", pattern = ("ALL.*SDI|SDI.*ALL*"))## get just SDI models
  model_results <- SDIALL_models
  
  stackit(model_results)
  
  
  ## ALL SIDI-ALL MODELS: Get model list ----
  
  
  SIDIALL_models <-list.files("Output/", pattern = ("ALL.*SIDI|SIDI.*ALL*"))## get just SIDI models
  model_results <- SIDIALL_models
  
  stackit(model_results)
  
  
  ## ALL RPR-ALL MODELS: Get model list ----
  
  
  RPRALL_models <-list.files("Output/", pattern = ("ALL.*RPR|RPR.*ALL*"))## get just RPR models
  model_results <- RPRALL_models
  
  stackit(model_results)
  
  
  ## ALL D-ALL MODELS: Get model list ----
  
  
  DALL_models <-list.files("Output/", pattern = ("D_.*ALL*"))## get just D models
  model_results <- DALL_models
  
  stackit(model_results)
  
  
  
  ## ALL RICH-AG MODELS: Get model list ----
  
  
  RICHAG_models <-list.files("Output/", pattern = ("AG.*RICH|RICH.*AG"))## get just RICH models
  model_results <- RICHAG_models
  
  stackit(model_results) 
  
  
  ## ALL SDI-AG MODELS: Get model list ----
  
  
  SDIAG_models <-list.files("Output/", pattern = ("AG.*SDI|SDI.*AG"))## get just SDI models
  model_results <- SDIAG_models
  
  stackit(model_results)
  
  
  ## ALL SIDI-AG MODELS: Get model list ----
  
  
  SIDIAG_models <-list.files("Output/", pattern = ("AG.*SIDI|SIDI.*AG"))## get just SIDI models
  model_results <- SIDIAG_models
  
  stackit(model_results)
  
  
  ## ALL RPR-AG MODELS: Get model list ----
  
  
  RPRAG_models <-list.files("Output/", pattern = ("AG.*RPR|RPR.*AG"))## get just RPR models
  model_results <- RPRAG_models
  
  stackit(model_results)
  
  
  ## ALL D-AG MODELS: Get model list ----
  
  
  DAG_models <-list.files("Output/", pattern = ("D_.*AG"))## get just D models
  model_results <- DAG_models
  
  stackit(model_results)
  
  
  
  # METRIC-BOUNDARY COMBOS ----
  
  
  ## ALL RICH-BBOX MODELS: Get model list ----
  
  
  RICHBBOX_models <-list.files("Output/", pattern = ("RICH.*BBOX*"))## get just RICH models
  model_results <- RICHBBOX_models
  
  stackit(model_results) 
  
  
  ## ALL SDI-BBOX MODELS: Get model list ----
  
  
  SDIBBOX_models <-list.files("Output/", pattern = ("SDI.*BBOX*"))## get just SDI models
  model_results <- SDIBBOX_models
  
  stackit(model_results)
  
  
  ## ALL SIDI-BBOX MODELS: Get model list ----
  
  
  SIDIBBOX_models <-list.files("Output/", pattern = ("SIDI.*BBOX*"))## get just SIDI models
  model_results <- SIDIBBOX_models
  
  stackit(model_results)
  
  
  ## ALL RPR-BBOX MODELS: Get model list ----
  
  
  RPRBBOX_models <-list.files("Output/", pattern = ("RPR.*BBOX*"))## get just RPR models
  model_results <- RPRBBOX_models
  
  stackit(model_results)
  
  
  ## ALL D-BBOX MODELS: Get model list ----
  
  
  DBBOX_models <-list.files("Output/", pattern = ("D_.*BBOX*"))## get just D models
  model_results <- DBBOX_models
  
  stackit(model_results)
  
  
  
  ## ALL RICH-CNTY MODELS: Get model list ----
  
  
  RICHCNTY_models <-list.files("Output/", pattern = ("RICH"))## get just RICH models
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- RICHCNTY_models[!(RICHCNTY_models %in% remove_datafiles)]
  
   stackit(model_results) 
  
  
  ## ALL SDI-CNTY MODELS: Get model list ----
  
  
  SDICNTY_models <-list.files("Output/", pattern = ("SDI"))## get just SDI models
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- SDICNTY_models[!(SDICNTY_models %in% remove_datafiles)]
  
  stackit(model_results)
  
  
  ## ALL SIDI-CNTY MODELS: Get model list ----
  
  
  SIDICNTY_models <-list.files("Output/", pattern = ("SIDI"))## get just SIDI models
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- SIDICNTY_models[!(SIDICNTY_models %in% remove_datafiles)]
  
  stackit(model_results)
  
  
  ## ALL RPR-CNTY MODELS: Get model list ----
  
  
  RPRCNTY_models <-list.files("Output/", pattern = ("RPR"))## get just RPR models
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- RPRCNTY_models[!(RPRCNTY_models %in% remove_datafiles)]
  
  stackit(model_results)
  
  
  ## ALL D-CNTY MODELS: Get model list ----
  
  
  DCNTY_models <-list.files("Output/", pattern = ("D_"))## get just D models, 12122024 add "_" to address overpulling
  remove_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- DCNTY_models[!(DCNTY_models %in% remove_datafiles)]
  
  stackit(model_results)
  
  
  
  # CROP-METRIC COMBOS ----
  
  ## ALL CORN-RICH MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH|RICH.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results) 
  
  
  ## ALL CORN-SDI MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI|SDI.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI|SDI.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI|SDI.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI|SDI.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI|SDI.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SIDI MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI|SIDI.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI|SIDI.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI|SIDI.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI|SIDI.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI|SIDI.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-D MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("^D.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("^D.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("^D.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("^D.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("^D.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-RPR MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR|RPR.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR|RPR.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR|RPR.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR|RPR.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR|RPR.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  # CROP-METRIC-CLASSIFICATION COMBOS ----
  
  ## ALL CORN-RICH-CDL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH_CDL|RICH_CDL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-CDL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH_CDL|RICH_CDL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-CDL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH_CDL|RICH_CDL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-CDL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH_CDL|RICH_CDL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-CDL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH_CDL|RICH_CDL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-CDL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI_CDL|SDI_CDL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-CDL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI_CDL|SDI_CDL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-CDL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI_CDL|SDI_CDL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-CDL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI_CDL|SDI_CDL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-CDL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI_CDL|SDI_CDL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-CDL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI_CDL|SIDI_CDL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-CDL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI_CDL|SIDI_CDL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-CDL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI_CDL|SIDI_CDL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-CDL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI_CDL|SIDI_CDL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-CDL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI_CDL|SIDI_CDL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-CDL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*D_CDL|D_CDL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-CDL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*D_CDL|D_CDL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-CDL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*D_CDL|D_CDL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-CDL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*D_CDL|D_CDL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-CDL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*D_CDL|D_CDL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-CDL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR_CDL|RPR_CDL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-CDL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR_CDL|RPR_CDL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-CDL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR_CDL|RPR_CDL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-CDL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR_CDL|RPR_CDL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-CDL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR_CDL|RPR_CDL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-RICH-RC MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH_RC|RICH_RC.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-RC MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH_RC|RICH_RC.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-RC MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH_RC|RICH_RC.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-RC MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH_RC|RICH_RC.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-RC MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH_RC|RICH_RC.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-RC MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI_RC|SDI_RC.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-RC MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI_RC|SDI_RC.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-RC MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI_RC|SDI_RC.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-RC MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI_RC|SDI_RC.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-RC MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI_RC|SDI_RC.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-RC MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI_RC|SIDI_RC.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-RC MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI_RC|SIDI_RC.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-RC MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI_RC|SIDI_RC.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-RC MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI_RC|SIDI_RC.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-RC MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI_RC|SIDI_RC.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-RC MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*D_RC|D_RC.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-RC MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*D_RC|D_RC.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-RC MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*D_RC|D_RC.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-RC MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*D_RC|D_RC.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-RC MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*D_RC|D_RC.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-RC MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR_RC|RPR_RC.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-RC MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR_RC|RPR_RC.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-RC MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR_RC|RPR_RC.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-RC MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR_RC|RPR_RC.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-RC MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR_RC|RPR_RC.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  # CROP-METRIC-BOUNDARY COMBOS ----
  
  ## ALL CORN-RICH-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH.*BBOX.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_.*BBOX.*corn")) # 12122024 fix issues with D call pulling up other models that include a letter d
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  ## ALL CORN-RICH-CNTY MODELS: Get model list ----
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH|RICH.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI|SDI.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI|SDI.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI|SDI.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI|SDI.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI|SDI.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI|SIDI.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI|SIDI.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI|SIDI.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI|SIDI.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI|SIDI.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR|RPR.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR|RPR.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR|RPR.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR|RPR.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))    
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR|RPR.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  
  # CROP-METRIC-SCOPE COMBOS ----
  
  ## ALL CORN-RICH-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH.*ALL|RICH.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH.*ALL|RICH.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH.*ALL|RICH.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH.*ALL|RICH.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH.*ALL|RICH.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI.*ALL|SDI.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI.*ALL|SDI.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI.*ALL|SDI.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI.*ALL|SDI.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI.*ALL|SDI.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI.*ALL|SIDI.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI.*ALL|SIDI.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI.*ALL|SIDI.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI.*ALL|SIDI.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI.*ALL|SIDI.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR.*ALL|RPR.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR.*ALL|RPR.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR.*ALL|RPR.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR.*ALL|RPR.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR.*ALL|RPR.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-RICH-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH.*AG|RICH.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH.*AG|RICH.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH.*AG|RICH.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH.*AG|RICH.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH.*AG|RICH.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI.*AG|SDI.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI.*AG|SDI.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI.*AG|SDI.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI.*AG|SDI.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI.*AG|SDI.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI.*AG|SIDI.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI.*AG|SIDI.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI.*AG|SIDI.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI.*AG|SIDI.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI.*AG|SIDI.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR.*AG|RPR.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR.*AG|RPR.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR.*AG|RPR.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR.*AG|RPR.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR.*AG|RPR.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  # CROP-METRIC-CLASSIFICATION-BOUNDARY COMBOS ----
  
  ## ALL CORN-RICH-CDL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_CDL.*BBOX.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-CDL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_CDL.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-CDL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_CDL.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-CDL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_CDL.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-CDL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_CDL.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-CDL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_CDL.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-CDL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_CDL.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-CDL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_CDL.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-CDL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_CDL.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-CDL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_CDL.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-CDL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_CDL.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-CDL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_CDL.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-CDL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_CDL.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-CDL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_CDL.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-CDL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_CDL.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-CDL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_CDL.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-CDL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_CDL.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-CDL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_CDL.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-CDL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_CDL.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-CDL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_CDL.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-CDL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_CDL.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-CDL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_CDL.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-CDL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_CDL.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-CDL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_CDL.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-CDL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_CDL.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  
  ## ALL CORN-RICH-RC-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_RC.*BBOX.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-RC-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_RC.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-RC-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_RC.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-RC-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_RC.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-RC-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_RC.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-RC-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_RC.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-RC-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_RC.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-RC-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_RC.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-RC-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_RC.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-RC-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_RC.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-RC-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_RC.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-RC-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_RC.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-RC-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_RC.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-RC-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_RC.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-RC-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_RC.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-RC-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_RC.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-RC-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_RC.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-RC-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_RC.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-RC-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_RC.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-RC-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_RC.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-RC-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_RC.*BBOX.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-RC-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_RC.*BBOX.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-RC-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_RC.*BBOX.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-RC-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_RC.*BBOX.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-RC-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_RC.*BBOX.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  
  
  ## ALL CORN-RICH-CDL-CNTY MODELS: Get model list ----
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH|RICH_CDL.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-CDL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH_CDL.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-CDL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH_CDL.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-CDL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH_CDL.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-CDL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH_CDL.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-CDL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI|SDI_CDL.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-CDL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI|SDI_CDL.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-CDL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI|SDI_CDL.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-CDL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI|SDI_CDL.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-CDL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI|SDI_CDL.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-CDL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI|SIDI_CDL.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-CDL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI|SIDI_CDL.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-CDL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI|SIDI_CDL.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-CDL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI|SIDI_CDL.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-CDL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI|SIDI_CDL.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-CDL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*D|D_CDL.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-CDL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*D|D_CDL.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-CDL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*D|D_CDL.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-CDL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*D|D_CDL.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-CDL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*D|D_CDL.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-CDL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR|RPR_CDL.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-CDL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR|RPR_CDL.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-CDL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR|RPR_CDL.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-CDL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR|RPR_CDL.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))    
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-CDL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR|RPR_CDL.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-RICH-RC-CNTY MODELS: Get model list ----
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RICH|RICH_RC.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-RC-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RICH|RICH_RC.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-RC-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RICH|RICH_RC.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-RC-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RICH|RICH_RC.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-RC-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RICH|RICH_RC.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-RC-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SDI|SDI_RC.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-RC-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SDI|SDI_RC.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-RC-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SDI|SDI_RC.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-RC-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SDI|SDI_RC.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-RC-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SDI|SDI_RC.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-RC-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*SIDI|SIDI_RC.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-RC-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*SIDI|SIDI_RC.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-RC-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*SIDI|SIDI_RC.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-RC-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*SIDI|SIDI_RC.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-RC-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*SIDI|SIDI_RC.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-RC-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*D|D_RC.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-RC-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*D|D_RC.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-RC-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*D|D_RC.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-RC-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*D|D_RC.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-RC-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*D|D_RC.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-RC-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("corn.*RPR|RPR_RC.*corn")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-RC-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("soy.*RPR|RPR_RC.*soy")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-RC-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("wwheat.*RPR|RPR_RC.*wwheat")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-RC-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("alfalfa.*RPR|RPR_RC.*alfalfa")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))    
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-RC-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("hay.*RPR|RPR_RC.*hay")) 
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))     
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  
  
  
  
  
  
  
  
  # CROP-METRIC-CLASSIFICATION-SCOPE COMBOS ----
  
  ## ALL CORN-RICH-CDL-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_CDL.*ALL.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-CDL-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_CDL.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-CDL-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_CDL.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-CDL-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_CDL.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-CDL-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_CDL.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-CDL-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_CDL.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-CDL-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_CDL.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-CDL-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_CDL.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-CDL-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_CDL.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-CDL-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_CDL.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-CDL-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_CDL.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-CDL-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_CDL.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-CDL-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_CDL.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-CDL-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_CDL.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-CDL-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_CDL.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-CDL-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_CDL.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-CDL-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_CDL.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-CDL-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_CDL.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-CDL-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_CDL.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-CDL-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_CDL.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-CDL-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_CDL.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-CDL-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_CDL.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-CDL-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_CDL.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-CDL-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_CDL.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-CDL-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_CDL.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  
  ## ALL CORN-RICH-RC-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_RC.*ALL.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-RC-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_RC.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-RC-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_RC.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-RC-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_RC.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-RC-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_RC.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-RC-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_RC.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-RC-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_RC.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-RC-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_RC.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-RC-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_RC.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-RC-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_RC.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-RC-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_RC.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-RC-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_RC.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-RC-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_RC.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-RC-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_RC.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-RC-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_RC.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-RC-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_RC.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-RC-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_RC.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-RC-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_RC.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-RC-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_RC.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-RC-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_RC.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-RC-ALL MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_RC.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-RC-ALL MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_RC.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-RC-ALL MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_RC.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-RC-ALL MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_RC.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-RC-ALL MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_RC.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  ## ALL CORN-RICH-CDL-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_CDL.*AG.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-CDL-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_CDL.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-CDL-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_CDL.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-CDL-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_CDL.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-CDL-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_CDL.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-CDL-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_CDL.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-CDL-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_CDL.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-CDL-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_CDL.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-CDL-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_CDL.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-CDL-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_CDL.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-CDL-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_CDL.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-CDL-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_CDL.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-CDL-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_CDL.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-CDL-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_CDL.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-CDL-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_CDL.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-CDL-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_CDL.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-CDL-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_CDL.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-CDL-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_CDL.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-CDL-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_CDL.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-CDL-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_CDL.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-CDL-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_CDL.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-CDL-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_CDL.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-CDL-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_CDL.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-CDL-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_CDL.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-CDL-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_CDL.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  
  ## ALL CORN-RICH-RC-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_RC.*AG.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-RC-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_RC.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-RC-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_RC.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-RC-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_RC.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-RC-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_RC.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-RC-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_RC.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-RC-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_RC.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-RC-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_RC.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-RC-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_RC.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-RC-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_RC.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-RC-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_RC.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-RC-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_RC.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-RC-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_RC.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-RC-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_RC.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-RC-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_RC.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-RC-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_RC.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-RC-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_RC.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-RC-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_RC.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-RC-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_RC.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-RC-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_RC.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-RC-AG MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_RC.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-RC-AG MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_RC.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-RC-AG MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_RC.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-RC-AG MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_RC.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-RC-AG MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_RC.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  # CROP-METRIC-SCOPE-BOUNDARY COMBOS ----
  
  ## ALL CORN-RICH-ALL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH.*BBOX.*ALL.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-ALL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH.*BBOX.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-ALL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH.*BBOX.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-ALL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH.*BBOX.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-ALL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH.*BBOX.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-ALL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI.*BBOX.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-ALL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI.*BBOX.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-ALL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI.*BBOX.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-ALL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI.*BBOX.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-ALL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI.*BBOX.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-ALL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-ALL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-ALL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-ALL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-ALL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI.*BBOX.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-ALL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_.*BBOX.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-ALL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_.*BBOX.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-ALL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_.*BBOX.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-ALL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_.*BBOX.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-ALL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_.*BBOX.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-ALL-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*ALL.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-ALL-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*ALL.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-ALL-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*ALL.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-ALL-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*ALL.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-ALL-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*ALL.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  
  ## ALL CORN-RICH-AG-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_.*BBOX.*AG.*corn")
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-AG-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RICH_.*BBOX.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-AG-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RICH_.*BBOX.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-AG-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RICH_.*BBOX.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-AG-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RICH_.*BBOX.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI-AG-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SDI_.*BBOX.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-AG-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SDI_.*BBOX.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-AG-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SDI_.*BBOX.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-AG-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SDI_.*BBOX.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-AG-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SDI_.*BBOX.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-SIDI-AG-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("SIDI_.*BBOX.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-AG-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("SIDI_.*BBOX.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-AG-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("SIDI_.*BBOX.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-AG-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("SIDI_.*BBOX.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-AG-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("SIDI_.*BBOX.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-AG-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("D_.*BBOX.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-AG-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("D_.*BBOX.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-AG-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("D_.*BBOX.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-AG-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("D_.*BBOX.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-AG-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("D_.*BBOX.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  ## ALL CORN-RPR-AG-BBOX MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*AG.*corn")) 
  model_results <- CORN_models
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-AG-BBOX MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*AG.*soy")) 
  model_results <- soy_models
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-AG-BBOX MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*AG.*wwheat")) 
  model_results <- wwheat_models
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-AG-BBOX MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*AG.*alfalfa")) 
  model_results <- alfalfa_models
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-AG-BBOX MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern = ("RPR_.*BBOX.*AG.*hay")) 
  model_results <- hay_models
  
  
  stackit(model_results)
  
  
  
  
  ## ALL CORN-RICH-ALL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_.*ALL.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-ALL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "RICH_.*ALL.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-ALL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "RICH_.*ALL.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-ALL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "RICH_.*ALL.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-ALL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "RICH_.*ALL.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI--ALL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "SDI_.*ALL.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-ALL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "SDI_.*ALL.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-ALL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "SDI_.*ALL.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-ALL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "SDI_.*ALL.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-ALL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "SDI_.*ALL.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SIDI-ALL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "SIDI_.*ALL.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-ALL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "SIDI_.*ALL.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-ALL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "SIDI_.*ALL.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-ALL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "SIDI_.*ALL.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-ALL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "SIDI_.*ALL.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-ALL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "D_.*ALL.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-ALL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "D_.*ALL.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-ALL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "D_.*ALL.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-ALL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "D_.*ALL.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-ALL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "D_.*ALL.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-RPR-ALL-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RPR_.*ALL.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-ALL-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "RPR_.*ALL.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-ALL-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "RPR_.*ALL.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-ALL-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "RPR_.*ALL.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-ALL-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "RPR_.*ALL.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  
  
  
  ## ALL CORN-RICH-AG-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RICH_.*AG.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RICH-AG-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "RICH_.*AG.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RICH-AG-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "RICH_.*AG.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RICH-AG-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "RICH_.*AG.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RICH-AG-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "RICH_.*AG.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SDI--AG-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "SDI_.*AG.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SDI-AG-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "SDI_.*AG.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SDI-AG-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "SDI_.*AG.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SDI-AG-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "SDI_.*AG.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SDI-AG-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "SDI_.*AG.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-SIDI-AG-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "SIDI_.*AG.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-SIDI-AG-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "SIDI_.*AG.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-SIDI-AG-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "SIDI_.*AG.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-SIDI-AG-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "SIDI_.*AG.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-SIDI-AG-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "SIDI_.*AG.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-D-AG-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "D_.*AG.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-D-AG-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "D_.*AG.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-D-AG-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "D_.*AG.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-D-AG-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "D_.*AG.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-D-AG-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "D_.*AG.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL CORN-RPR-AG-CNTY MODELS: Get model list ----
  
  
  CORN_models <-list.files("Output/", pattern =  "RPR_.*AG.*corn")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- CORN_models[!(CORN_models %in% bbox_datafiles)]
  
  
  
  stackit(model_results)
  
  
  ## ALL SOY-RPR-AG-CNTY MODELS: Get model list ----
  
  
  soy_models <-list.files("Output/", pattern =  "RPR_.*AG.*soy")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- soy_models[!(soy_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL WHEAT-RPR-AG-CNTY MODELS: Get model list ----
  
  
  wwheat_models <-list.files("Output/", pattern =  "RPR_.*AG.*wwheat")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- wwheat_models[!(wwheat_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL ALFALFA-RPR-AG-CNTY MODELS: Get model list ----
  
  
  alfalfa_models <-list.files("Output/", pattern =  "RPR_.*AG.*alfalfa")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- alfalfa_models[!(alfalfa_models %in% bbox_datafiles)]
  
  
  stackit(model_results)
  
  
  ## ALL HAY-RPR-AG-CNTY MODELS: Get model list ----
  
  
  hay_models <-list.files("Output/", pattern =  "RPR_.*AG.*hay")
  bbox_datafiles<-list.files("Output/", pattern = ("BBOX*"))
  model_results <- hay_models[!(hay_models %in% bbox_datafiles)]
  
  
  stackit(model_results)