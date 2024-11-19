

# Load necessary libraries
library(dplyr)
library(terra)
library(sf)
library(Epi)

# Assuming these variables are already loaded:
# bird_code_list: List of species codes
# BirdPointSamples: Dataframe containing bird data with SpeciesCode and DataType
# prob_brick_current: RasterBrick containing probability layers

# Initialize lists to store results
results_Train_list <- list()
results_Test_list <- list()

# Loop through each species in bird_code_list
for (species in bird_code_list) {
  #species <- "ALFL" #for testing only
  #species <- "COYE" #for testing only
  
  # Filter BirdPointSamples for the current species and DataType = Train
  filtered_points_Train <- BirdPointSamples %>%
    filter(SpeciesCode == species, DataType == "Train") %>%
    select(status)
  
  # Filter BirdPointSamples for the current species and DataType = Test
  filtered_points_Test <- BirdPointSamples %>%
    filter(SpeciesCode == species, DataType == "Test") %>%
    select(status)
  
  # Create the probability layer for the current species
  names (prob_brick_current)
  prob_layer <- prob_brick_current[[paste0(species, "_0")]]
  occ_layer <- occ_brick_current[[paste0(species, "_0")]]

  # Convert filtered_points_Test to SpatVector if needed
  filtered_points_vect <- vect(filtered_points_Test)
  
  # Extract raster values for Test and Train data
  raster_values_Test <- extract(prob_layer, filtered_points_Test)
  raster_values_Train <- extract(prob_layer, filtered_points_Train)
  raster_Occ_Test <- extract(occ_layer, filtered_points_Test)
  raster_Occ_Train <- extract(occ_layer, filtered_points_Train)
  raster_Occ_Test[[paste0(species, "_Occ")]] <- raster_Occ_Test[[paste0(species, "_0")]]
  raster_Occ_Train[[paste0(species, "_Occ")]] <- raster_Occ_Train[[paste0(species, "_0")]]
  
  result_Test <- cbind(filtered_points_Test, raster_values_Test, raster_Occ_Test) %>%
    select(status,  paste0(species, "_0"), paste0(species,"_Occ")) %>% na.omit()

  result_Train <- cbind(filtered_points_Train, raster_values_Train,raster_Occ_Train) %>%
    select(status, paste0(species, "_0"), paste0(species,"_Occ")) %>%
    na.omit()

  # Store results in lists by species
  results_Test_list[[species]] <- result_Test
  results_Train_list[[species]] <- result_Train
  
  result_Test <- result_Test %>%
    mutate(status_val = ifelse(status == "used", 1, 0))
  
  result_Train <- result_Train %>%
    mutate(status_val = ifelse(status == "used", 1, 0))
  
  dfROCTrain <- ROC(result_Train[[paste0(species, "_0")]], result_Train$status_val, plot ="ROC")
  title (main =species)
  dfROCTest <- ROC(result_Test[[paste0(species, "_0")]], result_Test$status_val, plot ="ROC")
  title (main =species)

  library(PresenceAbsence)
  # MaxSens+Spec maximizes (sensitivity+specificity)/2
  #cp <- optimal.thresholds(dfPredict, threshold = 101, opt.methods=3)
  #cutpointMaxSS <- cp$predbrt
  
  }

# Results are stored as lists of dataframes
ALFL <- results_Test_list[["ALFL"]] 

names(results_Test_list)
