library(terra)
library(sf)
# Load point file

filtered_points_Train_vect <- vect(filtered_points_Train)
# Extract values from each layer in final_brick for the points
extracted_values <- extract(final_brick, filtered_points_Train_vect)
#clean up data
filtered_points_Train_combined <- cbind(as.data.frame(filtered_points_Train_vect), extracted_values)
filtered_points_Train_combined <- filtered_points_Train_combined[, !duplicated(colnames(filtered_points_Train_combined))]
filtered_points_Train_combined <- na.omit(filtered_points_Train_combined)
filtered_points_Train_combined[is.na(filtered_points_Train_combined)] <- 0

# Optionally, save the updated dataset
write.csv(filtered_points_Train_combined, "filtered_points_Train_with_values_no_na.csv", row.names = FALSE)

# Loop through each species in bird_code_list
bird_code_list <- c("ALFL", "AMRE", "BAWW", "BBWA", "BHVI", "BLBW", "BRCR", "COYE",  "CSWA", "DEJU", "GCKI", "HETH")  # Add all required bird codes here
#bird_code_list <- c("ALFL", "AMRE", "BBWA", "BHVI", "BLBW", "BRCR", "COYE",  "CSWA", "DEJU", "GCKI", "HETH")  # Add all required bird codes here

# Initialize lists to store results
results_Train_list <- list()
results_Test_list <- list()
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
  
  filtered_points_Train_vect <- vect(filtered_points_Train)
  nrow(filtered_points_Train_vect)
  # Extract values from each layer in final_brick for the points
  extracted_values_Train <- extract(final_brick, filtered_points_Train_vect)
  nrow(extracted_values_Train)
  #clean up data
  filtered_points_Train_merged <- cbind(filtered_points_Train_vect, extracted_values_Train[, -1])
  nrow(filtered_points_Train_merged)
  
  filtered_points_Test_vect <- vect(filtered_points_Test)
  nrow(filtered_points_Test_vect)
  # Extract values from each layer in final_brick for the points
  extracted_values_Test <- extract(final_brick, filtered_points_Test_vect)
  nrow(extracted_values_Test)
  #clean up data
  filtered_points_Test_merged <- cbind(filtered_points_Test_vect, extracted_values_Test[, -1])
  nrow(filtered_points_Test_merged)
  
  
  #filtered_points_Train_combined <- cbind(as.data.frame(filtered_points_Train_vect), extracted_values)
  
  #filtered_points_Train_combined <- cbind(as.data.frame(filtered_points_Train_vect), extracted_values_Train)
  filtered_points_Train_combined <- cbind(
    as.data.frame(filtered_points_Train_vect, row.names = NULL), 
    extracted_values_Train, 
    species = species
  )
    filtered_points_Train_combined <- filtered_points_Train_combined[, !duplicated(colnames(filtered_points_Train_combined))]
  filtered_points_Train_combined <- na.omit(filtered_points_Train_combined)
  filtered_points_Train_combined[is.na(filtered_points_Train_combined)] <- 0
  
  #filtered_points_Test_vect <- vect(filtered_points_Test)
  filtered_points_Test_merged <- cbind(filtered_points_Test_vect, extracted_values_Test[, -1])
  # Extract values from each layer in final_brick for the points
  extracted_values <- extract(final_brick, filtered_points_Test_vect)
  #clean up data
  #filtered_points_Test_combined <- cbind(as.data.frame(filtered_points_Test_vect), extracted_values)
  filtered_points_Test_combined <- cbind(
    as.data.frame(filtered_points_Test_vect, row.names = NULL), 
    extracted_values_Test, 
    species = species
  )
   filtered_points_Test_combined <- filtered_points_Test_combined[, !duplicated(colnames(filtered_points_Test_combined))]
  filtered_points_Test_combined <- na.omit(filtered_points_Test_combined)
  filtered_points_Test_combined[is.na(filtered_points_Test_combined)] <- 0
  
  # Store results in lists by species
  results_Test_list[[species]] <- filtered_points_Test_combined
  results_Train_list[[species]] <- filtered_points_Train_combined
  
}

combined_results_Train <- do.call(rbind, results_Train_list)
rownames(combined_results_Train) <- NULL

combined_results_Test <- do.call(rbind, results_Test_list)
rownames(combined_results_Test) <- NULL


#####Save the file #########
setwd("~/FERIT_2/NISO/R_NISO")
saveRDS(combined_results_Train, file = "combined_results_Train.rds")
#combined_results_Train <- readRDS("combined_results_Train.rds")

saveRDS(combined_results_Test, file = "combined_results_Test.rds")
#combined_results_Test <- readRDS("combined_results_Test.rds")









locage <- results_Test_list[["locage"]] 

names(results_Test_list)
