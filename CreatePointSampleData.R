
library(sf)    # For reading geospatial data
#library(raster)
library (dplyr)
library(spsurvey)
setwd("~/FERIT_2/NISO/R_NISO")

#######################
# code to create the point data file, combined_species_data

# Define the path to the ESRI geodatabase
gdb_path <- "D:/FERIT_1/NISO/Bird_Counts.gdb"

# Check if the geodatabase exists
if (!file.exists(gdb_path)) {
  stop("The specified geodatabase does not exist.")
}

# List the available layers in the geodatabase
gdb_layers <- st_layers(gdb_path)

# Print the available layers
print(gdb_layers)

pointCount_layer <- st_read(gdb_path, layer = gdb_layers$name[2])  # NatureCounts_3, Pointcounts

# Check the current CRS of the layer
print(st_crs(pointCount_layer))

# Transform to CRS 26914 (NAD83 / UTM zone 14N)
pointCount_layer_utm <- st_transform(pointCount_layer, crs = 26914)

# Check the new CRS of the transformed layer
print(st_crs(pointCount_layer_utm))

# Optionally, save the transformed layer to a new file
# st_write(pointCount_layer_utm, "path/to/save/pointCount_layer_utm.shp")

# Plot the transformed layer
#plot(st_geometry(pointCount_layer_utm), main = "Point Count Layer in CRS 26914")


# Filter pointCount_layer_utm for rows where SpeciesCode matches bird_code_list
bird_data <- pointCount_layer_utm %>%
  filter(SpeciesCode %in% bird_code_list) %>%
  select(SpeciesCode, CommonName, YearCollected,SurveyAreaIdentifier)
  #select(SpeciesCode, CommonName, YearCollected,SurveyAreaIdentifier)
bird_data$count <- 1

# Extract unique SurveyAreaIdentifier and keep geometry
pointCount_locations <- pointCount_layer_utm %>%
  select(SurveyAreaIdentifier) %>%  # Keep only the SurveyAreaIdentifier field
  distinct()                        # Ensure unique values, retaining geometry

pointCount_locations$count <- 0

## View the result
print(pointCount_locations)
# Plot the unique locations
plot(st_geometry(pointCount_locations), main = "Unique Survey Area Locations")
# View the filtered data
print(bird_data)
# Generate a color palette for unique SpeciesCodes
species_colors <- rainbow(length(unique(bird_data$SpeciesCode)))
# Assign colors to each SpeciesCode
plot_colors <- species_colors[as.numeric(as.factor(bird_data$SpeciesCode))]
# Plot the data with different colors for each SpeciesCode
plot(
  st_geometry(bird_data), 
  col = plot_colors, 
  main = "Filtered Points by SpeciesCode",
  pch = 16
)
# Add a legend to the plot
legend(
  "topright", 
  legend = unique(bird_data$SpeciesCode), 
  col = species_colors, 
  pch = 16, 
  title = "SpeciesCode"
)
###############################
# filter data and select fields
# Initialize an empty list to store the resulting data frames
species_dataframes <- list()

# Loop over each species in bird_code_list
for (species in bird_code_list) {
  # Filter pointCount_layer_utm for the current SpeciesCode
  species_data <- pointCount_layer_utm %>%
    filter(SpeciesCode == species) %>%
    select(SurveyAreaIdentifier, SpeciesCode)
    #select(SurveyAreaIdentifier, SpeciesCode, CommonName, YearCollected)
  
  # Spatial join with pointCount_locations
  merged_data <- st_join(pointCount_locations, species_data, by = "SurveyAreaIdentifier")
  
  # Consolidate SurveyAreaIdentifier.x and SurveyAreaIdentifier.y
  merged_data <- merged_data %>%
    mutate(SurveyAreaIdentifier = coalesce(SurveyAreaIdentifier.x, SurveyAreaIdentifier.y)) %>%
    select(-SurveyAreaIdentifier.x, -SurveyAreaIdentifier.y)  # Drop redundant columns
  
  # Filter to keep records where count = 1 or where SpeciesCode is missing
  merged_data <- merged_data %>%
    mutate(count = ifelse(is.na(SpeciesCode), 0, 1)) %>%
    group_by(SurveyAreaIdentifier) %>%
    filter(count == 1 | is.na(SpeciesCode)) %>%
    ungroup()
  
  # Assign missing SpeciesCode to the current species
  merged_data <- merged_data %>%
    mutate(SpeciesCode = ifelse(is.na(SpeciesCode), species, SpeciesCode))
  
  # Store the resulting data frame in the list
  species_dataframes[[species]] <- merged_data
}

# View the first dataframe for ALFL for verification
print(species_dataframes[["ALFL"]])
# Combine all dataframes in the list into one sf object, includes all spp
combined_species_data <- bind_rows(species_dataframes)

# View the combined data
print(combined_species_data)

# Plot the combined data by SpeciesCode
plot(st_geometry(combined_species_data), col = as.factor(combined_species_data$SpeciesCode), main = "Combined Data by SpeciesCode")
legend("topright", legend = unique(combined_species_data$SpeciesCode), col = unique(as.factor(combined_species_data$SpeciesCode)), pch = 16)


species_count_summary <- combined_species_data %>%
  group_by(SpeciesCode, count) %>%
  summarise(RecordCount = n(), .groups = 'drop') %>%
  arrange(SpeciesCode, count)

# View and save the point data file
print(species_count_summary)
setwd("~/FERIT_2/NISO/R_NISO")
saveRDS(combined_species_data, file = "combined_species_data.rds")
#combined_species_data <- readRDS("combined_species_data.rds")


#########################
# GRTS sample selection
species_data_list <- list()
for (species in bird_code_list) {

#species <- "ALFL" #for testing only
# Filter combined data for the current SpeciesCode
selected_spp_sf <- combined_species_data %>%
  filter(SpeciesCode == species) %>%
  mutate(id = row_number())  # Add an ID column for reference
# Add a new variable "status" based on the value of "count"
selected_spp_sf <- selected_spp_sf %>%
  mutate(status = ifelse(count == 0, "avail", "used"))
nrow(selected_spp_sf)

# For selected spp, filter plot records with max count, can include multiple years
selected_spp_maxCnt <- selected_spp_sf %>% group_by(SurveyAreaIdentifier) %>% filter(count == max(count)) %>% ungroup()
nrow(selected_spp_maxCnt)
# Now select unique values for plot (SurveyAreaIdentifer -- USAI)

selected_spp_USAI <- selected_spp_maxCnt %>%
  distinct(SurveyAreaIdentifier, .keep_all = TRUE)
nrow(selected_spp_USAI)

# Calculate 25% of records for each status
avail_count <- nrow(selected_spp_USAI[selected_spp_USAI$status == "avail", ])
used_count <- nrow(selected_spp_USAI[selected_spp_USAI$status == "used", ])
avail_25 <- round(0.25 * avail_count)
used_25 <- round(0.25 * used_count)
caty_n <- c(avail = avail_25, used = used_25)
n_base_val = avail_25 + used_25

#GRTS sample
grts_result_train <- grts(selected_spp_USAI, n_base = n_base_val, caty_var = "status", caty_n = caty_n) #unequal inclusion probability

plot(
  grts_result_train,
  formula = siteuse ~ status,
  selected_spp_USAI,
  key.width = lcm(3)
)

#plot(grts_result, key.width = lcm(3))
train_points_grts <- sp_rbind(grts_result_train)
nrow(train_points_grts)
plot(train_points_grts[9]) # used and avail

species_count_summary <- train_points_grts %>%
  group_by(SpeciesCode, count) %>%
  summarise(RecordCount = n(), .groups = 'drop') %>%
  arrange(SpeciesCode, count)

# View the results
print(species_count_summary)

################
# Now select the test data
# Filter records where SurveyAreaIdentifier is not in grts_result_train
test_points_grts <- selected_spp_USAI %>%
  filter(!(SurveyAreaIdentifier %in% train_points_grts$SurveyAreaIdentifier))
nrow(test_points_grts)

species_data_list[[species]] <- list(
  train_points_grts = train_points_grts,
  test_points_grts = test_points_grts
)

}
#########################################

# Initialize an empty list to hold the dataframes with Species column
combined_list <- lapply(names(species_data_list), function(species) {
  # Add a column to both train and test dataframes
  train <- species_data_list[[species]]$train_points_grts %>%
    mutate(Species = species, DataType = "Train")
  
  test <- species_data_list[[species]]$test_points_grts %>%
    mutate(Species = species, DataType = "Test")
  
  # Combine train and test for the current species
  bind_rows(train, test)
})

# Combine all species data into a single dataframe
BirdPointSamples <- bind_rows(combined_list)

#####Save the file #########
setwd("~/FERIT_2/NISO/R_NISO")
saveRDS(BirdPointSamples, file = "BirdPointSamples.rds")
#BirdPointSamples <- readRDS("BirdPointSamples.rds")


