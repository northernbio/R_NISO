
#install.packages("raster")
library(raster)
library (dplyr)
setwd("~/FERIT_2/NISO/R_NISO")

# Define the list of bird codes
bird_code_list <- c("ALFL", "AMRE")  # Add all required bird codes here
esri_grid_dir <- ("D:/FERIT_1/NISO/ESRI_Bird_grids")

# Function to filter and load ESRI grids based on type and period range
get_raster_brick_future <- function(directory, bird_codes, type, periods) {
  raster_list <- list()
  
  for (period in periods) {
    # Construct the pattern to match files: type_code_period
    file_pattern <- paste0(type, "_(", paste(bird_codes, collapse = "|"), ")_", period)
    
    # List directories matching the ESRI grid pattern
    esri_grids <- list.files(directory, pattern = file_pattern, full.names = TRUE)
    
    # Ensure these are directories and not files (ESRI grids are folders)
    esri_grids <- esri_grids[file.info(esri_grids)$isdir]
    
    # Load each grid and add to list
    raster_list <- c(raster_list, lapply(esri_grids, raster))
  }
  
  # Combine into a RasterBrick
  raster_brick <- brick(stack(raster_list))
  
  return(raster_brick)
}

# Create the prob_brick_future where type is "prob" and period is 1 to 4
prob_brick_future <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 1:4)
print(prob_brick_future)

prob_brick_current <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 0)
print(prob_brick_current)


# Print details of the resulting RasterBrick
print(prob_brick)
print(prob_brick_future)

# Plot the second layer in the prob_brick
plot(prob_brick[[2]], main = paste("Layer 2:", names(prob_brick)[2]))
plot(prob_brick[[1]], main = paste("Layer 1:", names(prob_brick)[1]))


# Load necessary libraries
library(sf)    # For reading geospatial data

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


################ View the result
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
# Combine all dataframes in the list into one sf object
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

# View the results
print(species_count_summary)

#########################
# GRTS sample selection

species <- "ALFL" #for testing only
# Filter combined data for the current SpeciesCode
species_data_sf <- combined_data %>%
  filter(SpeciesCode == species) %>%
  mutate(id = row_number())  # Add an ID column for reference
# Add a new variable "status" based on the value of "count"
species_data_sf <- species_data_sf %>%
  mutate(status = ifelse(count == 0, "avail", "used"))

caty_n <- c(avail = 250, used = 250)
strata_n <- c(avail = 250, used = 250)
grts_result1 <- grts(species_data_sf, n_base = 500, caty_var = "status")
grts_result2 <- grts(species_data_sf, n_base = 500, caty_var = "status", caty_n = caty_n) #unequal inclusion probability
grts_result3 <- grts(species_data_sf, n_base = strata_n, stratum_var = "status") #equal inclusion probability, stratified by status

plot(
  grts_result2,
  formula = siteuse ~ status,
  species_data_sf,
  key.width = lcm(3)
)

plot(grts_result2, key.width = lcm(3))
plot(grts_result3, key.width = lcm(3))

combined_grts <- sp_rbind(grts_result2)
plot(combined_grts[9]) # used and avail

species_count_summary <- combined_grts %>%
  group_by(SpeciesCode, count) %>%
  summarise(RecordCount = n(), .groups = 'drop') %>%
  arrange(SpeciesCode, count)

# View the results
print(species_count_summary)


#########################################

