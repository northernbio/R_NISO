library(terra)
setwd("~/FERIT_2/NISO/R_NISO")

# Define the list of bird codes
bird_code_list <- c("ALFL", "AMRE")  # Add all required bird codes here
esri_grid_dir <- ("D:/FERIT_1/NISO/ESRI_Bird_grids")

get_raster_brick_future <- function(directory, bird_codes, type, periods) {
  raster_list <- list()
  
  for (period in periods) {
    # Construct the pattern to match files: type_code_period
    file_pattern <- paste0(type, "_(", paste(bird_codes, collapse = "|"), ")_", period)
    
    # List directories matching the ESRI grid pattern
    esri_grids <- list.files(directory, pattern = file_pattern, full.names = TRUE)
    
    # Ensure these are directories (ESRI grids are stored as folders)
    esri_grids <- esri_grids[file.info(esri_grids)$isdir]
    
    # Load each ESRI grid using terra::rast and add to list
    raster_list <- c(raster_list, lapply(esri_grids, rast))
  }
  
  # Combine all rasters into a SpatRaster using terra::sprc and terra::merge
  raster_brick <- sprc(raster_list)
  raster_brick <- merge(raster_brick)
  
  return(raster_brick)
}

# Create the prob_brick_future where type is "prob" and period is 1 to 4
#result <- get_raster_brick_future(directory, bird_codes, type, periods)

prob_brick_future <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 1:4)
print(prob_brick_future)

prob_brick_current <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 0)
print(prob_brick_current)

# Print details of the resulting RasterBrick
print(prob_brick_current)
print(prob_brick_future)

# Plot the second layer of prob_brick_current
plot(prob_brick_current[[2]], main = paste("Layer 2:", names(prob_brick_current)[2]))

# Plot the first layer of prob_brick_future
plot(prob_brick_future[[1]], main = paste("Layer 1:", names(prob_brick_future)[1]))


