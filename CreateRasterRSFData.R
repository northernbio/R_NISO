library(terra)
setwd("~/FERIT_2/NISO/R_NISO")

# Define the list of bird codes
bird_code_list <- c("ALFL", "AMRE", "CSWA", "COYE")  # Add all required bird codes here
bird_code_list <- c("ALFL", "AMRE", "BBWA", "BHVI", "BLBW", "BRCR", "COYE",  "CSWA", "DEJU", "GCKI", "HETH")  # Add all required bird codes here
bird_code_list <- c("ALFL")

esri_grid_dir <- ("D:/FERIT_1/NISO/ESRI_Bird_grids")
esri_grid_dir <- ("D:/FERIT_1/NISO/data_Nov_19_2024/birds/grids")
esri_habgrid_dir <- ("D:/FERIT_1/NISO/data_Nov_19_2024/birds/habitat_grids")

get_raster_brick_future <- function(directory, bird_codes, type, periods) {
  # Initialize an empty list to store raster layers by species and period
  raster_list <- list()
  
  # Loop through each bird code
  for (bird_code in bird_codes) {
    # Loop through each period
    for (period in periods) {
      # Construct the pattern to match files: type_code_period
      if (type == "habitat") {
        file_pattern <- paste0(bird_code, "_", period)
      } else {
        file_pattern <- paste0(type, "_", bird_code, "_", period)
      }
      
      # List directories matching the ESRI grid pattern
      esri_grids <- list.files(directory, pattern = file_pattern, full.names = TRUE)
      
      # Ensure these are directories (ESRI grids are stored as folders)
      esri_grids <- esri_grids[file.info(esri_grids)$isdir]
      
      # Load each ESRI grid using terra::rast and add to the raster list
      if (length(esri_grids) > 0) {
        for (grid in esri_grids) {
          raster <- rast(grid)
          # Add raster to the list with a unique name for species and period
          layer_name <- paste(bird_code, period, sep = "_")
          raster_list[[layer_name]] <- raster
        }
      } else {
        warning(paste("No matching rasters found for bird code:", bird_code, "and period:", period))
      }
    }
  }
  
  # Check if all raster extents match the extent of the first raster in the list
  if (length(raster_list) > 0) {
    first_extent <- ext(raster_list[[1]])
    extent_matches <- sapply(raster_list, function(r) ext(r) == first_extent)
    
    # Print results of extent comparison
    print(data.frame(
      Raster = names(raster_list),
      ExtentMatches = extent_matches
    ))
  } else {
    warning("No rasters were loaded, so extent comparison was not performed.")
  }
  
  # Combine all rasters into a single raster brick
  raster_brick <- rast(raster_list)
  
  return(raster_brick)
}

hab_code_list_test5 <- c("locage", "loccc", "locht", "locpcthwd", "locpctyng")
hab_code_list_test6 <- c("landcwed", "landpctmat")

hab_brick5 <- get_raster_brick_future(esri_grid_dir, hab_code_list_test5, "habitat", 0)
hab_brick6 <- get_raster_brick_future(esri_grid_dir, hab_code_list_test6, "habitat", 0)

reference_raster <- hab_brick5[[1]]
common_extent <- ext(reference_raster)
common_res <- res(reference_raster)

# Align all rasters to the common extent and resolution
align_to_reference <- function(brick, reference_extent, reference_res) {
  aligned_list <- lapply(brick, function(r) {
    r <- extend(r, reference_extent)
    resample(r, reference_raster)
  })
  rast(aligned_list)
}

hab_brick2_aligned <- align_to_reference(hab_brick2, common_extent, common_res)
hab_brick3_aligned <- align_to_reference(hab_brick3, common_extent, common_res)
hab_brick4_aligned <- align_to_reference(hab_brick4, common_extent, common_res)

hab_brick5_aligned <- align_to_reference(hab_brick5, common_extent, common_res)
hab_brick6_aligned <- align_to_reference(hab_brick6, common_extent, common_res)

# Combine all aligned raster bricks into one
final_brick <- c(hab_brick2_aligned, hab_brick3_aligned, hab_brick4_aligned)

final_brick <- c(hab_brick5_aligned, hab_brick6_aligned)

# Final output
final_brick

save(final_brick, file = "final_brick.RData")

# To confirm it saved correctly, you can later load it with:
# load("final_brick.RData")














# D:\FERIT_1\NISO\data_Nov_19_2024\birds\grids
# get_raster_brick_future <- function(directory, bird_codes, type, periods) {
#   # Initialize an empty list to store raster layers by species and period
# 
#    raster_list <- list()
#   
#   # Loop through each bird code
#   for (bird_code in bird_codes) {
#     # Loop through each period
#     for (period in periods) {
#       # Construct the pattern to match files: type_code_period
# 
#       if (type == "habitat") {
#         file_pattern <- paste0(bird_code, "_", period)
#       } else {
#         file_pattern <- paste0(type, "_", bird_code, "_", period)
#       }
# 
#       # List directories matching the ESRI grid pattern
#       esri_grids <- list.files(directory, pattern = file_pattern, full.names = TRUE)
#       
#       # Ensure these are directories (ESRI grids are stored as folders)
#       esri_grids <- esri_grids[file.info(esri_grids)$isdir]
#       
#       # Load each ESRI grid using terra::rast and add to the raster list
#       if (length(esri_grids) > 0) {
#         for (grid in esri_grids) {
#           raster <- rast(grid)
#           # Add raster to the list with a unique name for species and period
#           layer_name <- paste(bird_code, period, sep = "_")
#           raster_list[[layer_name]] <- raster
#         }
#       } else {
#         warning(paste("No matching rasters found for bird code:", bird_code, "and period:", period))
#       }
#     }
#   }
#   
#   # Combine all rasters into a single raster brick
#   raster_brick <- rast(raster_list)
#   
#   return(raster_brick)
# }

bird_code_list_test <- c("age", "ht", "cc",  "pcthwd", "pctmat", "pctyng", "locage", "loccc", 
  "locht", "locpcthwd", "locpctyng",  "landcwed", "landpctmat" )


bird_code_list_test <- c("age", "ht", "cc", "cwed", "pcthwd", "pctmat", "pctyng", "locage", "loccc", 
 "locht", "locpcthwd", "locpctyng",  "landcwed", "landpctmat" )




hab_brick <- get_raster_brick_future(esri_grid_dir, bird_code_list_test, "habitat", 0)
print(hab_brick)



hab_brick <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 0)
print(hab_brick)



prob_brick_future <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 1:4)
print(prob_brick_future)

# Define the specific layer name for ALFL in period 2
layer_name <- "ALFL_2"

# Check if the layer exists in the raster brick
if (layer_name %in% names(prob_brick_future)) {
  # Plot the raster for ALFL in period 2
  plot(prob_brick_future[[layer_name]], 
       main = paste("Raster for", layer_name), 
       col = terrain.colors(100))  # Adjust color palette as needed
} else {
  warning(paste("Layer", layer_name, "not found in the raster brick."))
}

prob_brick_current <- get_raster_brick_future(esri_grid_dir, bird_code_list, "prob", 0)
print(prob_brick_current)
str(prob_brick_current)

occ_brick_current <- get_raster_brick_future(esri_grid_dir, bird_code_list, "occ", 0)


# Print details of the resulting RasterBrick
print(prob_brick_current)
print(prob_brick_future)

names(prob_brick_current)
plot(prob_brick_current[["ALFL_0"]], main = "Probability: ALFL (Current)")

names(prob_brick_future)
plot(prob_brick_future[["ALFL_2"]], main = "Probability: ALFL (Future)")



# Save the raster brick as an RDS file
saveRDS(prob_brick_current, "prob_brick_current.rds")

# To load it back later
prob_brick_current <- readRDS("prob_brick_current.rds")



# Save the raster brick to a file
writeRaster(prob_brick_future, filename = "prob_brick_future.tif", filetype = "GTiff", overwrite = TRUE)
writeRaster(prob_brick_current, filename = "prob_brick_current.tif", filetype = "GTiff", overwrite = TRUE)


# Load the .tif files
prob_brick_future <- brick("prob_brick_future.tif")
prob_brick_current <- brick("prob_brick_current.tif")

#no, save as rds