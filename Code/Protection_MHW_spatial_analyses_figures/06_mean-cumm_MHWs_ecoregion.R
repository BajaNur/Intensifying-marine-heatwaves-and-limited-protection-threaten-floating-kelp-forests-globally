##Code for estimating the mean Cumm_MHWs per period/
##Creates rasters each scenario
##Estimaes mean and CI for each ecoregion
#Nur Arafeh Dalmau, nadalmau@stanford.edu

library (dplyr)
library (raster)
library(tidyr)
library(sp)
library(sf)
library(raster)

####Load data

All_OISST <- read.csv ("Data/MHW/OISST_median.csv")  %>% group_by (x,y) %>% 
  summarize(Ob = mean(c_across(20:39), na.rm = TRUE)) 

####Scenario IPPC

###SPP126

All_126 <- read.csv ("Data/MHW/ssp126_median.csv") %>% group_by (x,y) %>% 
  summarize(Sho_126 = mean(c_across(40:59), na.rm = TRUE),
            Mid_126 = mean(c_across(60:79), na.rm = TRUE),
            Long_126 = mean(c_across(100:119), na.rm = TRUE))

###SPP245

All_245 <- read.csv ("Data/MHW/ssp245_median.csv") %>% group_by (x,y) %>% 
  summarize(Sho_245 = mean(c_across(40:59), na.rm = TRUE),
            Mid_245 = mean(c_across(60:79), na.rm = TRUE),
            Long_245 = mean(c_across(100:119), na.rm = TRUE))

###Scenario IPPC 585

All_585 <- read.csv ("Data/MHW/ssp585_median.csv") %>% group_by (x,y) %>% 
  summarize(Sho_585 = mean(c_across(40:59), na.rm = TRUE),
            Mid_585 = mean(c_across(60:79), na.rm = TRUE),
            Long_585 = mean(c_across(100:119), na.rm = TRUE))

Kelp_all <- merge(merge(All_126, All_245, by = c("y", "x"), all = TRUE), 
                  All_585, by = c("y", "x"), all = TRUE)

write.csv(Kelp_all, "Data/Outputs/Mean_MHWs.csv")

# Merge dataframes

data_frames <- list(All_OISST, All_126, All_245, All_585)

Kelp_all <- Reduce(function(x, y) merge(x, y, by = c("y", "x"), all = TRUE), data_frames)



####### Now create rasters

unique_coords_df <- unique(Kelp_all[, c("x", "y")])

# Loop through the columns
for (col_name in colnames(Kelp_all)[3:ncol(Kelp_all)]) {
  center_x <- mean(unique_coords_df$x)
  center_y <- mean(unique_coords_df$y)
  
  # Create Points 
  spdf <- data.frame(
    x = Kelp_all$x,
    y = Kelp_all$y,
    value = Kelp_all[[col_name]]
  )
  
  coordinates(spdf) <- c("x", "y")
  proj4string(spdf) <- CRS("+proj=longlat +datum=WGS84")
  
  # Create a raster
  r <- rasterFromXYZ(spdf, crs = CRS("+proj=longlat +datum=WGS84"))
  
  # Save raster to file
  output_path <- paste0("raster/", col_name, "_raster.tif")
  writeRaster(r, output_path, format = "GTiff", overwrite = TRUE)
  
  cat("Raster", col_name, "successfull.\n")
}


###########Estimate mean and CI for each ecoregion and time

polygons <- st_read("Data/Spalding Ecoregions/meow_ecos.shp") ##Donwload at: https://databasin.org/datasets/3b6b12e7bcca419990c9081c0af254a2/

# Open folder that contains raster
raster_folder <- "raster/"

# Get all raster files
raster_files <- list.files(path = raster_folder, pattern = "\\.tif$", full.names = TRUE)

# Store results in an empty list
result_list <- list()

# Convert shapefile to sf object
polygons_sf <- st_as_sf(polygons)

# Loop through each raster file
for (file in raster_files) {
  raster_data <- raster(file)
  
  # Intersection
  intersection <- raster::intersect(raster_data, polygons_sf)
  
  # Extract values
  extract_values <- raster::extract(intersection, polygons_sf)
  
  # Filter no values
  non_empty_indices <- sapply(extract_values, function(x) any(!is.na(x)))
  
  # Calculate mean values 
  mean_values <- sapply(extract_values[non_empty_indices], mean, na.rm = TRUE)
  
  # Calculate 95% confidence interval 
  conf_intervals <- sapply(extract_values[non_empty_indices], function(x) {
    if (length(x) > 1) {
      t.test(x, conf.level = 0.95)$conf.int
    } else {
      c(NA, NA)
    }
  })
  
  # Get the raster name
  raster_name <- tools::file_path_sans_ext(basename(file))
  
  # Combine results 
  result_df <- data.frame(
    Ecoregion = polygons$ECOREGION[non_empty_indices],
    mean_value = mean_values,
    lower_ci = conf_intervals[1, ],
    upper_ci = conf_intervals[2, ]
  )
  
  # Remove rows with NA 
  result_df <- na.omit(result_df)
  
  # Rename with the raster name
  names(result_df)[2] <- raster_name
  
  # Rename lower_ci and upper_ci
  names(result_df)[3] <- paste0(raster_name, "_lower_ci")
  names(result_df)[4] <- paste0(raster_name, "_upper_ci")
  
  # Store the result data frame 
  result_list[[raster_name]] <- result_df
}

# Combine all data frames 
final_df <- Reduce(function(x, y) merge(x, y, by = "Ecoregion", all = TRUE), result_list) 


write.csv(final_df,"Outputs/Ecoregions_mean_MHWs.csv")

