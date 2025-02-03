# Step OBS1: Preparing means from raw data to add back to the ensembled anomalies, and regrid
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# October 2023

# NOTE: this step would need to be modified by the user, depending on their context
# Data used here were 0.5-degree data downloaded from Copernicus


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")
	
	
# Folders, etc. ----------------------------------------------------------------

  input_folder <- "/Volumes/BackPack/OISST"
  w <- detectCores()-4  # Number of workers (parallel processes) to use
  base_rast_folder <- make_folder("Base_raster")
  cell_res = 0.25 # Resolution of base-grid raster
  
  
  # Make base-grid raster ------------------------------------------------------
  
  
  base_rast <- paste0(base_rast_folder, "/base_rast.nc")
  r <- rast(resolution = cell_res)
  r[] <- 1
  mask2netCDF4(r, pth = base_rast_folder, 
               ncName = basename(base_rast), 
               dname = "dummy", 
               dlname = "dummy")
    
# Get the data and get the means -----------------------------------------------
  
	 files <- dir(input_folder, full.names = TRUE)
	 get_mean <- function(f) {
	   # First make a base raster with desired resolution
	   tmp_file <- paste0(dirname(f),"/tmp_", basename(f))
	   cdo_code <- paste0("cdo -L -timmean ", f, " ", tmp_file)
	    system(cdo_code)
	   tmp_file1 <- paste0(dirname(f),"/tmp1_", basename(f))
	   cdo_code <- paste0("cdo -L -remapnn,", base_rast, " ", tmp_file, " ", tmp_file1)
	    system(cdo_code)
    out_file <- paste0(dirname(f),"/mean_", basename(f))
    cdo_code <- paste0("cdo -L -setctomiss,0 ", tmp_file1, " ", out_file)
      system(cdo_code)
    system(paste0("rm ", dirname(f), "/tmp*"))
	  }
  walk(files, get_mean)
  system(paste0("rm -r ", base_rast_folder))
  
# Goto OBS_2_Prepare_mask_from_observations.R

