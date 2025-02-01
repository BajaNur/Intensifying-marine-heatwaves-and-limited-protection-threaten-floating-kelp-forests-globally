# Step OBS2: Preparing masks from the means from raw data, if needed
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# October 2023

# NOTE: this step would need to be modified by the user, depending on their context


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")
	
	
# Folders, etc. ----------------------------------------------------------------

  input_folder <- "/Volumes/BackPack/OISST"
  w <- detectCores()-4  # Number of workers (parallel processes) to use

	 
# Make a mask ------------------------------------------------------------------
  
  files <- dir(input_folder, pattern = "^mean_", full.names = TRUE)
  make_mask <- function(f) {
    v <- nc_open(f) %>% 
      .[["var"]] %>% 
      names() %>% 
      .[2] # The variable name
    out_file <- paste0(dirname(f), "/mask_", v, ".nc")
    cdo_code <- paste0("cdo -expr,'", v," = ((", v, ">-20000)) ? 1.0 : ", v, "/0.0' ", f, " ", out_file)
    system(cdo_code)
    }
	 
  walk(files, make_mask)
  
  
# Goto Kelp_1_Anomalies_and_calendars.R

