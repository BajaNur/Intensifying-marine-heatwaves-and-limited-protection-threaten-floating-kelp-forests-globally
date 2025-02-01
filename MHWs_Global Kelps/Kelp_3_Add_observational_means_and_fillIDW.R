# Kelp Step 3: Add observational means back to the anomalies to complete the "change-factor"/"delta-method" bias correction, then fill missing values (land) with inverse-distance-weighted means to accommodate coastal cells
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")
	
	
# Folders, etc. ----------------------------------------------------------------

  input_folder <- "/Volumes/Mega_Disk/Kelp_regridded_anomalies"
  output_folder <- make_folder("/Volumes/Stripe_Raid/Kelp_bias_corrected_and_filled")
  w <- 14  # Number of workers (parallel processes) to use

	 
# Bias-correct and fill anomalies ----------------------------------------------
  
  mn <- "/Volumes/BackPack/OISST/mean_tos_Oday_OISST_observed_r1i1p1f1_rg_19820101-20141231.nc"
  delta_correct <- function(f) {
    out_file <- f %>% 
      str_replace(input_folder, output_folder)
    cdo_code <- paste0("cdo -s -L -setmisstodis,8 -add ", f, " ", mn, " ", out_file)
      system(cdo_code)	  
    }

  netCDFs <- dir(input_folder, full.names = TRUE)      
  plan(multisession, workers = w)
    future_walk(netCDFs, delta_correct)
  plan(sequential)
  
# Goto Kelp_4_Coordinates_and_Boxes.R





