# Kelp Step 2: Regridding CMIP6 anlomalies to IOSST-friendly grid
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")
	
	
# Folders, etc. ----------------------------------------------------------------

  input_folder <- "/Volumes/MixRAID/Kelp_anomalies"
  output_folder <- make_folder("/Volumes/Mega_Disk/Kelp_regridded_anomalies")
  w <- 14  # Number of workers (parallel processes) to use


# Remap files ------------------------------------------------------------------
	
  remap_netCDF <- function(anom_file) {
    mask_nc <- "/Volumes/BackPack/OISST/mask_tos.nc"
    out_file <- anom_file %>% 
	    str_replace(input_folder, output_folder)
    cdo_code <- paste0("cdo -s -L -remapbil,", mask_nc, " ", anom_file, " ", out_file)
  	    system(cdo_code)
	  }

# Get files and remap
	netCDFs <- dir(input_folder, full.names = TRUE)
	plan(multisession, workers = w)
	  future_walk(netCDFs, remap_netCDF)
	plan(sequential)

# Goto Kelp_3_Add_observational_means_and_fillIDW.R

