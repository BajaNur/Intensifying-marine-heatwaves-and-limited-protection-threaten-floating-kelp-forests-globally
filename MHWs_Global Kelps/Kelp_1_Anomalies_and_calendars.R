# Kelp Step 1: Compute anomalies relative to historical periods and fix calendars
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023

  # Relies on merged CMIP6 daily files developed for Overshoot project

# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Folders and other parameters to set ------------------------------------------

  input_folder <- make_folder("/Volumes/Stripe_Raid/tmp_merged")
  anom_folder <- make_folder("/Volumes/MixRAID/Kelp_anomalies")
  w <- 9  # Number of workers (parallel processes) to use
  hist_year_start <- 1982 
  hist_year_end <- 2014 # End of CMIP6 historical
  ssp_year_start <- 2015 # Standard for CMIP6
  ssp_year_end <- 2100 # Most models end here...alter if you are working with longer series
  

# Anomalies --------------------------------------------------------------------
  # Projections
  netCDFs <- dir(input_folder, full.names = TRUE) %>% 
		  str_subset("ssp") # Just the projections
		
  # Make anomalies ---------------------------------------------------------------
  
    do_anom <- function(f) {
      anom_out <- str_replace_all(f, dirname(f), anom_folder)
        cdo_code <- paste0("cdo -L sub ", f, " -timmean -selyear,", hist_year_start, "/", hist_year_end, " ", f," ", anom_out)
        system(cdo_code)
      }
 
    plan(multisession, workers = w)
     future_walk(netCDFs, do_anom)  
    plan(sequential)


# Fix calendars ----------------------------------------------------------------

	netCDFs <- dir(anom_folder, full.names = TRUE)
	plan(multisession, workers = w)
	  future_walk(netCDFs, fix_cal)
	plan(sequential)
	
	
# Goto Kelp_2_Regrid_anomalies.R
		