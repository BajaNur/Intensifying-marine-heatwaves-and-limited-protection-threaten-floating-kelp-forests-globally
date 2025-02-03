# Kelp Step 6: Create ensemble medians across models for each scenario and period
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# May 2022; modified October 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Folders, etc. ----------------------------------------------------------------

  input_folder <- "/Volumes/Stripe_Raid/Kelp_bias_corrected_and_filled"
  w <- 3  # Number of workers (parallel processes) to use
  

# Get combinations -------------------------------------------------------------

  # List with combinations of variables and scenarios
  l <- dir(input_folder, full.names = TRUE) %>% 
    map(get_CMIP6_bits) %>%  
    map(`[`, c("Variable", "Frequency", "Scenario")) %>% 
    map(bind_cols) %>% 
    bind_rows() %>% 
    distinct() %>% 
    as.list() %>% 
    unname() 
  

# Make CMIP ensemble medians ---------------------------------------------------

	make_ensembles <- function(v, fr, s) {
		files <- dir(input_folder, full.names = TRUE)%>% 
		  str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", s, "_", ")"))
		out_name <- files[1] %>% 
		  str_replace(get_CMIP6_bits(files[1])$Model, "ensemble")
	  cdo_code <- paste0("cdo -L -z zip -ensmedian ", paste0(files, collapse = " "), " ", out_name)
	  system(cdo_code)
		}
	plan(multisession, workers = w)
		future_pwalk(l, make_ensembles)
	plan(sequential)
	# Note that the warning "cdo    ensmedian (Warning): Input parameters have different levels!" is just a warning. The ensemble is produced, anyway.
	
# Goto CMIP_7_Mask_ensembles_with_observed_mask.R
