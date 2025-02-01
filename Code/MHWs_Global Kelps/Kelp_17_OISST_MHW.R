# Kelp Step 17: Extract daily time series from each grid-square and compute MHW stats and save
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Folders --------------------------------------------------------

  input_file <- "/Users/davidschoeman/Dropbox/Documents/Data/OSST_Combo/tos_Oday_OISST_remappednfilled365_V2HR_raw_19820101-20211231.nc"
  working_folder <- make_folder("/Volumes/BackPack/tmp")
	output_folder <- make_folder("Final_Cum_Int_OISST")


# How many cores? ---------------------------------------------------------

	w <- detectCores()-4
	

# Subset the world --------------------------------------------------------
	
	cells <- read_rds("boxes.rds") %>% 
	  transpose() # Get the limits of the boxes and write them as a list
		
	split_the_world <- function(f) {
	  ff <- f %>% 
	    str_replace(input_folder, working_folder)
	  file.copy(f, ff)
	  do_split <- function(b) {
	    output_file <- f %>% 
	      str_replace(input_folder, working_folder) %>%
	      str_replace(".nc", paste0("_", b$xmin, "_", b$xmax, "_", b$ymin, "_", b$ymax, ".nc"))
	    cdo_code <- paste0("cdo -L -sellonlatbox,", paste(b$xmin, b$xmax, b$ymin, b$ymax, sep = ","), " ", ff, " ", output_file)
	      system(cdo_code)
	    }
	  
	  plan(multisession, workers = w)
	    future_walk(cells, do_split)
	  plan(sequential)
	  terminal_code <- paste0("rm ", ff)
	    system(terminal_code)
	  }
	
	
# Extract  bias corrected time series, and compute Cum Int ------------------------
	
	get_cum_int <- function(nc_file) {
	  r <- rast(nc_file)
	  xy <- read_rds("coords.rda")
	  r1 <- r[[1]]
	  pts <- terra::extract(r1, xy, xy = TRUE) %>% 
	    na.omit() %>% 
	    dplyr::select(x, y)
	  out <- terra::extract(r, pts, ID = FALSE)
	  tss <- map(transpose(out), unlist, use.names = FALSE)
		dates <- nc_open(nc_file) %>%
		  nc_dates()
		years <- unique(year(as.Date(dates)))

		# Function to extract cum_MHW_impact from each time series
  		get_mhw_imp <- function(ssts, 
  		                        dts = dates) { # To allow compliance with IPCC near-, mid- and long-term periods
  		  df <- data.frame(Date = dts, SST = ssts)
  		  if(sum(!is.na(df$SST)) > 0) {
  		    # First calculate the climatologies
  		    clim <- ts2clm(data = df, x = Date, y = SST, climatologyPeriod = c("1983-01-01", "2012-12-31")) # As per Oliver et al. and Smale et al.
  		    # Then the events
  		    yrs <- tibble(year = years)
  		    out <- detect_event(data = clim, x = Date, y = SST) %>% 
  		      .$climatology %>% 
  		      mutate(intensity = SST-seas,
  		             year = year(Date)) %>% 
  		      dplyr::filter(event == TRUE) %>% 
  		      group_by(year, .drop = FALSE) %>%
  		      dplyr::summarise(Cum_Int = sum(intensity, na.rm = TRUE))
  		    suppressMessages(out <- left_join(yrs, out) %>% 
  		                       mutate(Cum_Int = ifelse(is.na(Cum_Int), 0, Cum_Int)))
        		  } else {
        		    out <- tibble(year = years,
        		                  Cum_Int = NaN)
        		  }
  		  return(out$Cum_Int)
  		}	
  	# Do it
  		plan(multisession, workers = w)
  		    cum_int <- future_map(tss, get_mhw_imp, .options = furrr_options(seed = TRUE)) %>% 
  		      data.frame() %>% 
  		      t() %>% 
  		      data.frame(row.names = NULL) %>% 
  		      bind_cols(pts, .)
  		  plan(sequential)
  		  return(cum_int)
    }

# Do the work -------------------------------------------------------------

	do_cum_int <- function(f) {
	  split_the_world(f)
	  nc_boxes <- dir(working_folder, full.names = TRUE)
	  plan(multisession, workers = w)
	    out <- future_map(nc_boxes, get_cum_int, .options = furrr_options(seed = TRUE)) %>% 
	      bind_rows()
	  plan(sequential)
	  out_name <- f %>% 
	    str_replace(".nc", ".csv") %>% 
	    str_replace(input_folder, output_folder)
	  write_csv(out, out_name)
	  terminal_code <- paste0("rm ", paste0(nc_boxes, collapse = " "))
	    system(terminal_code)
	  }
	
	do_cum_int(input_file)
	
# Goto Kelp_18_Compute_final_OISST_values