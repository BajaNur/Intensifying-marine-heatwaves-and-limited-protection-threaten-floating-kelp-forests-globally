# Kelp Step 18: Get medians and percentiles from the OISST data
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Folders, etc. ----------------------------------------------------------------

  input_folder <- "Final_Cum_Int_OISST"
  output_folder <- make_folder("Final_CumInt_OISST_Summary_Outputs")
  w <- detectCores()-4  # Number of workers (parallel processes) to use
  

# Make CMIP ensemble medians ---------------------------------------------------

  files <- dir(input_folder, full.names = TRUE)
	make_summaries <- function(f) {
		# files <- dir(input_folder, full.names = TRUE)%>% 
		#   str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", s, "_", ")"))
		out_name <- f %>%
		  basename()%>% 
		  str_replace(get_CMIP6_bits(f)$Model, "results") %>% 
		  str_replace(get_CMIP6_bits(f)$Frequency, "Oyear") %>% 
		  str_replace(get_CMIP6_bits(f)$Variable, "CumInt") %>% 
		  paste0(output_folder, "/", .)
	  d <- map(f, read_csv) %>% 
	    map(~pivot_longer(.x, -c(1:2), names_to = "Year", values_to = "CumInt")) %>%
	    bind_cols() %>% 
	    dplyr::rename(x = 1, y = 2, year = 3) %>% 
	    dplyr::select(x, y, year, starts_with("CumInt")) %>% 
	    mutate(year = str_remove(year, "^X")) %>%
	    mutate(year = as.numeric(year)) %>%
	    mutate(year = year + 1981) %>% 
	    pivot_longer(-c(1:3), names_to = "model", values_to = "Cum_Int") %>% 
	    group_by(x, y, year) %>% 
	    dplyr::summarise(median_CI = median(Cum_Int, na.rm = TRUE),
	              pctl_10 = quantile(Cum_Int, .1, na.rm = TRUE),
	              pctl_90 = quantile(Cum_Int, .90, na.rm = TRUE)) %>% 
	    ungroup()
	  d1 <- d %>% 
	    dplyr::select(1:4) %>% 
	    pivot_wider(names_from = year, values_from = median_CI) %>% 
	    write_csv(paste0(output_folder, "/OISST_median.csv"))
   }
	
  make_summaries(files)
	