# Kelp Step 7: Get medians and percentiles from the raw data
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Folders, etc. ----------------------------------------------------------------

  input_folder <- "Final_Cum_Int"
  output_folder <- make_folder("Final_CumInt_Summary_Outputs")
  w <- detectCores()-4  # Number of workers (parallel processes) to use
  

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

	make_summaries <- function(v, fr, s) {
		files <- dir(input_folder, full.names = TRUE)%>% 
		  str_subset(paste0("(?=.*", v, "_", ")(?=.*", fr, "_", ")(?=.*", s, "_", ")"))
		out_name <- files[1] %>% 
		  str_replace(input_folder, output_folder) %>% 
		  str_replace(get_CMIP6_bits(files[1])$Model, "results") %>% 
		  str_replace(get_CMIP6_bits(files[1])$Frequency, "Oyear") %>% 
		  str_replace(get_CMIP6_bits(files[1])$Variable, "CumInt")
	  d <- map(files, read_csv) %>% 
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
	    write_csv(paste0(output_folder, "/", s, "_median.csv"))
	  d2 <- d %>% 
	    dplyr::select(c(1:3, 5)) %>% 
	    pivot_wider(names_from = year, values_from = pctl_10) %>% 
	    write_csv(paste0(output_folder, "/", s, "_10th_pctl.csv"))
	  d3 <- d %>% 
	    dplyr::select(c(1:3, 6)) %>% 
	    pivot_wider(names_from = year, values_from = pctl_90) %>% 
	    write_csv(paste0(output_folder, "/", s, "_90th_pctl.csv"))
   }
	
  pwalk(l, make_summaries)
  
# Goto Kelp_8_Check_OISST_CumInt.R
	