# Kelp Step 9: Compare the CumInt MHS across OISST and historical runs from ESMs
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# December 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Folders, etc. ----------------------------------------------------------------

  esm_folder <- "Final_Cum_Int"
  oisst_folder <- "OISST_Cum_Int"
  

# Get scenarios -------------------------------------------------------------

  # List with combinations of variables and scenarios
  scenarios <- dir(esm_folder) %>% 
    map(get_CMIP6_bits) %>%  
    map("Scenario") %>%
    unlist() %>% 
    unique()
  

# Get files and combine them ---------------------------------------------------

	esm <- dir(esm_folder, full.names = TRUE)
  oisst <- dir(oisst_folder, full.names = TRUE)
  hist_files <- c(ens, esm)


# Function to get the info we need from a folder --------------------------

  get_mn_near_term_CI <- function(f) {
    read_csv(f) %>% 
      dplyr::select(3:35) %>% 
      rowMeans()
    }
  

# Deploy this across files ------------------------------------------------

  plot_comparison <- function(s) {
    h <- hist_files %>% 
      str_subset(s) %>% 
      str_subset("_ensemble_", negate = TRUE)
    hist <- map(h, get_mn_near_term_CI) %>% 
      bind_cols() %>% 
      rowwise() %>%
      dplyr::mutate(`Median of Historical` = median(c_across(where(is.numeric)))) %>% 
      dplyr::select(`Median of Historical`)
    e <- hist_files %>% 
      str_subset(s) %>% 
      str_subset("_ensemble_")
    ee <- get_mn_near_term_CI(e) %>% 
      tibble(Ensemble = .)
    oo <- get_mn_near_term_CI(oisst) %>% 
      tibble(OISST = .)
    plt <- bind_cols(oo, ee, hist) %>% 
      pivot_longer(-1, names_to = "Approach", values_to = "CI") %>% 
      ggplot(aes(x = OISST, y = CI, col = Approach)) +
        geom_point(alpha = .25) +
        geom_smooth(method = "lm") + 
      labs(x = "Mean MHW Cum Int from OISST",
           y = "Mean MHW Cum Int from 9 ESMs",
           title = paste("Historical (1982-2014) mean annual MHW Cum Int for ", s)) +
      theme_bw()
      return(plt)
      }  
  plots <- map(scenarios, plot_comparison)
  library(patchwork)
  (plots[[1]] + plots[[2]]) / (plots[[3]] + plot_spacer())
  # OF COURSE, they're all the same, because they're all based on historical data, only...so you need to look at only one
  
  plots[[1]] +
    labs(title = "Historical (1982-2014) mean annual MHW Cum Int") 
  ggsave("Ensemble vs ESMs.pdf", paper = "A4r")
  

# See if we can ID the "hot blob" -----------------------------------------
  
    h <- hist_files %>% 
      str_subset(scenarios[1]) %>% 
      str_subset("_ensemble_", negate = TRUE)
    xy <- read_csv(h[1]) %>% 
      dplyr::select(x, y)
    hist <- map(h, get_mn_near_term_CI) %>% 
      bind_cols() %>% 
      rowwise() %>%
      dplyr::mutate(Median = median(c_across(where(is.numeric)))) %>% 
      dplyr::select(Median)
    bind_cols(xy, hist) %>% 
      ggplot(aes(x = y, y = Median)) +
        geom_point(alpha = .5) +
        labs(x = "Latitude",
             y = "Mean annual median MHW Cum Int") +
        theme_bw()
    ggsave("Historical MHW CI vs latitude.pdf", paper = "A4r")
    
# Goto Kelp_10_Check_for_ice.R
