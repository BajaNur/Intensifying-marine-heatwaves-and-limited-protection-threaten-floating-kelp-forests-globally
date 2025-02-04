# Kelp Step 13: Output the final data, just to be sure that we're all on the same page
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # January 2024


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Folders ----------------------------------------------------------------------

  input_folder1 <- "Final_CumInt_Summary_Outputs"
  output_folder <- make_folder("Final_Output_Data")


# Data --------------------------------------------------------------------

  files <- dir(input_folder1, full.names = TRUE)
  bits <- files %>% 
    basename() %>% 
    str_split("_", simplify = TRUE) %>% 
    map(1)
  scenarios <- bits  %>% 
    str_subset("ssp") %>% 
    unique()
  metrics <- bits %>% 
    map(c("median", "10th", "90th"), str_detect, string = .) %>%
    reduce(`|`) %>% 
    magrittr::extract(bits, .) %>% 
    str_remove_all(".csv") %>% 
    unique()
  xy <- read_rds("land_sea_weights.rds") %>% 
    dplyr::select(-percent_land)


# By scenario and metric -------------------------------------------------------

  combos <- expand_grid(scenarios, metrics)
  
  collect_and_combine_data <- function(s, m) {
    f <- files %>% 
      str_subset(s) %>% 
      str_subset(m)
    out_name <- f[1] %>% 
      str_replace(input_folder1, output_folder)
    read_csv(f) %>% 
      distinct()  %>% 
      left_join(xy, .) %>% 
      write_csv(out_name)
    }
  walk2(combos$scenarios, combos$metrics, collect_and_combine_data)  
  
# Goto Kelp_14_Cross_checking_summaries.R
  
  
  