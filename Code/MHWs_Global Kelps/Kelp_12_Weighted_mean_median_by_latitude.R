# Kelp Step 12: Compute weighted mean and median MHW Cum Int by latitude
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # December 2023


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Folders ----------------------------------------------------------------------

  input_folder1 <- "Final_CumInt_Summary_Outputs"
  output_folder <- make_folder("MHW_by_lat_period")


# Data --------------------------------------------------------------------

  files <- dir(input_folder1, full.names = TRUE) %>% 
    str_subset("_median.csv")
  scenarios <- files %>% 
    basename() %>% 
    str_split("_", simplify = TRUE) %>% 
    map(1) %>% 
    str_subset("ssp") %>% 
    unique()
  xy <- read_rds("land_sea_weights.rds") %>% 
    dplyr::select(-percent_land)


# By scenario ------------------------------------------------------------------

  collect_scenario <- function(s) {
    f <- files %>% 
      str_subset(s)
    out_name <- f[1] %>% 
      str_replace(input_folder1, output_folder)
    read_csv(f) %>% 
      distinct() %>% 
      pivot_longer(-c(x, y), names_to = "Year", values_to = "Cum_Int") %>% 
      mutate(Year = as.numeric(Year)) %>%
      mutate(Period = case_when(
        between(Year, 2001, 2020) ~ "Present",
        between(Year, 2021, 2040) ~ "Near",
        between(Year, 2041, 2060) ~ "Mid",
        between(Year, 2061, 2080) ~ "Int",
        between(Year, 2081, 2100) ~ "Long",
        TRUE ~ NA)) %>% 
      na.omit() %>% 
      distinct() %>% 
      group_by(Period, x, y) %>% 
      dplyr::summarise(Cum_Int = mean(Cum_Int)) %>% 
      ungroup() %>% 
      left_join(xy) %>% 
      group_by(Period, y) %>% 
      dplyr::summarise(meanCI = mean(Cum_Int),
                wghtd_meanCI = weighted.mean(Cum_Int, percent_sea)) %>% 
      write_csv(out_name)
    }
  walk(scenarios, collect_scenario)  

# Goto Kelp_13_Output_final_data.R
  
  
  
  
  