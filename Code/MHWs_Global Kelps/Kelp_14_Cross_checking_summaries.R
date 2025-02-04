# Kelp Step 14: Recomputing summaries to compare with Nur
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # January 2024


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Folders ----------------------------------------------------------------------

  input_folder <- "Final_Output_Data"
  output_folder <- make_folder("Redoing_MHW_by_lat_period")


# Data --------------------------------------------------------------------

  files <- dir(input_folder, full.names = TRUE) %>% 
    str_subset("_median.csv")
  scenarios <- files %>% 
    basename() %>% 
    str_split("_", simplify = TRUE) %>% 
    map(1) %>% 
    str_subset("ssp") %>% 
    unique()


# By scenario ------------------------------------------------------------------

  collect_scenario <- function(s) {
    f <- files %>% 
      str_subset(s)
    d <- read_csv(f) %>% 
      distinct() %>% 
      pivot_longer(-c(x, y, percent_sea), names_to = "Year", values_to = "Cum_Int") %>% 
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
      group_by(Period, y) %>% 
      dplyr::summarise(meanCI = mean(Cum_Int),
                wghtd_meanCI = weighted.mean(Cum_Int, percent_sea))
    out_name1 <- f[1] %>% 
      str_replace(input_folder, output_folder) %>% 
      str_replace(".csv", "_unweighted.csv")
    out_name2 <- f[1] %>% 
      str_replace(input_folder, output_folder) %>% 
      str_replace(".csv", "_weighted.csv")
    d %>% 
      dplyr::select(-4) %>% 
      pivot_wider(names_from = Period, values_from = meanCI) %>% 
      dplyr::select(1, 6, 5, 4, 2, 3) %>% 
      write_csv(out_name1)
    d %>% 
      dplyr::select(-3) %>% 
      pivot_wider(names_from = Period, values_from = wghtd_meanCI) %>% 
      dplyr::select(1, 6, 5, 4, 2, 3) %>% 
      write_csv(out_name2)
    }
  walk(scenarios, collect_scenario)  
  
  
# Goto Kelp_15_Outputs_per_pixel_by_period.R
  
  
  