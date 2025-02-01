# Kelp Step 15: Outputting data per pixel and period to compare with Nur
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # January 2024


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Folders ----------------------------------------------------------------------

  input_folder <- "Final_Output_Data"
  output_folder <- make_folder("Redoing_MHW_by_pixel_and_period")


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
    out_name <- f[1] %>% 
      str_replace(input_folder, output_folder)
    read_csv(f) %>% 
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
      group_by(x, y, Period) %>% 
      dplyr::summarise(Mean_Cum_Int = mean(Cum_Int, na.rm = FALSE)) %>% 
      pivot_wider(names_from = Period, values_from = Mean_Cum_Int) %>% 
      dplyr::select(1, 2, 7, 6, 5, 3, 4) %>% 
      write_csv(out_name)
    }
  walk(scenarios, collect_scenario)  
  
  
  
  
  
  