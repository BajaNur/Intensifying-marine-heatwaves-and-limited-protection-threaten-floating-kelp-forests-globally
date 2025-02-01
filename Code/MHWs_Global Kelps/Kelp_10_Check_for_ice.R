# Kelp Step 10: Check to see whether ice is messing with MHWs
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # December 2023


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Folders, etc. ----------------------------------------------------------------

  input_folder <- "/Volumes/Bullet_RAID/Kelp_monthly"
  output_folder <- make_folder("/Volumes/Bullet_RAID/Kelp_test")


# Work out with monthly averages go below 2C ------------------------------

  files <- dir(input_folder, pattern = "Omon_", full.names = TRUE)
  xy <- read_rds("coords.rda") %>% 
    dplyr::filter(y > 53)
  
  get_monthly_SST <- function(f, crit_tmp = 0) {
    r <- rast(f)
    out <- terra::extract(r, xy, ID = FALSE)
    ssts <- map(transpose(out), unlist, use.names = FALSE) %>% 
      map(~which(.x <= crit_tmp)) %>% 
      keep( ~ length(.x) > 0) %>% 
      append(basename(f), .)
    return(ssts)
    }

  plan(multisession, workers = 15)
    ice_test <- future_map(files, get_monthly_SST)
  plan(sequential)
  
  ice_test %>% 
    map(unlist) %>% 
    map(unique)
  