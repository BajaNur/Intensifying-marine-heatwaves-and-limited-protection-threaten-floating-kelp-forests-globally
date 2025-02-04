# Kelp Step 11: Compute weights for each 0.25º grid square to represent %land for plot by latitude
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # December 2023


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Input data -------------------------------------------------------------------

  xy <- read_rds("coords.rda")
  sea <- st_read("/Users/davidschoeman/Dropbox/Documents/ShapeFiles/Ocean_hi_res/ne_10m_ocean.shp") %>% 
    st_make_valid()


# For each coordinate make a fine-scale rast, and compute % sea ----------------

  xy <- xy %>% 
    distinct() %>% 
    transpose()
  percent_sea <- function(cc){
    x1 <- cc$x - 0.125
    x2 <- cc$x + 0.125
    y1 <- cc$y - 0.125
    y2 <- cc$y + 0.125
    r <- rast(xmin = x1, xmax = x2, ymin = y1, ymax = y2, 
              resolution = .0001) # Approximately a 10-m grid at the equator
    rs <- rasterize(sea, r)
    ps <- data.frame(x = cc$x, y = cc$y, 
                     percent_land = values(rs) %>%
                       sum(na.rm = TRUE)/ncell(rs))
    return(ps)
    }
  out <- map(xy, percent_sea) %>% 
    bind_rows() %>% 
    mutate(percent_sea = 1 - percent_land)
  write_rds(out, "land_sea_weights.rds")
  
# Goto Kelp_12_Weighted_mean_median_by_latitude.R
  