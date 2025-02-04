# Kelp Step 16: Recomputing OISST MHWs
  # Written by Dave Schoeman (david.schoeman@gmail.com)
    # February 2024


# Source the helpers -----------------------------------------------------------

  source("Helpers.R")


# Folders ----------------------------------------------------------------------

  input_folder <- "/Users/davidschoeman/Dropbox/Documents/Data/OSST_Combo"
  

# Make base-grid raster --------------------------------------------------------
  
  r <- rast(resolution = 0.25)
  r[] <- 1
  mask2netCDF4(r, pth = getwd(), 
               ncName = basename("base_rast.nc"), 
               dname = "dummy", 
               dlname = "dummy")
  
# Pre-process OISST data -------------------------------------------------------
  
  files <- dir(input_folder, pattern = "_V2HR_raw_", full.names = TRUE)
  outname <- files[1] %>%
    str_replace("-19911231", "-20211231")
  cdo_code <- paste0("cdo -L -mergetime ", paste(files, collapse = " "), " ", outname)
    system(cdo_code)
  outname1 <- outname %>% 
    str_replace("_OISST_V2HR_raw_", "_remapped_V2HR_raw_")
  cdo_code <- paste0("cdo -L -remapbil,base_rast.nc ", outname, " ", outname1)
    system(cdo_code)
  outname2 <- outname %>% 
    str_replace("_OISST_V2HR_raw_", "_remappednfilled_V2HR_raw_")
  cdo_code <- paste0("cdo -L -setmisstodis,8 ", outname1, " ", outname2)
    system(cdo_code)
  outname3 <- outname2 %>% 
    str_replace("_remappednfilled_", "_remappednfilled365_")
  cdo_code <- paste0("cdo -L -setcalendar,365_day -delete,month=2,day=29 ", outname2, " ", outname3)
    system(cdo_code)
  

# Get boxes ---------------------------------------------------------------
    
    xy <- read_rds("coords.rda")
    res <- 5 # Allows us to change resolution of world boxes
    r <- rast(resolution = res) # A 5-degree grid
    r[] <- 1:ncell(r)
    cells <- extract(r, xy, xy = TRUE) %>% 
      dplyr::select(x, y) %>% 
      distinct() %>%  # The coordinates of the centres of the grid
      mutate(xmin = x - (res/2),
             xmax = x + (res/2),
             ymin = y - (res/2),
             ymax = y + (res/2)) %>% 
      dplyr::select(-1, -2)
    write_rds(cells, "boxes.rds")		
  
# Goto Kelp_17_OISST_MHW
  
  
  
  
  
  
  
  

