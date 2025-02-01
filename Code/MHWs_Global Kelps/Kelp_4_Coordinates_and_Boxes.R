# Kelp Step 4: Get coordinates of kelp grid cells and boxes they fall in
	# Written by Dave Schoeman (david.schoeman@gmail.com)
		# November 2023


# Source the helpers -----------------------------------------------------------

	source("Helpers.R")


# Get kelp cells and make boxes to break the world -----------------------------
	
	m <- st_read("Data/Global_Floating_Kelp/Global_Kelp_Canopy_2-24.shp") %>% 
	  st_make_valid() %>% # The damned shapefile is still broken somewhere
	  dplyr::filter(!st_is_empty(.)) # 22 geometries (at last count) were empty — remove these
	mm <- vect(m) # Need this to rasterize in terra
	
	# Rasterize kelp to 0.25º
  	grd <- rast(resolution = .25)
  	r <- terra::rasterize(mm, grd, touches = TRUE) # Gives you all 0.25º cells that contain kelp. These are the ones we need MHW data for.
  	xy <- as.data.frame(r, xy = TRUE) %>% 
  	  dplyr::select(-layer) # Cell centres
  	saveRDS(xy, "coords.rda")
	
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

