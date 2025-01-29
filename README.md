# Intensifying-marine-heatwaves-and-limited-protection-threaten-floating-kelp-forests-globally
Code for "Intensifying marine heatwaves and limited protection threaten floating kelp forests globally"

You can download the three shapefiles needed to run the code in R stuido. The global map of floating kelp forests and the marine protected area database with the Protected Seas Scores for the countries that have floating kelp forests are in the Zenodo repository: https://doi.org/10.5281/zenodo.14736355. The marine ecoregions of the world shapefiles can be accessed at https://databasin.org/datasets/3b6b12e7bcca419990c9081c0af254a2/.  


Then there is a “Code” folder in this repository that has three sub folders with the codes for the different analyses. 1) “MHW estimation” estimates annual cumulative MHW intensities for all pixels with kelp forests. 

2) “Protection and MHW spatial analyses” conducts a spatial overlay analysis to quantify the % protection of kelp forests inside different types of marine protected areas. This analysis is also conducted at the country and biogeographic level. Then, we also conduct an overlay analysis with annual cumulative MHWs at the ecoregional level.

3) “Ecoregional MHW exposure”. This code estimates the mean annual MHW intensities for each time (Contemporary, short, mid, and long term) for each ecoregion.

