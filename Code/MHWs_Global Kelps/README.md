# Processing ESM data from CMIP6
  Generic routines for processing — regridding and (simple) bias correcting — ESM outputs downloaded from ESGF

## Initial warnings
This code is provided "as is". It runs start-to-end on my machine, but I cannot guarantee the same for others, as I have not tested it in an external environment.
Much of the code requires system calls to `CDO` (Climate Data Operators), described here: https://code.mpimet.mpg.de/projects/cdo/wiki. These system calls work well on MacOS, *but I have not tested them in the Windows environment*.
I have done my best to avoid use of `R` packages nearing deprecation, but  this might need final checks.

## Dependencies
The user will need to install `CDO` (Climate Data Operators: https://code.mpimet.mpg.de/projects/cdo/wiki), `NCO` (netCDF Operarors: https://nco.sourceforge.net) and `wget` (https://www.gnu.org/software/wget) on their machine for this code to work. 

## Sequence of code chunks
### Utilities

- `Nur_kelp_maps_code.Rproj`: The `R` project file

- `Helpers.R`: A list of packages and a series of utility functions that are used regularly across several code chunks. This file is sourced at the start of each code chunk.

### Process observational data — in this case, IOSST data

- `OBS_1_Merge_observations_and_extract_mean.R`: Functions to merge data for observations, and to compute an observational mean for use in "delta-change" bias correction. This includes a routine to regrid to a `terra`-friendly grid of given resolution (default code is 0.25º). By default `cdo` returns grids that are offest to the east by a small amount.

- `OBS_2_Prepare_mask_from_observations.R`: An optional code chunk, should a mask be required to harmonise ESM grids with those for observations.

### Process ESM outputs

- I started with raw ESM tos (SST) merged with historical data back to 1982 and running our to 2100. These data were developed in a previous project. Code can be provided.

- `Kelp_1_Anomalies_and_calendars.R`: Computes anomalies relative to the historical (1982-2014) mean, then standardises calendars (365-day calendar, after deleting leap days for daily data).

- `Kelp_2_Regrid_anomalies.R`: Regrid all ESM anomalies onto a standard. `terra`-friendly grid of given resolution (default code is 0.25º).

- `Kelp_3_Add_observational_means_and_fillIDW.R`: Complete "delta-change" bias correction by adding the observational mean to each set of anomalies, resulting in data in intended units that are bias corrected relative to observations.At the same time, use inverse-distance weighted interpolation to fill in conditions at the coast (ESMs often don't capture coastal cells). Interpolation isn't ideal, but it is the method employed by well-known datasets like Bio-ORACLE.

- `Kelp_4_Coordinates_and_Boxes.R`: Identify the coordinates of 0.25° cells that contain kelp, then use those to identify 5° boxes from which SST can easily be extracted.

- `Kelp_5_Split_the_world_and_get_CumInt.R`: For each model in turn, break the world into 5° boxes that contain kelp, extract the daily time series and compute cumulative MHW intensity per year. As per Oliver et al. and Smale et al., we used a 30-year baseline running "1983-01-01", "2012-12-31")). Each step of the MHW computation has been carefully checked.


### ESMs and data used
  Daily data (tos) from ESGF for 9 ESMs for which the ssp5-3.4-overshoot scenario is available. This is an ensemble already being used for a separate project:
  
    - ACCESS-CM2
    
    - ACCESS-ESM1-5 
    
    - CanESM5
  
    - CESM2-WACCM
  
    - CMCC-ESM2
    
    - IPSL-CM6A-LR
  
    - MIROC6
  
    - MRI-ESM2-0
    
    - NorESM2-LM

### Scenarios
  For each ESM, I downloaded historical and SSP1-2.6, SSP2-4.5, SSP5-8.5 data

### Variables
  `tos`: sea surface temperature (K)

## General approach, caveats and notes

Note that the warning during ensembling `cdo    ensmedian (Warning): Input parameters have different levels!` is just a warning. The ensemble is produced, anyway, discarding levels, as needed.

Raw and processed data files (mainly in netCDF format) require significant storage space (upwards of 10TB). Some routines are also memory intensive, so adjustments to arguments might be needed in the calls to set up parallel processes. Finally, depending on processor speed and number of ESMs/scenarios considered, this workflow can take >1 week to run. Do not attempt the workflow before provisioning your machine accordingly.

