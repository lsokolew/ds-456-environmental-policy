# ds-456-environmental-policy

Group Members: Alicia Severiano Perez, Sydney Ohr, Lilabeth Sokolewicz

## Summarized Context & Research Question:

- Our project focuses on power plants in Minnesota and how they align with the state's commitment to environmental justice. Environmental justics aims to avoid disproportionate environmental and health impacts on marginalized communities, as well as taking the wellbeing and voice of all people into account when creating environmental policy. 

- Our research question concerns the locations of Minnesota powerplants and their effects on surrounding
communities, quanitified through demographics, air quality, and health outcomes.

## Motivation:
We were initially interested in how policies, both past and present, affect local communities. Past discriminatory practices led to the building of power plants in minority communities and these practices have lasting effects on surrounding communities today. We wanted to pinpoint these effects, show how these power plants are harmful, and determine what communities are hit the hardest. Governments are beginning to implement environmental justice principles into ordinances, yet these initiatives are at risk given pressures to withdraw funding. It has never been more important to recognize how different communities are impacted differently.  

 ## Description of Files:
  1. The Data folder consists of all the data used for this analysis
  2. The www folder contains all images present in the report
  3. The `runApp.R` file is where the ui render function lives
  4. The `ui.R` file organizes the layout of the shiny app, that being the plots, text, and sidebar
  5. The `server.r` file is where the code for the plots are stored
  6. The `Global.R` file is where we set themes and store text chunks
  7. The `CleaningData.R` file is where we clean and wrangle all data required for our analysis
  8. The `process_airdata.R` file is used to acquire air quality data, discussed below
  9. The `references` qmd and html, and folder (references_files) as well as the Library.bib file, produce our bibliography

To render the app, run the `runApp.R` file after downloading the neccessary data, packages, and running the `CleaningData.R` file. 


## Required Data:

### The raw Data (and used to wrangle) used in `CleaningData.R` or `process_airdata.R`: 
- `Power_Plants.csv`: We acquired this dataset from the EIA, the link, however is no longer working. We have been unable to find the dataset elsewhere, but it is in our repository. Having that, the information was saved by another user at the following link: https://michaelminn.net/tutorials/data/2024-power-plants-metadata.html
- `powerplant_data_eia_2024_generator_operable.csv`: Data from Form EIA-860 (2024). By EPA. Used for power plant first operation dates, for all operational powerplants in 2024. Available at [https://www.eia.gov/electricity/data/eia860/].
- `airdata_clean.csv`: Data from EPA. Used for annual average PM2.5 values in MN, 1980-2025. Available at the following link,https://aqs.epa.gov/aqsweb/airdata/download_files.html#Annual, but running `process_airdata.R` is recommended, as it pre-combines and filters the datasets as well as downloading them.
- `MN_asthma_ED.csv `: By MN Department of Health, via the MN Public Health Data Access Portal. Used for emergency department visits due to asthma.
- `zcta_pop_data.csv`: American Community Survey 5-Year data at the ZCTA level (acquired through Tidycensus) and has proportions of demographics by race and income.
- `ej_mpca_census.csv` &  `ej_mpca_census.shp` & `census_tribal_areas.shp`:  By Minnesota Pollution Control Agency. Via Minnesota Geospatial Commons. Used for location of environmental justice areas and demographic information. Link:https://gisdata.mn.gov/dataset/env-ej-mpca-census
- `school_program_locations.shp`: From the Minnesota Department of Education via the Minnesota Geospatial Commons. Used to map out the locations of schools in Minnesota.
- `CensusACSTract.xlsx`: Data from American Community Survery. This was used to look at median household income by census tract. The data is from 2019-2023 and from the following source:https://gisdata.mn.gov/dataset/us-mn-state-metc-society-census-acs 
- `mn_zctas_2020.rds`: Data from the United States Census Bureau. This was used to get the shapefiles for MN counties. 
- `emissions2017.xlsx`, `emissions2018.xlsx`,`emissions2019.xlsx`,`emissions2020.xlsx`,`emissions2021.xlsx`: From the U.S. Energy Information Administration. Used for NOx, SO2, and CO2 emissions.



### The edited data used to run App (all loaded in `global.R`):

- `mn_powerplants.csv`: Wrangled from the houseplants csv, where Minnesota only poweplants were filtered and labeled as fossil fuel or not
- `powerplants_sf.shp`: Exact Same as mn_powerplants just as a shapefile instead of csv
- `mn_tracts.shp`: obtained Minnesota census track geometries from library('tigris') and saved them to a shapefile to be loaded easily
- `ej_sf.shp`: wrangled the environmental justice area information from above, so the information would be long rather than wide, also saved it as a shapefile so it could be mapped
- `asthma_poc_powerplants.csv`: Joined data from MN_asthma_ED.csv, zcta_pop_data.csv, and mn_powerplants.csv.
- `distinct_schools_metro.csv`: Wrangled from the school_program_locations.shp to get the locations of schools in the metro area along with the distance to the nearest power plant.
- `ej_mpca_census.shp` & `census_tribal_areas.shp`:  By Minnesota Pollution Control Agency. Via Minnesota Geospatial Commons. Used for location of environmental justice areas and demographic information. Link:https://gisdata.mn.gov/dataset/env-ej-mpca-census
- `metro_area_pp.shp`: Calculated number of powerplants in EJ areas filtered to the metro area. Used mn_powerplants and ej_mpca_census datasets to do this
- `pp_summary.shp`: Calculated proportion of powerplants in EJ areas filtered in all MN. Used mn_powerplants and ej_mpca_census datasets to do this
- `wrangled_airdata.rds`: a combination of wrangled datasets for air quality anlysis, using `airdata_clean.csv` and `mn_powerplants.csv`. 
- `all_emissions_data.csv`: From the U.S. Energy Information Administration. Used for NOx, SO2, and CO2 emissions, wrangled/single dataset combining 2017-2021 information from emissions excel files
- `zcta_joined_asthma.shp`: Joined zip code data with asthma data from the MN Department of Health. Used to map out zip codes using Leaflet, along with asthma rates in these zip codes. 
- `AirData_allyears.rds`: a cleaned version of `airdata_clean.csv` above. 
- `school_program_locations.shp`: From the Minnesota Department of Education via the Minnesota Geospatial Commons. Used to map out the locations of schools in Minnesota.


## Required R Packages
- tidycensus
- dplyr
- tidyverse
- sf
- ggthemes
- ggplot2
- gganimate
- gifski
- readxl
- tigris
- shiny
- bslib
- leaflegend
- scales
- leaflet.extras2
- leaflet


