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
  9. The `references` qmd and html, as well as the Library.bib file, produce our bibliography

To render the app, run the `runApp.R` file after downloading the neccessary data, packages, and running the `CleaningData.R` file. 


## Required Data:
- `Power_Plants.csv`: We acquired this dataset from the EIA, at the following link, which is no longer working. We have been unable to find the dataset elsewhere, but it is in our repository. \[TODO: ADD lINK, LOOK FOR DATA ELSEWHERE\]
- `powerplant_data_eia_egrid_2024_generator_operable.csv`: Data from Form EIA-860 (2024). By EPA. Used for power plant first operation dates, for all operational powerplants in 2024. Available at [https://www.eia.gov/electricity/data/eia860/].
- `airdata_clean.csv`: Data from EPA. Used for annual average PM2.5 values in MN, 1980-2025. Acquirable at the following link,[https://aqs.epa.gov/aqsweb/airdata/download_files.html#Annual], or by running `process_airdata.R`. 
[[TODO: update the following\]
- `MN-asthma-zipcode.csv ` & `copd.csv`: By MN Department of Health, via the MN Public Health Data Access Portal. Used for hospitalizations due to asthma and COPD. 
- `CensusACSTract.xlsx`: American Community Survey 5-Year Summary File. By US Census Bureau. Via Minnesota Geospatial Commons. Used for househould income data. 
- `ej_mpca_census.csv`:  By Minnesota Pollution Control Agency. Via Minnesota Geospatial Commons. Used for location of environmental justice areas and demographic information.

## Required R Packages
- tidyverse
- dplyr
- shiny
- ggplot2
- sf
- bslib
- readxl
- leaflet
- ggthemes
- gganimate
- gifski
