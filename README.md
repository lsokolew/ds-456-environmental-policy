# ds-456-environmental-policy

Group Members: Alicia Severiano Perez, Sydney Ohr, Lilabeth Sokolewicz

## Summarized Context & Research Question:

- Our project focuses on the topic of environmental justice, which aims to avoid disproportionate 
environmental and health impacts on marginalized communities, as well as taking the wellbeing and voice 
of all people into account when creating environmental policy. 

- Our research question concerns the locations of Minnesota powerplants and their effects on surrounding
communities, quanitified through demographics, air quality, and health outcomes. 

## Longer Context & Research Question:

The need for electricity stems from its essential role in daily life, powering various
things from vehicle charging to heating and refrigeration. Due to the vast amount of demand and use of it, 
finding reliable sources of electricity has become crucial. Power plants are facilities that generate electric
energy from various sources, including fossil fuels, such as coal, natural gas, and petroleum, and renewable sources, 
like sunlight, water, and wind. Fossil fuel power plants operate by burning  the chosen fuel to generate heat,
which generates steam or gas in a boiler. This steam or gas spins a turbine, which converts heat energy into 
rotational energy, that is then transformed into electricity. However, this process releases vast amounts of harmful 
pollutants, such as mercury, greenhouse gasses, and carbon dioxide. The release of such pollutants contributes to harmful 
air pollution levels that harm individuals' health. On the other hand, renewable energy power plants are 
considered to be cleaner, as they don’t burn fuel or release greenhouse gases. However, there are still some 
drawbacks to individuals, such as the displacement of communities, in order to build these power plants. Furthermore, 
research has shown that fossil fuel power plants have been constructed near predominantly black, hispanic, and asian 
communities and historical redlined areas. Some examples include Chicago, Los Angeles, and Philadelphia. Consequently, 
these communities end up being harmed by the releases of different fuels and are the ones shouldering the unequal
distribution of air quality as a result of these power plants.

When it comes to Minnesota, there are about 100 fossil fuel and 682 renewable energy power plants distributed around the state.
This has led us to wonder about the placement of these power plants throughout the state, particularly to discover 
if they are also built around communities of color. Furthermore, we would like to explore the air quality and health effects
of them. 


## Description of Files:
  1. The 'Data" folder consists of all the data used for this analysis
  2. The 'ui.R' file organizes the layout of the shiny app, that being the plots, text, and sidebar
  3. The 'server.r' file is where the code for the plots are stored
  4. The 'app.R' file is where the ui render function lives
  5. The 'set_code.R' file is where we load in and wrangle data that will be used for plots

## Ethical issues (who may be harmed and who may benefit): ???

[INSERT FILE/ORGANIZATIONAL DESCRIPTIONS] ("If there's more than one RMD, an overall guide to what is in each one")
- `lilabeth_aq_work` contains an analysis of air quality and powerplants in Minnesota. 

## Datasets Used:
- Power plant dataset: from EIA. Used for power plant point locations, fuel types, output amounts, and other characteristics.
- eGRID Form EIA-860: from EPA. Used for powerplant operational & retirement dates.
- Minnesota Air Quality Data by county: From Minnesota Pollution Control Agency. Used for ozone & PM2.5 values in MN counties, including modeled data for counties without monitors.
- Hospitalization data: From the MN Department of Health, via the MN Public Health Data Access Portal. Used for hospitalizations due to asthma and COPD. 
- American Community Survey 5-Year Summary File: From US Census Bureau. Via Minnesota Geospatial Commons. Used for househould income data. 
- Environmental Justice Areas:  By Minnesota Pollution Control Agency. Via Minnesota Geospatial Commons. Used for location of environmental justice areas and demographic information. 

## Plan for the rest of the semester can be found here: 
https://docs.google.com/document/d/1UmGPfNbmCvgxP4FVRG4xTKzD4NWpulKdt1rvIgYfyVs/edit?usp=sharing

## Contributions to FP4:
- Alicia: built Shiny app framework, ...
- Lilabeth: ...
- Sydney: ...
