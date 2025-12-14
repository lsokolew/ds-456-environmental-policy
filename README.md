# ds-456-environmental-policy

Group Members: Alicia Severiano Perez, Sydney Ohr, Lilabeth Sokolewicz

## Summarized Context & Research Question:

- Our project focuses on the topic of environmental justice, which aims to avoid disproportionate 
environmental and health impacts on marginalized communities, as well as taking the wellbeing and voice 
of all people into account when creating environmental policy. 

- Our research question concerns the locations of Minnesota powerplants and their effects on surrounding
communities, quanitified through demographics, air quality, and health outcomes.

## Motivation:
We were initially interested in how policies, both past and present, affect local communities. Past discriminatory practices led to the building of power plants in minority communities and these practices have lasting effects on surrounding communities today. We wanted to pinpoint these effects, show how these power plants are harmful, and determine what communities are hit the hardest. Governments are beginning to implement environmental justice principles into ordinances, yet these initiatives are at risk given pressures to withdraw funding. It has never been more important to recognize how different communities are impacted differently.   


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

## Ethical issues (who may be harmed and who may benefit): 

By taking a positive viewpoint on this topic and saying that Minnesota has a good framework for power plants,  we may be downplaying the experiences of some who live in communities negatively impacted by power plants. We cannot tell every individual’s story; while the data may look positive overall, we should not forget that not everyone’s experience is.

Further, by using pre-defined environmental justice areas (EJAs), we risk overlooking communities who don’t meet that exact definition, but should nevertheless be considered as a vulnerable area. For instance, a tract made up of 39% people of color will not qualify as an EJA for our analysis, yet it is barely distinct from a tract with 40% that would. 

## Description of Files:
  1. The Data folder consists of all the data used for this analysis
  2. The `ui.R` file organizes the layout of the shiny app, that being the plots, text, and sidebar
  3. The `server.r` file is where the code for the plots are stored
  4. The `app.R` file is where the ui render function lives
  5. The `set_code.R` file is where we load in and wrangle data that will be used for plots
  6. `lilabeth_aq_work.qmd` contains an analysis of air quality and powerplants in Minnesota.
  7. `FP2Redone.qmd` contains an analysis of hospitalizations due to asthma and COPD. 

## Datasets Used:
- `Power_Plants.csv`: By EIA. Used for power plant point locations, fuel types, output amounts, and other characteristics.
- `powerplant_data_eia_egrid_2024_generator_operable.csv` & `powerplant_data_eia_egrid_2024_generator_retired.csv`: eGRID Form EIA-860. By EPA. Used for powerplant operational & retirement dates.
- `Modeled_PM25_Ozone_MN_county_data_allyears.csv`: By Minnesota Pollution Control Agency. Used for ozone & PM2.5 values in MN counties, including modeled data for counties without monitors.
- `MN-asthma-zipcode.csv ` & `copd.csv`: By MN Department of Health, via the MN Public Health Data Access Portal. Used for hospitalizations due to asthma and COPD. 
- `CensusACSTract.xlsx`: American Community Survey 5-Year Summary File. By US Census Bureau. Via Minnesota Geospatial Commons. Used for househould income data. 
- `ej_mpca_census.csv`:  By Minnesota Pollution Control Agency. Via Minnesota Geospatial Commons. Used for location of environmental justice areas and demographic information. 

## Plan for the rest of the semester can be found here: 
https://docs.google.com/document/d/1UmGPfNbmCvgxP4FVRG4xTKzD4NWpulKdt1rvIgYfyVs/edit?usp=sharing

## Next steps:
Minnesota has one of the leading frameworks regarding power plants, their environmental impacts, and where they can be located. Our goal is to do a comparative analysis of another city that may lack environmental justice policies and how their different policies impact local communities. For our final shiny app, we want to be able to tell a story— the history of power plants in the state, policies, the demographics affected— through an article format. 


## Contributions to FP4:
- Alicia: built Shiny app framework, demographics & EJA analysis
- Lilabeth: air quality analysis, looked into the historical data and fixed issues in the data 
- Sydney: health analysis, had a lot of context on negative impacts of power plants in other areas
- All: made next steps plan, drafted presentation, contributed to shiny app

## AI Statement
No AI was used in creating this README file.
