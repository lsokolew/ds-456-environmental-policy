# Libraries
library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(sf)
library(gifski)  
###================================ Load in Data ================================###


mn_powerplants              <- read_csv('Data/cleaning_data/mn_powerplants.csv') 
zcta_joined                 <- st_read('Data/cleaning_data/zcta_joined.shp') 
AirData_allyears            <- readRDS("Data/aq_data_clean/AirData_allyears.rds")
schools_sf                  <- sf::read_sf("Data/shp_struc_school_program_locs/school_program_locations.shp")
mn_tracts                   <- st_read("Data/cleaning_data/mn_tracts.shp")
ej_sf                       <- st_read("Data/cleaning_data/ej_sf.shp")
asthma_poc_powerplant       <- read_csv("Data/cleaning_data/asthma_poc_powerplant.csv")
distinct_schools_metro.csv  <- read_csv("Data/cleaning_data/distinct_schools_metro.csv")
ej_shp                      <- st_read("Data/ej_mpca/ej_mpca_census.shp")
metro_area                  <- st_read("Data/cleaning_data/metro_area_pp.shp")
powerplants                 <- st_read("Data/cleaning_data/powerplants_sf.shp")
tribal_shp_wgs              <- st_read("Data/tribal_areas/census_tribal_areas.shp")


load("Data/aq_data_clean/wrangled_airdata.rds")
all_emissions_data <- read_csv("Data/cleaning_data/all_emissions_data.csv")


# Metro Zip Codes
metro_zips <- c(55401, 55402, 55403, 55404, 55405, 55406, 55407, 55408, 55409, 
                55410, 55411, 55412, 55413, 55414, 55415, 55416, 55418, 55419, 55422,
                55423, 55426, 55427, 55428, 55431, 55433, 55443, 55450, 55454, 55455, 
                55101, 55102, 55103, 55104, 55105, 55106, 55107, 55108, 55109, 55110, 
                55111, 55112, 55113, 55114, 55115, 55116, 55117, 55118, 55119, 55121,
                55122, 55123, 55124, 55125, 55126, 55127, 55128, 55129, 55130, 55133, 
                55144, 55145, 55146, 55150, 55155, 55161, 55164, 55165, 55168, 55170, 
                55171, 55172, 55175, 55180, 55187, 55199, 55305, 55311, 55316, 55331, 
                55340, 55343, 55344, 55345, 55346, 55347, 55348, 55356, 55357, 55359, 
                55361, 55364, 55369, 55374, 55375, 55384, 55391, 55392, 55005, 55070,
                55079, 55092, 55303)


###================================ Colors/Fonts/ETC ================================###

# App color
light_blue <- "#CBE0F7"

# theme to use around ggplots
theme_1 <- theme(panel.background = element_blank(),     
                 plot.background = element_blank(),     
                 panel.grid = element_blank(),
                 strip.background = element_blank(),
                 legend.background = element_rect(fill = "transparent", color = NA) )

# Fuel typ color
fuel_colors <- scale_fill_manual( values = c("Fossil Fuel" = "#d95f02", "Renewable" = "#1C693A"))

# colors for ej areas
pal1 <- colorFactor(palette = c("#dc0073", "#22577a", "#c44900", "#f4e285"),
                    domain = c("ALL", "ONLY LOW INCOME", "ONLY POC", "ONLY POC & LOW INCOME"))

# health palete
pal3 <- colorFactor(palette = c("red", "green"), domain = mn_powerplants$fossil_fuel)

# HERC - POC pallete
poc_cols <- c("#D5D1E6","#DFCEE9", "#534680","#312A4C", "#1E192E") 
herc_cols <- colorBin(palette = c("#D5D1E6","#DFCEE9", "#534680","#312A4C", "#1E192E"),
                      domain = (metro_area$prppoc *100), bins = 5)

# For all plants
  plants_proj_1 <- st_transform(powerplants, crs = 26915)
  buffer_distance <- 1609.34 * 1
  plants_buffer <- st_buffer(plants_proj_1, dist = buffer_distance)
  plants_buffer <- st_transform(plants_buffer, crs = 4326)

  
###================================ Buffer Information (HERC) ================================###
  
# Filter to the HERC
herc<- powerplants %>% filter(plnt_cd == 10013)

# make sure the data is correct crs
plants_proj <- st_transform(herc, crs = 26915)

# determine 1 mile buffer based on km
buffer_distance <- 1609.34 * 1

# create buffer and then transform data back to crs for leaflet
herc_buffer <- st_buffer(plants_proj, dist = buffer_distance)
herc_buffer <- st_transform(herc_buffer, crs = 4326)

###================================ Text Tab 1  ================================###

intro <- "Intro"

# power plants

pp_locations <- "Power plants draw from several sources of energy aside from traditional fossil fuels. Wind farms, particularly on the state's open
western and southern prairies, allow for wind energy to provide 25% of the state's total net electricity generation in 2024 (US EIA, 2025).
The state's flat terrain offers little opportunities for hydroelectric generation; however, several small plants in the northeast produce roughly 1%
of the state's electricity. Most solid waste biomass plants are in highly populated areas of southern Minnesota, while wood-fueled plants tend to be in the more forested parts of northern
Minnesota (US EIA, 2025).
"

pp_energy_sources <- "There are far more solar-fired power plants in Minnesota than any other kind - by nearly 5 times. However, as we
see in the map above, many are very small, with a capacity between 1 to 5 MW. Wind plants are the second-most-common, and tend to have a
higher capacity (around 40 MW on average). Notably, Minnesota has only 8 coal and 2 nuclear plants. Yet, these plants have capacities of
several hundred MW, meaning that one coal plant produces, on average, as much as 10 wind plants and more than 100 solar plants."

pp_elec_by_source <- "Indeed, we see that electricity production by fossil-fuel plants outstrips wind and solar. Solar
produces around the same amount as nuclear, despite having a drastically greater number of plants. While wind makes up a
significant part of electricity produced, as the EPA claims, natural gas produces the most out of any energy source in the state.
Around one-fifth of the state's natural gas goes to electricity production, which has been increasing in recent years - Minnesota's
electric power sector consumed around four times more natural gas in 2024 than in 2014 (US EPA, 2025). Overall, fossil fuel
sources have the capacity to produce around 750 MW more than renewable sources.
"

pp_by_year <- "Facilities for renewable power around the turn of the 21st century, preceding a boom in their
construction around 2018. New fossil fuel plant construction has remained fairly constant post-1950, with a lull in the 80s and
spikes in the 50s, 70s, and early 2000. This implies that energy needs spiked around 2000, since many new plants of both kinds were built,
as well as increasing eco-consciousness and efforts to shift away from reliance on fossil fuels in more recent years. However, the
location of the remaining fossil-fuel plants, major sources of pollution, has the potential to impact some communities more than others.
"

# EJ

ej_text <- "Research has found power plants are more likely to be built around redlining neighborhoods,
particularly fossil fuel power plants/coal-powered power plants. As a result, already struggling communities
take on additional health burdens. This is particularly true when it comes to health effects, as there have
been relationships found between the implementation of power plants and high levels of bad air quality.
<br>

The Minnesota pollution agencyhas taken on creating environmentally just environments for all,
particularly for those most at risk. These communities are defined as Environmental justice areas.
These areas are census tracts which might fall on one of the following categories:
<br>
<br>
1.) at least 40% of the population is people of color<br>
2.) at least 35 percent of households have income at or below 200 percent of the federal poverty level<br>
3.) at least 40 percent of the population has limited proficiency in English<br>
4.) are located within Indian Country, which is defined as federally recognized reservations and other Indigenous lands<br>
<br>
A concern that arises is if these communities are affected more by power plants than other?"

# Air Quality 

aq_text_1 <- "Electric power plants — especially ones burning fossil fuels such as coal and natural gas — are a major contributor to air pollution and its associated health risks. The EPA states that fossil-fuel fired power plants altogether are the largest stationary source of nitrogen oxides (NOₓ) and sulfur dioxide (SO₂) in the US, and they produce significant quantities of fine particulate matter under 2.5 micrometers in diameter (PM₂.₅), both directly and through reactions of secondary pollutants in the atmosphere (\"Human Health & Environmental Impacts\", 2025). PM2.5 particles are small enough to enter the lungs and bloodstream, potentially accumulating in the respiratory system. In addition to serious adverse effects on human health, pollutants emitted by power plants are linked to environmental damage, including loss of biodiversity and climate change (\"Human Health & Environmental Impacts\", 2025). To mitigate this damage, the EPA sets air quality national standards for pollutants and works with state and local governments to meet them. In 2020, the standard for annual average PM2.5 was lowered to 9 µg/m3 from 12 µg/m3, set in 2013, down from 15.0 µg/m3 in 2006 and 50 µg/m3 in 1999 (\"Timeline of Particulate Matter\", 2025)." 

aq_text_2 <- "The EPA provides data from air quality monitors, placed at irregular locations across the state.  The map above visualizes the locations of these air monitors in relation to power plants and EJ areas. Many are in or near EJ areas, meaning that it is possible to more accurately apply our analysis to these areas. This placement also offers confidence that the government has access to information about air quality in the most marginalized areas and is able to consider these conditions in creating policy. In the Twin Cities, for instance, there is a higher concentration of power plants, EJ areas, and air monitors than elsewhere in the state. However, there are large clusters of EJ tracts in the northern half of the state that have no nearby air monitors. In the following plots, we compare the readings of air monitors that have nearby power plants, visualized with increasingly dark blue buffers, to those that do not, with white buffers. "

aq_text_3 <- "Monitors which are within a three-mile radius of at least one power plant, and thus are most likely to pick up on the effects of plants' pollution, have consistently higher PM2.5 concentrations than monitors which are not. 
<br>
<br>
Some of this effect could be due to the fact that air monitors tend to be located in centers of population, like the Twin Cities, which experience higher pollution levels due to factors like vehicle traffic as well as power plants. It is also important to note that some categories have limited data: while 12+ monitors every year have no fossil-fired plants nearby, only a handful have one or more nearby. 
<br>
<br>
Still, this plot suggests that air quality in areas near power plants is consistently worse than other areas. Communities near power plants are experiencing higher ambient PM2.5 concentrations, and the power plants are certainly not decreasing that environmental burden. 
"
aq_text_4 <- "
Zooming out from a three-mile radius, we see a similar trend: air quality monitors in counties with no fossil fuel plants tend to report lower PM2.5 concentrations than counties with fossil fuel plant(s). After 2015, though, this pattern appears to shift. Two fossil-fuel counties even drop below 2.5 µg/m3, giving the impression that renewable-only counties have worse air quality overall than fossil-fuel counties. However, the air monitors in these counties only have observations starting in 2015. They are located in the northeast tip of the state, around Lake Superior and the Boundary Waters, where air quality tends to be best. Further, most of the very low, fossil-only counties are relatively large, with air monitors far from plants. Thus, the pattern post-2015 should not be taken as evidence of significant improvement in fossil-fuel counties overall. 
<br>
<br>
During this time period, both plots also show near-universal spikes in PM2.5 concentrations in 2021 and 2023. This is potentially due to Canadian wildfire smoke, a significant cause of ambient PM2.5 (MPCA, n.d.).
<br>
<br>
Yet, both plots do show average annual PM2.5 concentration declining over time, which aligns with research by Jbaily et al. at Harvard T.H. Chan School of Public Health, who found that
<br>" 

aq_text_5 <- "on average across the U.S., PM2.5 concentration levels fell from 2000 to 2016, with average exposure falling from 12.6 μg/m3 to 7.5 μg/m3—a 40.4% drop. They also found that the percentage of the population exposed to PM2.5 levels higher than 12 μg/m3 decreased from 57.3% in 2000 to 4.5% in 2016. (\"Racial, Ethnic Minorities and Low-Income Groups\", 2022)"

aq_text_6 <- "This aligns with decreasing EPS NAAQS standards for permissible PM2.5, and is part of a greater trend of decreasing PM2.5 levels nationally, potentially indicating the success of EPA programs for pollution control (PM2.5 national trend). It also aligns with the spike in new renewable power plants starting around 2000, potentially demonstrating the effectiveness of Minnesota's commitment to improving air quality via less reliance on fossil fuels. By 2040, in fact, electricity providers will be expected to generate or procure 100% of their electricity from carbon-free sources (US EPA, 2025). 
"



# Health 

health_blurb <- "Asthma, the most common chronic disease in the United States, is triggered by irritants such as air pollution.
Class and race are factors that affect the levels of pollutants in the surrounding environment according to the article
“Environmental Justice: The Economics of Race, Place, and Pollution.” by authors Spencer Banzhaf, Lala Ma and Christopher Timmins.
Using the demographics of the neighborhood they studied, it was concluded that facilities may seek out non-white areas with lower income
levels because of the inexpensive land and low wages. This is a result of past instances of red-lining and the government’s involvement
in the concentration of regulations in white areas.  These polluters, such as power plants, release tons of particulate matter into the
surrounding air. Air quality has been monitored for years, showing a steady improvement in air quality over the years."

asthma_plot_one <- "Looking at the plot above, you can see how asthma hospitalization rates are much higher in parts of Northside Minneapolis
and near downtown Saint Paul. In Northside Minneapolis, there are three nonrenewable power facilities: Covanta Hennepin Energy (changed to
Hennepin Energy Recovery Center) and two plants belonging to the University of Minnesota. The two university plants, the Southeast Steam
and CHP plants are low emission plants used for heat and power in university buildings. The Hennepin Energy plant is actually a waste
incinerator that produces a small amount of energy and steam.
<br>
Kim is a life-long Minneapolis resident who recently moved from Brooklyn Park to his home in North Minneapolis with his wife and his four year old son.
He recalls how when he would wash his car in his previous home, it could stay clean for months, whereas now, a black film appears on his car in just days.
He says the HERC is the root of this, and has been polluting the area for years. After his wife became interested in joining a climate control group out
of a local church, Kim became familiarized with the HERC and how the community is affected.
<br>
\"I don't think the community knows enough. I think that's the main problem. It's always going to come back to the community that does not know enough.
They can point out something that burns downtown, and they might not even know that it's burning. They might just think, oh, it's the heating system for
downtown, and that's why it's got the white smoke. And a lot of people are under the impression that the white smoke does, in fact, mean that it's non-pollutant.
But that's not the reality of it.\"
<br>
Kim also worked at a clinic in North Minneapolis for some time and saw a surprising amount of young people with asthma problems. At one point, the Canadian
wildfires were blamed, but he now believes the HERC was a root cause, as he was seeing the issue well before the fires started.
He is concerned about how living near the HERC will impact his young son.
<br>
\"He's gonna wanna go to the park and…and walk on the street, and do all of those things that normal kids should and can't do. And my fear is that the Herc
is not gonna turn off in 20... uh, 2027, 2028. And that he's gonna be playing in the backyard or in the park, running around, taking deep breaths, and
it's just gonna hurt him more. I want... I want to shut [the HERC] down for the next generation, for my kiddo, for my life, I want to be around for him longer.\""


###================================ Text Tab 2  ================================###


pp_data_methods <- "We got our main data about the locations, characteristics, and inital operation dates of all power plants in Minnesota as of 2024
from the US Energy Information Administration (EIA)."

ej_data_methods <- "In order to examine demographics and characteristics of Minnesota
counties, we used <b>American Community Survey (ACS)</b> data, collected by the <b>US Census Bureau,</b> from 2022. We made use of <b>Minnesota
Pollution Control Agency's (MPCA)</b> restructured version of that ACS data to explore tracts considered Environmental
Justice Areas. </b> "

aq_data_methods <-"<b>The Environmental Protection Agency (EPA)</b> provided pre-generated Air Data files of annual summaries of
PM2.5 (fine particulate matter) concentration from around 50 monitors in Minnesota (1999-2025), which we used to evaluate the impacts of power
plants on air quality. I downloaded these via R script from Air Quality System Data Mart available via https://www.epa.gov/outdoor-air-quality-data. Accessed Month DD, YYYY.
"

health_data_methods <- "Finally, in order to explore the human-level impacts of air quality, we used <b>MN Department of Health's
data</b> on hospitalizations due to asthma and COPD.</b>"

data_methods_conclusion <- "To find code and reproduce our work, please see our github repository at https://github.com/lsokolew/ds-456-environmental-policy/tree/main."

acknowledgements <- "We would like to thank Kayla Walsh, Minnesota Environmental Review Board Administrator, for her invaluable insight in guilding the direction of our project. We
sincerely appreciate Minneapolis community members, Kim and Anndrea, for sharing their experiences with us. Thanks to Professor Shilad Sen for his feedback, and Professor Brianna
Heggeseth also for her help."


ai_statement <- "ChatGPT was used to debug code for some plots in the air quality section of this report. 
Additonally, ChatGPT was used to debug animation plot, specifically, the one thing used was transition_manual. 
No generative AI was used in the writing of our analysis."

###================================ OLD TEXT - NOTE DELETE ================================###

context <- "The need for electricity stems from its essential role in daily life,
powering various things from vehicle charging to heating and refrigeration. Due to the vast
amount of demand and use of it,  finding reliable sources of electricity has become crucial.
Power plants are facilities that generate electric energy from various sources, including fossil fuels,
such as coal, natural gas, and petroleum, and renewable sources, like sunlight, water, and wind.
Fossil fuel power plants operate by burning  the chosen fuel to generate heat,
which generates steam or gas in a boiler. This steam or gas spins a turbine, which converts
heat energy into rotational energy, that is then transformed into electricity. However,
this process releases vast amounts of harmful pollutants, such as mercury, greenhouse gasses,
and carbon dioxide. The release of such pollutants contributes to harmful air pollution levels
that harm individuals' health. On the other hand, renewable energy power plants are considered
to be cleaner, as they don’t burn fuel or release greenhouse gases. However, there are still some
drawbacks to individuals, such as the displacement of communities, in order to build these power plants.
Furthermore, research has shown that fossil fuel power plants have been constructed near predominantly black,
hispanic, and asian communities and historical redlined areas. Some examples include Chicago, Los Angeles,
and Philadelphia. Consequently, these communities end up being harmed by the releases of different fuels
and are the ones shouldering the unequal distribution of air quality as a result of these power plants."
