# Libraries
library(shiny)
library(bslib)
library(leaflet)
library(tidyverse)
library(sf)
library(gifski)  
###================================ Load in Data ================================###


mn_powerplants              <- read_csv('Data/cleaning_data/mn_powerplants.csv') 
zcta_joined_asthma          <- st_read('Data/cleaning_data/zcta_joined_asthma.shp') 
AirData_allyears            <- readRDS("Data/aq_data_clean/AirData_allyears.rds")
schools_sf                  <- sf::read_sf("Data/shp_struc_school_program_locs/school_program_locations.shp")
mn_tracts                   <- st_read("Data/cleaning_data/mn_tracts.shp")
ej_sf                       <- st_read("Data/cleaning_data/ej_sf.shp")
asthma_poc_powerplant       <- read_csv("Data/cleaning_data/asthma_poc_powerplants.csv")
distinct_schools_metro      <- read_csv("Data/cleaning_data/distinct_schools_metro.csv")
ej_shp                      <- st_read("Data/ej_mpca/ej_mpca_census.shp")
metro_area                  <- st_read("Data/cleaning_data/metro_area_pp.shp")
powerplants                 <- st_read("Data/cleaning_data/powerplants_sf.shp")
tribal_shp_wgs              <- st_read("Data/tribal_areas/census_tribal_areas.shp")
tribal_shp_wgs              <- st_read("Data/tribal_areas/census_tribal_areas.shp")
pp_summary                  <- st_read("Data/pp_summary.shp")

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

intro <- "Without fail, North Minneapolis resident Kim M’s car is covered by a layer of black film just days after a wash. When he lived in Brooklyn Park, 
his car could go months without needing a carwash, so he knows this is a community issue. After learning about the HERC during a local climate activism group meeting, 
the black film, the kids he’d see with respiratory issues, and even the smell in the air all made sense. 
<br>
<br>
The HERC, or the Hennepin Energy Recovery Center, is a waste to energy facility located in the North Loop neighborhood. Since its opening in 1989, it has processed a 
thousand tons of waste per day. Back then, it was the solution to calls for the redirection of trash from landfills to waste incinerators, but its presence in the
community has always been heavily debated. Environmental justice is the idea that everyone has the right to a safe and healthy environment free of hazardous waste. The HERC 
is considered by many to be in violation of this idea, and the fight to shut it down has spanned many years. 

<br>
<br>
In this analysis, we consider power plants to be facilities with a capacity of 1 megawatt or larger. The HERC has a capacity of 40MW. We have two questions: 
<strong>how do power plants align with Minnesota’s commitment to environmental justice and how do power plants affect the surrounding community?</strong>
<br>
<br>
2020 marked a shift in Minnesota's electricity generation - for the first time, coal-fired power plants contributed less energy than renewable resources (EIA profile analysis). The state is shifting away from fossil fuels. 
\"Minnesota's mandatory renewable energy standard, initially enacted in 2007, requires that the state's electricity providers generate or procure at least 25% of their electricity retail sales from eligible renewable sources
by 2025. In 2023, Minnesota's legislature raised the standard, requiring utilities to obtain 80% carbon-free electricity by 2030, 90% by 2035, and 100% by 2040. Eligible carbon-free generating fuels include solar energy, 
hydropower, wind energy, biomass, and nuclear energy\" (EIA profile analysis). Through these new standards, the state is trying to minimize the amount of plants opening up and limit emissions. With studies showing that 
fossil fuel plants are often concentrated in neighborhoods with large populations of color, it will be interesting to see how different communities in the area are affected by this new policy. 
<br>
<br>
Anndrea is a community organizer who grew up in ward 5 of North Minneapolis– right near the HERC. She first became aware of the HERC and other large polluters during a town hall meeting and ever since
then has been pressuring city officials to shut them down. Although the county is responsible for abiding to these new standards, Anndrea feels that the lack of acknowledgement from city
officials is directly harming vulnerable communities by preventing education and engagement, which makes it hard for locals to take action. “Once the people who are mostly impacted become more involved, 
I think that that is when the revolution will actually begin.”
"


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

ej_text <- "The Minnesota Pollution Control Agency (MPCA) is dedicated to addressing the 
disproportionate bearing of negative environmental consequences. Particularly
when it comes to the state of Minnesota, it has been found that though only 36% 
of communities have air-pollution related risk above health guidance, in low income 
areas it is 53% of communities, in communities of color it is 78%. Thus it has become 
important to put an emphasis on developing strategies to reduce the disparities on these 
communities most at risk. 
<br>

These MPCA defines Enviromental Justice areas as 
census tracts that meet at least one of the following criteria:
<br>
<br>
1.) at least 40% of the population is people of color<br>
2.) at least 35 percent of households have income at or below 200 percent of the federal poverty level<br>
3.) at least 40 percent of the population has limited proficiency in English<br>
4.) are located within Indian Country, which is defined as federally recognized reservations and other Indigenous lands<br>
<br>
When mapping out these areas,
we find that throughout the state of Minnesota, there are more low-income environmental 
justice areas than EJ areas defined by people of color. Additionally, many of the reservations 
and indigenous lands seem to be more in the Northern part of the state, where there are significantly 
fewer power plants.  However, when zooming into the Twin Cities metropolitan area, we find way more 
environmental justice areas, particularly in clusters. "


ej_text_2<-"When mapping out power plants, we notice that there are 
more renewable energy power plants on the outskirts of the metro area,
in comparison to the concentration of fossil fuel power plants in the metro area. 
Having said this, the power plants do seem well distributed, though there are 
significantly more renewable energy plants. For example, there are almost no 
fossil fuel power plants in the nearby Dulutuh area, but quite a few renewable energy plants in Dulutuh."


ej_text_3<-"Research has found that fossil fuel power plants, which emit more harmful pollutants than 
renewable sources,  aredisproportionately located near black, brown and indigenous, and poor communities 
(Donaghy2023fossilFuelRacism).  Some of this is a result of redlining that still linger today and disproportionately affect air 
quality for individual reading in these historic communities(HarvardChan2023redlining). Additionally, health harms have been 
linked to result from waste and power facilities, particularly for residents living within a 1-mile 
radius(Vrijheid, 2009)."


ej_text_4 <- "To assess local impacts, we explored the proportion of EJ census tracts within a 
1-mile radius of each fossil fuel power plant. Statewide, most power plants do not overlap with 
EJ areas. When focusing on the Twin Cities  Metro Area (Anoka, Carver, Dakota, Ramsey, Hennepin,
Scott, and Washington Counties), 17 of out 28 power plants do not touch an EJ tract within a 1-mile radius."

ej_text_5 <-"Though these findings demonstrate that in Minnesota powerplants aren’t heavily in EJ areas, 
it is still important to highlight that there do exist fossil fuel power plants that heavily overlap EJ areas. 
These facilities are particularly ones we should worry abot, as residents in these areas continue to be vulnerable 
to evneormental and health risks."

ej_plot_text_1<- "<i>Note: the purple shading represents indigenous/tribal lands</i>"

ej_plot_text_2<- "<i>Note: You can move the plot left to right, the orange dots represent a powerplant and each of it's one mile radius. If the plot 
is showing brown filled census tracts, you are viewing the fossil fuel power plants. If you see green census tracts, you are viewing the renewable energy
power plants. The filled census tracts (brown or green) represent EJ Areas. </i>"


# Air Quality 

aq_text_1 <- "Electric power plants — especially ones burning fossil fuels such as coal and natural gas — are a major contributor to air pollution and its associated health risks. The EPA states that fossil-fuel fired power plants altogether are the largest stationary source of nitrogen oxides (NOₓ) and sulfur dioxide (SO₂) in the US, and they produce significant quantities of fine particulate matter under 2.5 micrometers in diameter (PM2.5), both directly and through reactions of secondary pollutants in the atmosphere (\"Human Health & Environmental Impacts\", 2025). PM2.5 particles are small enough to enter the lungs and bloodstream, potentially accumulating in the respiratory system. In addition to serious adverse effects on human health, pollutants emitted by power plants are linked to environmental damage, including loss of biodiversity and climate change (\"Human Health & Environmental Impacts\", 2025). To mitigate this damage, the EPA sets air quality national standards for pollutants and works with state and local governments to meet them. In 2020, the standard for annual average PM2.5 was lowered to 9 µg/m3 from 12 µg/m3, set in 2013, down from 15.0 µg/m3 in 2006 and 50 µg/m3 in 1999 (\"Timeline of Particulate Matter\", 2025)." 

aq_text_2 <- "The EPA provides data from air quality monitors, placed at irregular locations across the state.  The map above visualizes the locations of these air monitors in relation to power plants and EJ areas. Many are in or near EJ areas, meaning that it is possible to more accurately apply our analysis to these areas. This placement also offers confidence that the government has access to information about air quality in the most marginalized areas and is able to consider these conditions in creating policy. In the Twin Cities, for instance, there is a higher concentration of power plants, EJ areas, and air monitors than elsewhere in the state. However, there are large clusters of EJ tracts in the northern half of the state that have no nearby air monitors. In the following plots, we compare the readings of air monitors that have nearby power plants, visualized above with increasingly dark blue buffers, to those that do not, with white buffers. "

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

aq_text_7 <- "Yet, plants are still causing damage in the meantime. One study estimates that 'between 1999 and 2020, 460,000 deaths would not have occurred in the absence of emission from the coal power plants' (Doctrow, 2023). Thus, communities near coal power plants are at risk of significant health concerns and even increased mortality. But what do health effects of exposure to pollutants like PM2.5 look like in the Twin Cities?"

# Health 

health_blurb <- "As a kid growing up in Ward 5 of Minneapolis, Anndrea was never formally diagnosed with Asthma, but she had an inhaler 
and had to undergo nebulizer treatments. When Covid hit, she decided to return home from Nashville where she had attended university. “I 
know that as an adult coming back into Minneapolis, I would notice and I've mentioned like when you blow your nose and sometimes it's like kind of dirty in your nose”
<br>
<br>
The Environmental Protection Agency recognizes that nonrenewable energy plants are the largest stationary source of nitrogen dioxides and sulfur dioxide emissions, 
while also being a large source of particulate matter. Nitrogen dioxides form ground level ozone and fine particulate matter, which are known to cause respiratory problems 
such as asthma. Data released from the Minnesota Pollution Control Agency shows that in 2019, the HERC was Hennepin county’s number one emitter of NO2 and second in the county 
for sulfur dioxide and PM2.5. These pollutants are known to worsen respiratory issues such as asthma and evidence shows that it may cause asthma in both children and adults. 
<br>
<br>
Kim mentioned running into a lot of asthma patients while working at a clinic for marginalized community members in North Minneapolis. Children and young adults were coming
in with asthma-related health issues, yet the Canadian wildfires were blamed, even though these issues long preceded the wildfires. 

"

asthma_plot_one <- "The map of the Twin Cities Metro Area visualizes where higher rates of emergency department visits occur and points out where power plants with the largest 
emissions are located. Each power plant has a radius of 1-mile, which studies have revealed people in radius are most affected. There does seem to be a huge cluster of power plants in 
North Minneapolis on both sides of the Mississippi, and around downtown Saint Paul. The HERC lies in zip code 55405, which has a rate of 36 emergency department visits per 10,000 people. 
In the adjacent zip codes within one mile,  55403 has 60 and 55411 has 141. For comparison, zip code 55416, which is around 2 miles away, has 11. But could there be a further look into what 
communities are hit the hardest by these plants?"

asthma_plot_two <- "In metro area zip codes not within and within a one mile radius of at least one power plant, there is a trend in asthma rates as the proportion of people of color increases. 
However, in zip codes that are within one mile of a power plant, there is a much higher asthma rate and an uptick in the amount of zip codes highly populated by people of color near power plants. 
This reflects an EPA study on the effects of low level pollution on African American children with asthma and how levels below national standards still had disproportionately detrimental effects.
Kids are outside more than adults and their respiratory systems are still developing. A study jointly conducted by the EPA and Johns Hopkin shows that children exposed to outdoor PM2.5-10 are more 
likely to develop asthma and need to be hospitalized. Since air quality monitors are reported elevated levels of PM2.5 around power plants, children in the area are at risk
"

school_plot <-"Using data on locations of schools in Minnesota, power plants, and environmental justice tracts, on average, schools within designated environmental justice tracts are slightly closer to power plants. 
However, taking a closer look at the Twin Cities metro area, schools in environmental justice tracts are on average two times closer to power plants. 
<br>
<br>
Kim fears for his son. \"My kiddo has Down syndrome, he's 4 years old. And he already has a lot of things against him. We are hopeful that a respiratory problem isn't something that 
catches up to him, we make sure that he...Well, he doesn't get a lot of outside time because of this, too. He plays mostly in the house. But as he gets older, he's gonna wanna play outside.”
"


# HERC

HERC_text_1 <- "The Hennepin Energy Recovery Center(HERC) lies right up North of Target Field in Minneapolis, an area where many frequent and live nearby. 
When investigating the HERC, we identified 14 census tracts within a 1-mile radius, with a combined population of 45,254 residents.
While the census tract one which the HERC lies on is not classified as an Envriomental Justtice area, 5 surrounding tracts are."

HERC_plot_1_text<- "<i>The orange donut represents the Hennepin Recovery Center, and the orange layer around it represents the 1-mile radius.
Moreover, the purple shades represent the proportion of People of Color per census tract.</i>"

HERC_text_2 <-"Within the 1-mile radius, 43.66% of the residents are people of color, and 30.48% live below or at federal poverty level. Additionally, 2 
of these census tracts are each composed of over 88% people of color. These values reinforce many community concerts about the HERC impacting many people of color.
Moreover, we do find that Monitors near the HERC track higher PM2.5 concentrations than those not nearby (Plotted Below). From this we can grasp that
that those living nearby to the HERC are burdened with common negative effects caused by Fossil Fuel Power Plants."


HERC_text_3 <- "Although our overall analysis suggests that the distribution of power plants are not consitenly located in environmental justice areas, 
it is important to highlight the additional burdend placed on communities that do have them. The HERC, for example, is among the top 5 polluters in Hennepin
County (Minneapolis, MN), alongside Xcel Energy Riverside Generating Plant, Tiller Corp asphalt processing facility, NRG Energy Center Minneapolis, and the 
University of Minnesota, Twin Cities. Its contitribution to local carbon dioxide emission, adds on to the pre-existing environmental and health challenges 
already by many individuals in the area. Thus it is still crucial to continue efforts in finding solutions to reduce the number of polluters in such vulnerable 
areas."

# Conclusion

conclusion_text_1 <- "Overall, our findings found that communities near fossil-fired power plants tend to experience higher levels of PM2.5. 
Such high levels of this pollutant have been associated wiith negative outcomes on health, particulary increased asthma attacks.

<br><br>

While we find Minnesota's proactive policies for environmental justice have generally been successful at limiting the 
placement of power plants in vulnerable areas, communities located near these facilities still bear the additional health and evieomental burdens. 
Morover, residents, such as those near the HERC, have expressed their lack of confidence that local goverments will follow through on plans to shut 
fossil-fired plants down.

<br><br>

This analysis does have some limitations. For instance, there is limited air quality data, unaccounted confounding variables 
(e.g.,traffic patterns), and inability to consider plant size in the area or capacity. Moreover, though we do consider Envireomtnal Justice Areas, 
as labeled by the Pollution Control Agency, the data used for such classicaltion comes from 2018-2022, as in it is not recent data. We also worked 
on the HERC, which is a powerplant that is in the works of being shut down, but did not consider past fossil fuel power plant shutdowns. Additionally, 
although we cite EPA and other US government resources throughout this report, it's important to note that some of these resources have recently been 
undergoing politically motivated changes, which could impact the quality of outside context we provide."

conclusion_text_2 <- "<i>The potential for small-scale (less than 1 megawatt) wind installations exists across the state and Minnesota is among 
the five states with the greatest potential for small-scale, customer-sited residential wind installations</i>"

conclusion_text_3 <- "Residents can support efforts to close down harmful plants through organiztions like the Zero Waste Coalition, 
help inform communities about the effects of large polluters, and critically evaluate claims made about power plants.  
There has been misleading data found when evaluating such plants, for instance some data has been presented that minimized the harms of 
facilities like the HERC . All in all, there is much still that can be done to help reduce the envieormental and impacts faced by affected communities."

###================================ Text Tab 2  ================================###


pp_data_methods <- "We got our main dataset about the locations and characteristics of power plants in Minnesota as of 2024
from the Environmental Protection Agency (EPA), although its original source is the US Energy Information Administration (EIA). The initial dataset contained all power plants in the US,
so we filtered it down to just Minesota. It contained the plant's geographical coordinates, name, city & county, primary fuel source, and a uniquely-identifying 
plant code. We also consider the total capacity of the plant, or the most electricity (in MW) it can be producing at any time, from this dataset.
We grouped plants into fossil fuel or renewable, based on their primary source of fuel, and manually updated the one 'other' fuel plant to 'waste heat' based on information
we found online. We also re-classified the Covanta plant (The HERC) as fossil-fuel based on Minnesota legislation marking it non-renewable. 
<br>
<br>
Data from EIA Form-860 was downloaded directly to supply the initial dates of operation for the power plants in our main dataset. These dates range from 1906
to 2024, with a typical value around 2010. Missing data, from plants that began operation in the first two months of 2025, was supplied manually from information via the plants' utility's 
website (![https://mysunshare.com/about-us/]). Where there were multiple generators with different start dates, we took the first as the overall
start date of the plant. We joined this to the main power plants datset using the shared plant ID. 
"

ej_data_methods <- "In order to examine demographics and characteristics of Minnesota
counties, we used <b>American Community Survey (ACS)</b> data, collected by the <b>US Census Bureau,</b> from 2022. We made use of <b>Minnesota
Pollution Control Agency's (MPCA)</b> restructured version of that ACS data to explore tracts considered Environmental
Justice Areas. </b> "


aq_data_methods <-"The EPA provided pre-generated Air Data files of annual summaries of
PM2.5 (fine particulate matter) concentration from around 50 total monitors in Minnesota (1999-2025), which we used to explore the impacts of power
plants on air quality. It contains annual mean PM2.5 readings from various sites around the state, some of which have multiple monitors, and provides the locations
of the sites and the metric used to calculate annual mean PM2.5 (eg, 1- or 24-hour averages). It also notes the local site name, county name, and city name,
if applicable. We downloaded these via R script from Air Quality System Data 
Mart available via https://www.epa.gov/outdoor-air-quality-data, on November 21st, 2025. We filter the observations to just 
Minnesota and 24-hour average PM2.5 readings. If there are multiple monitors at a site, we take the average, so that there is at most one reading per site
per year.  This average annual mean PM2.5 ranges from 1 to 15  μg/m3, with a typical value around 7.5  μg/m3.
<br>
<br>
Then, to find the number of power plants near a monitor, we create a three-mile buffer around the monitors and check which power plants fall within that buffer,
and save that count. We then calculate the average annual mean PM2.5 concentrations for monitors near 0, 1, or 2+ power plants. "

health_data_methods <- "Finally, in order to explore the human-level impacts of air quality, we used <b>MN Department of Health's
data</b> on hospitalizations due to asthma.</b> Asthma-related emergency department visits were summarized by each zip code in the Twin Cities metropolitan area. The data was
summarized based rates per 10,000 people, split into age groups of 0-17,  18+,, and all ages. This data was joined with data from the <b>American Community Survey</b> to get the 
percent of people of color in the area as well as the geometry of each zip code for mapping purposes. One limitation of this is the data is modeled to predict the amount of people in 
each zip code and is not a direct headcount."

data_methods_conclusion <- "To find code and reproduce our work, please see our github repository at https://github.com/lsokolew/ds-456-environmental-policy/tree/main."

acknowledgements <- "We would like to thank Kayla Walsh, Minnesota Environmental Review Board Administrator, for her invaluable insight in guilding the direction of our project. We
sincerely appreciate Minneapolis community members, Kim and Anndrea, for sharing their experiences with us. Thanks to Professor Shilad Sen for his feedback, and Professor Brianna
Heggeseth also for her help."


ai_statement <- "ChatGPT was used to debug code for some plots in the air quality section of this report. 
Additonally, ChatGPT was used to debug animation plot, specifically, the one thing used was transition_manual. It also
was used to attempt to realign the legends across the leaflet plots, but didn't really work. Instead, users on stack over flow and other
platforms explained the issue (sadly no solution seemed to work). No generative AI was used in the writing of our analysis."
