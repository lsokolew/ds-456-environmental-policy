# Libraries
library(shiny)
library(bslib)

###================================ Load in Data ================================###


mn_powerplants =  read_csv('mn_powerplants.csv') 

zcta_joined =  st_read('zcta_joined.shp') 
# mn_counties_aq =  st_read('mn_counties_aq.shp') 

###================================ Colors/Fonts/ETC ================================###



# Colors/Fonts
light_blue <- "#CBE0F7"

my_colors <- c("red", "grey")
values <- c("New Power Plant", "Old Power Plant")

theme_1 <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                 panel.background = element_blank(),     
                 plot.background = element_blank(),     
                 panel.grid = element_blank(),
                 strip.background = element_blank(),
                 legend.background = element_rect(fill = "transparent", color = NA) )

fuel_colors <- scale_fill_manual(
  values = c("Fossil Fuel" = "#A39081", 
             "Renewable" = "#1C693A"))


# colors for ej areas
pal1 <- colorFactor(
  palette = c("#dc0073", "#22577a", "#c44900", "#f4e285"),
  domain = c("ALL", "ONLY LOW INCOME", "ONLY POC", "ONLY POC & LOW INCOME")
)

# health palete
pal3 <- colorFactor(
  palette = c("red", "green"),
  domain = mn_powerplants$fossil_fuel
)


###================================ Text ================================###

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

data_intro <- "We got our main data about the locations and characteristics of all power plants in Minnesota 
from the US Energy Information Administration (EIA).  <b>The Environmental Protection Agency (EPA)</b> had data about 
their dates of operation that we incorporated. In order to examine demographics and characteristics of Minnesota 
counties, we used <b>American Community Survey (ACS)</b> data, collected by the <b>US Census Bureau.</b> We made use of <b>Minnesota
Pollution Control Agency's (MPCA)</b> restructured version of that ACS data to explore tracts considered Environmental 
Justice Areas.</b> The MPCA, using their own monitors and EPA's Fused Air Quality Surfaces Using Downscaling Tool and 
Community Multiscale Air Quality model, also provided air quality data by county. This included data about ozone 
and PM2.5 (fine particulate matter) levels, with modeled estimates for counties without air monitors. Finally, in 
order to explore the human-level impacts of air quality, we used <b>MN Department of Health's data</b> on hospitalizations 
due to asthma and COPD.</b>"

aq_blurb <- "Electric power plants — especially ones burning fossil fuels such as coal and natural gas — are a major contributor 
to air pollution and its associated health risks. The EPA states that fossil-fuel fired power plants are the largest stationary 
source of nitrogen oxides (NOₓ) and sulfur dioxide (SO₂) in the US, and they emit significant quantities of fine particulate matter
(PM₂.₅). These pollutants contribute to environmental damage, including acid rain, loss of biodiversity, and climate change. (Source: 
https://www.epa.gov/power-sector/human-health-environmental-impacts-electric-power-sector)"

interactive_aq_plot_descrip <- "Until 2015, PM₂.₅ stays most concentrated in the Twin Cities Area, with generally higher levels in  
southern counties than northern ones. Southern counties also contain the most fossil fuel power plants; they are much sparser in the north.
In 2016, we see generally better air quality levels across the state, as well as an uptick in the additions of new renewable power plants.
In 2021, there is a spike in PM₂.₅ levels in the northwestern counties."

aq_line_plot_descrip <- "These plots show the change in average PM₂.₅ and ozone levels between 2008 and 2021, with each line representing
one county. Lines are colored red if a county has at least one fossil fuel power plant, or green if a county has only fossil fuel or no power 
plants."

health_blurb <- "Asthma, the most common chronic disease in the United States, is triggered by irritants such as air pollution. 
Class and race are factors that affect the levels of pollutants in the surrounding environment according to the article 
“Environmental Justice: The Economics of Race, Place, and Pollution.” by authors Spencer Banzhaf, Lala Ma and Christopher Timmins. 
Using the demographics of the neighborhood they studied, it was concluded that facilities may seek out non-white areas with lower income 
levels because of the inexpensive land and low wages. This is a result of past instances of red-lining and the government’s involvement 
in the concentration of regulations in white areas.  These polluters, such as power plants, release tons of particulate matter into the 
surrounding air. Air quality has been monitored for years, showing a steady improvement in air quality over the years."


ej_areas <- "Research has found power plants are more likely to be built around redlining neighborhoods, 
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
