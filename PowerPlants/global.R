# Libraries
library(shiny)
library(bslib)
library(leaflet)

###================================ Load in Data ================================###


mn_powerplants =  read_csv('mn_powerplants.csv') 

zcta_joined =  st_read('zcta_joined.shp') 

AirData_allyears <- readRDS("Data/aq_data_clean/AirData_allyears.rds")


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
  values = c("Fossil Fuel" = "#d95f02", 
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

data_intro <- "We got our main data about the locations, characteristics, and inital operation dates of all power plants in Minnesota as of 2024 
from the US Energy Information Administration (EIA). In order to examine demographics and characteristics of Minnesota 
counties, we used <b>American Community Survey (ACS)</b> data, collected by the <b>US Census Bureau,</b> from [ADD YEAR]. We made use of <b>Minnesota
Pollution Control Agency's (MPCA)</b> restructured version of that ACS data to explore tracts considered Environmental 
Justice Areas.</b> <b>The Environmental Protection Agency (EPA)</b> provided pre-generated Air Data files of annual summaries of 
PM2.5 (fine particulate matter) concentration from around 50 monitors in Minnesota (1980-2025), which we used to evaluate the impacts of power 
plants on air quality. Finally, in order to explore the human-level impacts of air quality, we used <b>MN Department of Health's 
data</b> on hospitalizations due to asthma and COPD.</b>"

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
