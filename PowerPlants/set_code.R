# Libraries
library(shiny)
library(bslib)

###================================ Colors/Fonts/ETC ================================###

# Colors/Fonts
light_blue <- "#CBE0F7"

my_colors <- c("red", "grey")
values <- c("New Power Plant", "Old Power Plant")


###================================ Data ================================###


###=============Load in data=============###

# power plant
mn_powerplants = read_csv('Data/Power_Plants.csv') %>%
  filter(State == "Minnesota") %>%
  mutate(fossil_fuel = ifelse(PrimSource %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable"),
         PrimSource = ifelse(PrimSource == "other", "waste heat", PrimSource))  %>% # change 'other' to 'waste heat'
  janitor::clean_names() 

# power plant dates
powerplant_dates <- read_csv("Data/powerplant_data_eia_egrid_2024_generator_operable.csv") %>% 
  janitor::clean_names() 
powerplant_dates_retired <- read_csv("Data/powerplant_data_eia_egrid_2024_generator_retired.csv") %>% 
  janitor::clean_names()

# air quality 
mn_aq_all_years <- read_csv("Data/Modeled_PM25_Ozone_MN_county_data_allyears.csv") %>% 
  mutate(county = str_split_i(county, ",", 1),
         county = str_remove(county, "County"),
         county = str_trim(county)
  )

###=============Wrangling =============###

powerplant_dates_mn <- powerplant_dates %>% 
  filter(state == "MN") %>% 
  mutate(full_date = make_date(year = operating_year, month = operating_month, day = 1)) %>% 
  group_by(plant_code) %>% 
  summarise(first_op_date = min(full_date))

# add years to mn_powerplants (main dataset we're using for powerplants)
mn_powerplants <- mn_powerplants %>% left_join(powerplant_dates_mn)

# fill in missing dates:
# Buffalo Sun CSG - active since 2/1/2025
# Oster Sun CSG	- active since 1/1/2025
# Quarry Sun CSG - active since 1/1/2025
# via sunshare website
mn_powerplants <- mn_powerplants %>%
  mutate(first_op_date = case_when(plant_code == 66070 ~ as.Date(mdy("2/1/2025")),
                                   plant_code == 66072 ~ as.Date(mdy("1/1/2025")),
                                   plant_code == 66073 ~ as.Date(mdy("1/1/2025")),
                                   TRUE ~ first_op_date), 
         first_op_year = year(first_op_date)
  ) 

# get date of *last* generator's retirement for a powerplant (disregard dates of generators retired later)
powerplant_dates_retired_mn <- powerplant_dates_retired %>% 
  filter(state == "MN", plant_code != 1912) %>% #inaccurate info given for plant 1912 (it's still operational)
  mutate(full_retirement_date = make_date(year = retirement_year, month = retirement_month, day = 1)) %>% 
  group_by(plant_code) %>% 
  summarise(last_retire_date = max(full_retirement_date, na.rm = TRUE)) # those with missing dates don't appear in mn_powerplants & can be excluded

# join to mn_powerplants
mn_powerplants <- mn_powerplants %>% left_join(powerplant_dates_retired_mn) %>% mutate(last_retire_year = year(last_retire_date))

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
and are the ones shouldering the unequal distribution of air quality as a result of these power plants.
"

data_intro <- "We got our main data about the locations and characteristics of all power plants in Minnesota 
from the US Energy Information Administration (EIA). The Environmental Protection Agency (EPA) had data about 
their dates of operation that we incorporated. In order to examine demographics and characteristics of Minnesota 
counties, we used American Community Survey (ACS) data, collected by the US Census Bureau. We made use of Minnesota
Pollution Control Agency's (MPCA) restructured version of that ACS data to explore tracts considered Environmental 
Justice Areas. The MPCA, using their own monitors and EPA's Fused Air Quality Surfaces Using Downscaling Tool and 
Community Multiscale Air Quality model, also provided air quality data by county. This included data about ozone 
and PM2.5 (fine particulate matter) levels, with modeled estimates for counties without air monitors. Finally, in 
order to explore the human-level impacts of air quality, we used MN Department of Health's data on hospitalizations 
due to asthma and COPD. 
"
