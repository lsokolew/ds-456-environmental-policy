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
  mutate(fossil_fuel = ifelse(PrimSource %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable")) %>%
  rename(plant_code = Plant_Code)

# historical data
powerplant_dates <- read_csv("Data/powerplant_data_eia_egrid_2024_generator_operable.csv") %>% 
  janitor::clean_names() 


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
