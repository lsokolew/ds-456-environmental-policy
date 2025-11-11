# Libraries
library(shiny)
library(bslib)
library(tidycensus)

###================================ Colors/Fonts/ETC ================================###

# Colors/Fonts
light_blue <- "#CBE0F7"

my_colors <- c("red", "grey")
values <- c("New Power Plant", "Old Power Plant")

theme_1 <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                 panel.background = element_blank(),     
                 plot.background = element_blank(),     
                 panel.grid = element_blank(),
                 legend.background = element_rect(fill = "transparent", color = NA) )

fuel_colors <- scale_fill_manual(
  values = c("Fossil Fuel" = "#A39081", 
             "Renewable" = "#1C693A"))


# colors for ej areas
pal1 <- colorFactor(
  palette = c("#dc0073","#22577a", "#c44900", "#f4e285"),
  domain = c( "ALL", "ONLY LOW INCOME", "ONLY POC", "ONLY POC & LOW INCOME"))

# health palete
pal3 <- colorFactor(
  palette = c("red", "green"),
  domain = mn_powerplants$fossil_fuel
)
###================================ Data ================================###


###==================== Load in data ===================###

###=== Needed Overall ===###

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

###=== Air Quality ===###

# air quality 
mn_aq_all_years <- read_csv("Data/Modeled_PM25_Ozone_MN_county_data_allyears.csv") %>% 
  mutate(county = str_split_i(county, ",", 1),
         county = str_remove(county, "County"),
         county = str_trim(county))

# load spatial/boundary info
mn_counties <- counties(state = "MN", cb = TRUE) %>%
  st_transform(crs = 4326)

mn_tracts <- tracts(state = "MN", cb = TRUE) %>%
  st_transform(crs = 4326)

# join AQ data to add spatial data
mn_counties_aq <- mn_counties %>%
  left_join(mn_aq_all_years, by = c("NAME" = "county")) %>% 
  janitor::clean_names()

###=== Healthcare ===###

# Asthma
asthmaMN <- read_csv("Data/MN-asthma-zipcode.csv")
 
# Zip codes
mn_zctas <- readRDS("Data/mn_zctas_2020.rds")

###=== EJ Areas ===###

# Environmental justice areas (subseted)
ej_spaces <- read_csv("Data/ej_mpca_census.csv") %>%
  select(-Shape_Area, -Shape_Length, -source, -statefp, -funcstat, -name, 
         -namelsad, -mtfcc, -intptlat, -intptlon, -geography, -countyfp, 
         -aland, -awater)

# tracts
mn_tracts <- tracts(state = "MN", cb = TRUE, year = 2023) %>%
  st_transform(crs = 4326)%>%
  mutate(GEOID = as.double(GEOID),
         County = gsub(" County", "", NAMELSADCO))  %>%
  select(GEOID, geometry, County) 
 
###==================== Wrangling ====================###

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

###=== Asthma ===###

# Asthma Data and ZCTAs

zcta_joined <- asthmaMN %>%
  left_join(mn_zctas, by = c("_ZIP" = "GEOID20")) %>%
  mutate(`Age-adjusted rate per 10,000` = na_if(`Age-adjusted rate per 10,000`, "*"),
         `Age-adjusted rate per 10,000` = as.numeric(`Age-adjusted rate per 10,000`)) %>%
  mutate(value_cat = case_when(
    `Age-adjusted rate per 10,000` >= 0 & `Age-adjusted rate per 10,000` <= 2 ~ "0-2",
    `Age-adjusted rate per 10,000` >= 2 & `Age-adjusted rate per 10,000` <= 4 ~ "2-4",
    `Age-adjusted rate per 10,000` >= 4 & `Age-adjusted rate per 10,000` <= 7 ~ "4-7",
    `Age-adjusted rate per 10,000` >= 7 ~ "7+"))

zcta_joined <- st_as_sf(zcta_joined)


###=== EJ Areas ===###

Power_Plants <- read_csv("Data/Power_Plants.csv")  %>%
  filter(State == "Minnesota") %>%
  mutate(fossil_fuel = ifelse(PrimSource %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable"))

# Join Environmental Justice Data
ej_tracts <- mn_tracts %>%
  left_join(ej_spaces, by = c("GEOID" = "geoid"))

# Subseting to find each type of case of enviromental justice areas

# 40% or more of estimate population with limited English proficiency, based on maxprplep (calculated) &
# Max estimated population that identify as people of color is 50% or more, based on maxprppoc (calculated)	&
# 35% or more of estimate population under 200% of the federal poverty level, based on prp200x (calculated)
statuselp_filtered <- ej_spaces %>%
  filter(statuslep == "YES") %>%
  left_join(mn_tracts, by = c("geoid" = "GEOID")) %>%
  mutate(EJ_area = "ALL")

# ONLY 35% or more of estimate population under 200% of the federal poverty level, based on prp200x (calculated)
status200x_filtered <- ej_spaces %>%
  filter(status200x == "YES" & statuspoc == "NO" & statuslep == "NO")%>%
  left_join(mn_tracts, by = c("geoid" = "GEOID")) %>%
  mutate(EJ_area = "ONLY LOW INCOME")

# ONLY Max estimated population that identify as people of color is 50% or more, based on maxprppoc (calculated)	&
statuspoc_filtered <- ej_spaces %>%
  filter(statuspoc == "YES" & status200x == "NO"  & statuslep == "NO")%>%
  left_join(mn_tracts, by = c("geoid" = "GEOID"))  %>%
  mutate(EJ_area = "ONLY POC")

# BOTH  Max estimated population that identify as people of color is 50% or more, based on maxprppoc (calculated)	&
# 35% or more of estimate population under 200% of the federal poverty level, based on prp200x (calculated)
low_income_poc_areas <- ej_spaces %>%
  filter(statuspoc == "YES" & status200x == "YES" & statuslep == "NO") %>%
  left_join(mn_tracts, by = c("geoid" = "GEOID")) %>%
  mutate(EJ_area = "ONLY POC & LOW INCOME")

# bring it all together
EJ_stacked <- bind_rows(statuselp_filtered, status200x_filtered,
                        statuspoc_filtered, low_income_poc_areas) %>%
  mutate(EJ_OR_NOT = TRUE)


# Subset fossil fuel power plants
fossil_power_plants <- Power_Plants %>%
  filter(fossil_fuel == "Fossil Fuel", County %in% EJ_stacked$County)

# Subset Reneable power plants
Renewable_power_plants <- Power_Plants %>%
  filter(fossil_fuel == "Renewable", County %in% EJ_stacked$County)

# transform to sf
ej_sf <- st_as_sf(EJ_stacked)


# Loading in census data
mn_tracts <- tracts(state = "MN", year = 2020, class = "sf")
Power_Plants_sf <- st_as_sf(Power_Plants, coords = c("Longitude", "Latitude"), crs = 4326)

mn_tracts <- st_transform(mn_tracts, crs = st_crs(Power_Plants_sf))

# Bringing it together
Power_Plants_with_tract <- st_join(Power_Plants_sf, mn_tracts[, c("GEOID", "NAME", "COUNTYFP")], join = st_within)

# Add a column to identify is power plant is part of EJ or Not also simplify df
plants_in_ej <- st_join(Power_Plants_with_tract, ej_sf, join = st_within) %>%
  mutate(EJ_OR_NOT = if_else(is.na(EJ_OR_NOT), FALSE, EJ_OR_NOT)) %>%
  select(X, Y, OBJECTID.x, Plant_Code, Plant_Name, County.x, Zip, PrimSource, 
         fossil_fuel, geometry, GEOID, NAME, COUNTYFP,EJ_OR_NOT, EJ_area)

# Find the count of power plants per census tracts
plants_in_ej_counts <- plants_in_ej %>%
  group_by(GEOID, fossil_fuel, EJ_OR_NOT) %>%
  summarize(plant_count = n())  

# Getting population data
total_pop <- get_acs(geography = "tract", variables = "B01003_001", state = "MN", year = 2023, geometry = FALSE)

# Joining power plant + ej areas with population data
Power_Plants_with_pop <- plants_in_ej %>%
  left_join(total_pop %>% select(GEOID, estimate), by = "GEOID") %>%
  rename(total_population = estimate)


# power plant and population 
plants_per_pop <- Power_Plants_with_pop %>%
  group_by(GEOID, fossil_fuel, EJ_OR_NOT) %>%
  summarize(plant_count = n(), total_population = first(total_population), 
            plants_per_10k = (plant_count / total_population) * 10000)

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

health_blurb <- "Asthma, the most common chronic disease in the United States, is triggered by irritants such as air pollution. 
Class and race are factors that affect the levels of pollutants in the surrounding environment according to the article 
“Environmental Justice: The Economics of Race, Place, and Pollution.” by authors Spencer Banzhaf, Lala Ma and Christopher Timmins. 
Using the demographics of the neighborhood they studied, it was concluded that facilities may seek out non-white areas with lower income 
levels because of the inexpensive land and low wages. This is a result of past instances of red-lining and the government’s involvement 
in the concentration of regulations in white areas.  These polluters, such as power plants, release tons of particulate matter into the 
surrounding air. Air quality has been monitored for years, showing a steady improvement in air quality over the years."


ej_areas <- "context"
