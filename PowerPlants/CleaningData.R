# Load Libraries
library(tidycensus)
library(dplyr)
library(tidyverse)
library(sf)


###==================== Load in data ===================###

###=== Needed Overall ===###

# power plant
mn_powerplants = read_csv('Data/Power_Plants.csv') %>%
  filter(State == "Minnesota") %>%
  mutate(fossil_fuel = ifelse(PrimSource %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable"),
         PrimSource = ifelse(PrimSource == "other", "waste heat", PrimSource))  %>% # change 'other' to 'waste heat'
  janitor::clean_names() 

# power plant dates
powerplant_dates <- read_csv("Data/powerplant_data_eia_2024_generator_operable.csv") %>% 
  janitor::clean_names() 

###=== Air Quality ===###

# air quality 
AirData_allyears <- read_csv("Data/airdata_clean.csv") 

# load spatial/boundary info
# mn_counties <- counties(state = "MN", cb = TRUE) %>%
#   st_transform(crs = 4326)

# mn_tracts <- tracts(state = "MN", cb = TRUE) %>%
#   st_transform(crs = 4326)

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


tribal_shp <- st_read("Data/tribal_areas/census_tribal_areas.shp")


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

###=== Air Quality ===###

AirData_allyears <- AirData_allyears %>%
  filter(pollutant_standard == "PM25 24-hour 2012") %>% # 2012 is the most recent standard with the most observations; it doesn't change the numbers
  group_by(site_num, parameter_code, pollutant_standard, year, longitude, latitude, local_site_name, county_name, city_name, cbsa_name) %>%
  summarize(avg_pm25 = mean(arithmetic_mean)) %>% # if multiple POC (monitors at a site), take average
  mutate(county_name = ifelse(county_name == "Saint Louis", "St Louis", county_name))  %>% # standardize with power plants df naming
  ungroup()
  
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
fossil_power_plants <- mn_powerplants %>%
  filter(fossil_fuel == "Fossil Fuel", county %in% EJ_stacked$County)

# Subset Reneable power plants
Renewable_power_plants <- mn_powerplants %>%
  filter(fossil_fuel == "Renewable", county %in% EJ_stacked$County)

# transform to sf
ej_sf <- st_as_sf(EJ_stacked)

# Combine ej census tracts with info about power plants
mn_powerplants_with_EJ <- mn_powerplants %>%
  left_join(EJ_stacked %>% select(County, EJ_OR_NOT),
            by = c("county" = "County")) %>%
  mutate(EJ_OR_NOT = if_else(is.na(EJ_OR_NOT), FALSE, EJ_OR_NOT))


#--------
# Find if a power plant is by an EJ area

## Work on EJ
# Manually enter crs
st_crs(ej_shp) <- 26915

# merge the ej areas relabeled with the ej shp
EJ_stacked <- EJ_stacked %>%
  mutate(geoid = as.character(geoid))
# join data
ej_tracts_sf <- ej_shp %>%
  left_join(EJ_stacked %>% select(geoid, EJ_OR_NOT),by = "geoid") %>%
  mutate(EJ_OR_NOT = if_else(is.na(EJ_OR_NOT), FALSE, TRUE))

## make sure power plants sf is sf
powerplants_sf <- mn_powerplants %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(26915)
ej_tracts_sf <- st_transform(ej_tracts_sf, st_crs(powerplants_sf))

## Join data
powerplants_with_ej <- st_join(
  ej_tracts_sf,
  powerplants_sf %>% select(plant_name, total_mw, fossil_fuel, county, zip, plant_code),
  join = st_contains) %>%
  mutate(power_plant_or_not = !is.na(plant_name))

metro_area_pp<- powerplants_with_ej %>% filter(countyfp %in% c("123", "053", "003", "019", "025", "037", "049", "139", "163")) 
  
your_points <- st_as_sf(your_data, coords = c("lon", "lat"), crs = 4326)



tt <- powerplants_with_ej %>%
  filter(countyfp %in% c("123", "053", "003", "019", "025", "037", "049", "139", "163")) %>%  #metro areas
  group_by(EJ_OR_NOT, power_plant_or_not, fossil_fuel) %>%
  summarize(count = n())

full <- powerplants_with_ej %>%
  group_by(EJ_OR_NOT, power_plant_or_not, fossil_fuel) %>%
  summarize(count = n())


herc <- powerplants_with_ej %>%
  filter(plant_code == 10013) %>%
  select(plant_name, total_mw, fossil_fuel, county, zip, plant_code, prp200x, tractce, prppoc, prplep)



# # Loading in census data
# mn_tracts <- tracts(state = "MN", year = 2020, class = "sf")
# Power_Plants_sf <- st_as_sf(mn_powerplants, coords = c("longitude", "latitude"), crs = 4326)
# 
# mn_tracts <- st_transform(mn_tracts, crs = st_crs(Power_Plants_sf))
# 
# # Bringing it together
# Power_Plants_with_tract <- st_join(Power_Plants_sf, mn_tracts[, c("GEOID", "NAME", "COUNTYFP")], join = st_within)
# 
# # Add a column to identify is power plant is part of EJ or Not also simplify df
# plants_in_ej_counts <- st_join(Power_Plants_with_tract, ej_sf, join = st_within) %>%
#   mutate(EJ_OR_NOT = if_else(is.na(EJ_OR_NOT), FALSE, EJ_OR_NOT)) %>%
#   select(x, y, OBJECTID, plant_code, plant_name, county, zip, prim_source, 
#          fossil_fuel, geometry, GEOID, NAME, COUNTYFP,EJ_OR_NOT, EJ_area) %>%
#   group_by(GEOID, fossil_fuel, EJ_OR_NOT) %>%
#   summarize(plant_count = n())  

# Getting population data
# total_pop <- get_acs(geography = "tract", variables = "B01003_001", state = "MN", year = 2023, geometry = FALSE)

# Joining power plant + ej areas with population data
# plants_per_pop <- ej_tracts %>%
#   left_join(total_pop %>% select(GEOID, estimate), by = "GEOID") %>%
#   rename(total_population = estimate) %>%
#   group_by(GEOID, fossil_fuel, EJ_OR_NOT) %>%
#   summarize(plant_count = n(), total_population = first(total_population), 
#             plants_per_10k = (plant_count / total_population) * 10000)


###==================== Write as csv ===================###

# overall
write.csv(mn_powerplants, "mn_powerplants.csv", row.names = FALSE)

# air quality
saveRDS(AirData_allyears, "Data/aq_data_clean/AirData_allyears.rds")

# health
st_write(zcta_joined, "zcta_joined.shp", row.names = FALSE)

# ej areas
st_write(mn_tracts, "mn_tracts.shp", row.names = FALSE)
st_write(ej_sf, "ej_sf.shp", row.names = FALSE)
st_write(plants_in_ej_counts, "plants_in_ej_counts.shp", row.names = FALSE)
# st_write(plants_per_pop, "plants_per_pop.shp", row.names = FALSE)

st_write(metro_area_pp, "metro_area_pp.shp", row.names = FALSE)

st_write(powerplants_sf, "powerplants_sf.shp", row.names = FALSE)
