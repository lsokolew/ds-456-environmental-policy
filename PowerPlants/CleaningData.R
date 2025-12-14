# Load Libraries
library(tidycensus)
library(dplyr)
library(tidyverse)
library(sf)
library(ggthemes)
library(ggplot2)
library(gganimate)
library(gifski)
library(readxl)

###==================== Load in data ===================###

###=== Needed Overall ===###

# power plant
mn_powerplants = read_csv('Data/Power_Plants.csv') %>%
  filter(State == "Minnesota") %>%
  mutate(fossil_fuel = ifelse(PrimSource %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable"),
         PrimSource = ifelse(PrimSource == "other", "waste heat", PrimSource))  %>% # change 'other' to 'waste heat'
  janitor::clean_names() 

mn_powerplants$fossil_fuel[mn_powerplants$plant_code == "10013"] <- "Fossil Fuel" # change HERC to nonrenewable


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
MN_asthma_ED <- read_csv("Data/MN_asthma_ED.csv")

# Zip codes
mn_zctas <- readRDS("Data/mn_zctas_2020.rds")

###=== EJ Areas ===###

# Environmental justice areas (subseted)
ej_spaces <- read_csv("Data/ej_mpca_census.csv") %>%
  select(-Shape_Area, -Shape_Length, -source, -statefp, -funcstat, -name, 
         -namelsad, -mtfcc, -intptlat, -intptlon, -geography, -countyfp, 
         -aland, -awater)

ej_shapefile <- st_read("Data/ej_mpca_census.shp")


tribal_shp <- st_read("Data/tribal_areas/census_tribal_areas.shp")


# tracts
mn_tracts <- tracts(state = "MN", cb = TRUE, year = 2023) %>%
  st_transform(crs = 4326)%>%
  mutate(GEOID = as.double(GEOID),
         County = gsub(" County", "", NAMELSADCO))  %>%
  select(GEOID, geometry, County) 

mn_counties <- counties(state = "MN", cb = TRUE, class = "sf")

schools_sf <- sf::read_sf("Data/shp_struc_school_program_locs/school_program_locations.shp")


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

# initial cleaning 
AirData_allyears <- AirData_allyears %>%
  filter(pollutant_standard == "PM25 24-hour 2012") %>% # 2012 is the most recent standard with the most observations; it doesn't change the numbers
  group_by(site_num, parameter_code, pollutant_standard, year, longitude, latitude, local_site_name, county_name, city_name, cbsa_name) %>%
  summarize(avg_pm25 = mean(arithmetic_mean)) %>% # if multiple POC (monitors at a site), take average
  mutate(county_name = ifelse(county_name == "Saint Louis", "St Louis", county_name))  %>% # standardize with power plants df naming
  ungroup()

# get # plants near each monitor

AirData_sf <- st_as_sf(AirData_allyears, coords = c("longitude", "latitude"), crs = 4326)
mn_pp_sf <- st_as_sf(mn_powerplants, coords = c("longitude", "latitude"), crs = 4326) %>% 
  filter(fossil_fuel == "Fossil Fuel")

# Project to meters (UTM 15N for Minnesota)
AirData_proj <- st_transform(AirData_sf, 26915)
mn_pp_proj  <- st_transform(mn_pp_sf, 26915)

# buffer in meters
buffer_distance <- 1609.34 * 3
air_buffers_proj <- st_buffer(AirData_proj, dist = buffer_distance)

# Spatial relationships in projected CRS
within_3miles <- st_within(mn_pp_proj, air_buffers_proj)
mn_pp_sf$near_monitor <- lengths(within_3miles) > 0

pp_within_each_monitor <- st_intersects(air_buffers_proj, mn_pp_proj)
air_buffers_proj$nearby_pp_count <- lengths(pp_within_each_monitor)

# Transform back to WGS84 for leaflet
air_buffers <- st_transform(air_buffers_proj, 4326)
  

# calculate average pm2.5 for each year 

calc_avg_pm25 <- function(year_spec){
  air_buffers %>% 
    filter(year == year_spec) %>% 
    mutate(grouped_nearby_pp_count = ifelse(nearby_pp_count > 1, "2+", as.character(nearby_pp_count))) %>% 
    group_by(grouped_nearby_pp_count) %>% 
    summarise(avg_pm25_grouped = mean(avg_pm25)) %>% 
    mutate(year = year_spec)
}

years <- 1999:2024 # incomplete data for 2025; would be inaccurate comparison
grouped_summ_pm25_allyears <- map(years, calc_avg_pm25) %>% list_rbind()

# check out area near the HERC

target_pp <- mn_pp_proj %>% filter(str_detect(plant_name, "Covanta"))
# find which air buffers intersect this plant's location
hits <- st_intersects(air_buffers_proj, target_pp)
# keep only buffers that intersect
air_buffers_near_target <- air_buffers_proj %>% filter(lengths(hits) > 0)
air_buffers_near_target <- st_transform(air_buffers_near_target, 4326)

herc_monitor_avgs <- air_buffers_near_target %>% 
  group_by(year) %>% 
  summarize(pm25_avg_by_year = mean(avg_pm25)) %>% 
  mutate(id = "Near HERC")

all_monitor_avgs <- air_buffers %>% 
  group_by(year) %>% 
  summarize(pm25_avg_by_year = mean(avg_pm25)) %>% 
  mutate(id = "Whole State")

all_avgs <- rbind(all_monitor_avgs, herc_monitor_avgs)

###=== Asthma ===###

# Asthma Data and ZCTAs

zcta_joined_asthma <- MN_asthma_ED %>%
  left_join(mn_zctas, by = c("_ZIP" = "GEOID20")) %>%
  mutate(`Age-adjusted rate per 10,000` = na_if(`Age-adjusted rate per 10,000`, "*"),
         `Age-adjusted rate per 10,000` = as.numeric(`Age-adjusted rate per 10,000`)) %>%
  mutate(value_cat = case_when(
    `Age-adjusted rate per 10,000` >= 0 & `Age-adjusted rate per 10,000` < 15 ~ "0-15",
    `Age-adjusted rate per 10,000` >= 15 & `Age-adjusted rate per 10,000` < 30 ~ "15-30",
    `Age-adjusted rate per 10,000` >= 30 & `Age-adjusted rate per 10,000` < 45 ~ "30-45",
    `Age-adjusted rate per 10,000` >= 45 & `Age-adjusted rate per 10,000` < 60 ~ "45-60",
    `Age-adjusted rate per 10,000` >= 60 ~ "60+",
    TRUE ~ NA_character_))




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

###=====Schools====###
points_sf_crs <- st_as_sf(
  mn_powerplants,
  coords = c("longitude", "latitude"),
  crs = 4326) %>%
  st_transform(26915)  

ej_areas <- ej_shapefile %>%
  filter(status200x == "YES" | statuspoc == "YES" | statuslep == "YES")

# -------- EJ Tracts ---------
# If ej_shapefile already has a CRS, USE IT — do not overwrite.
# Only set CRS if it is missing:
if (is.na(st_crs(ej_areas))) {
  ej_areas <- st_set_crs(ej_areas, 26915)
}

# -------- Schools shapefile --------
if (is.na(st_crs(schools_sf))) { # Only set CRS if missing
  schools_sf <- st_set_crs(schools_sf, 26915)
}
schools_sf <- schools_sf %>%
  filter(is.na(GRADERANGE))

school_proj <- st_transform(schools_sf, 26915)

school_pp_dist <- st_distance(school_proj, points_sf_crs)

schools_in_ej <- st_within(school_proj, ej_areas)
school_proj$schools_in_ej <- lengths(schools_in_ej) > 0

nearest_pp <- apply(school_pp_dist, 1, which.min)

school_proj$nearest_pp_id <- points_sf$plant_code[nearest_pp]
school_proj$nearest_pp_dist_m <- apply(school_pp_dist, 1, min)
school_proj$nearest_pp_dist_mi <- school_proj$nearest_pp_dist_m / 1609.34

distinct_schools <- school_proj %>% 
  distinct(GISADDR, .keep_all = TRUE)

distinct_schools_metro <- distinct_schools %>% 
  mutate(zip_code = str_extract(GISADDR, "\\b\\d{5}(?:-\\d{4})?(?=\\D|$)")) %>% filter(zip_code %in% metro_zips) 

###====Emissions====###
path <- "Data/emissions2017.xlsx"
sheets <- excel_sheets(path)

xl_list <- lapply(sheets, function(file){
  emissions_2017 <- read_xlsx(path, sheet = file)
})
NO2_emissions2017 <- xl_list[[3]]

names(NO2_emissions2017) <- as.character(NO2_emissions2017[1, ])
NO2_emissions2017 <- NO2_emissions2017[-1,]

NO2_emissions2017 <- NO2_emissions2017 %>%
  janitor::clean_names() %>%
  filter(state == "MN") %>%
  mutate(plant_code = as.numeric(plant_code)) %>%
  mutate(year = 2017)

# 2018
path2 <- "Data/emissions2018.xlsx"
sheets2 <- excel_sheets(path2)

xl_list2 <- lapply(sheets2, function(file){
  emissions_2018 <- read_xlsx(path2, sheet = file)
})
NO2_emissions2018 <- xl_list2[[3]]

names(NO2_emissions2018) <- as.character(NO2_emissions2018[1, ])
NO2_emissions2018 <- NO2_emissions2018[-1,]

NO2_emissions2018 <- NO2_emissions2018 %>%
  janitor::clean_names() %>%
  filter(state == "MN") %>%
  mutate(plant_code = as.numeric(plant_code)) %>%
  mutate(year = 2018)

# 2019
path3 <- "Data/emissions2019.xlsx"
sheets3 <- excel_sheets(path3)

xl_list3 <- lapply(sheets3, function(file){
  emissions_2019 <- read_xlsx(path3, sheet = file)
})
NO2_emissions2019 <- xl_list3[[3]]

names(NO2_emissions2019) <- as.character(NO2_emissions2019[1, ])
NO2_emissions2019 <- NO2_emissions2019[-1,]

NO2_emissions2019 <- NO2_emissions2019 %>%
  janitor::clean_names() %>%
  filter(state == "MN") %>%
  mutate(plant_code = as.numeric(plant_code)) %>%
  mutate(year = 2019)

# 2020
path4 <- "Data/emissions2020.xlsx"
sheets4 <- excel_sheets(path4)

xl_list4 <- lapply(sheets4, function(file){
  emissions_2020 <- read_xlsx(path4, sheet = file)
})
NO2_emissions2020 <- xl_list4[[3]]

names(NO2_emissions2020) <- as.character(NO2_emissions2020[1, ])
NO2_emissions2020 <- NO2_emissions2020[-1,]

NO2_emissions2020 <- NO2_emissions2020 %>%
  janitor::clean_names() %>%
  filter(state == "MN") %>%
  mutate(plant_code = as.numeric(plant_code)) %>%
  mutate(year = 2020)

# 2021
path5 <- "Data/emissions2021.xlsx"
sheets5 <- excel_sheets(path5)

xl_list5 <- lapply(sheets5, function(file){
  emissions_2021 <- read_xlsx(path5, sheet = file)
})
NO2_emissions2021 <- xl_list5[[3]]

names(NO2_emissions2021) <- as.character(NO2_emissions2021[1, ])
NO2_emissions2021 <- NO2_emissions2021[-1,]

NO2_emissions2021 <- NO2_emissions2021 %>%
  janitor::clean_names() %>%
  filter(state == "MN") %>%
  mutate(plant_code = as.numeric(plant_code)) %>%
  mutate(year = 2021)

all_emissions <- rbind(NO2_emissions2017, NO2_emissions2018)
all_emissions <- rbind(all_emissions, NO2_emissions2019)
all_emissions <- rbind(all_emissions, NO2_emissions2020)
all_emissions <- rbind(all_emissions, NO2_emissions2021)

mn_powerplants_nonrenewable <- mn_powerplants %>% filter(fossil_fuel == "Fossil Fuel")

all_emissions_data <- all_emissions %>%
  distinct(plant_code, eia_model_estimates_of_n_ox_emissions_tons, year, .keep_all = TRUE) %>%
  mutate(plant_code = as.numeric(plant_code)) %>%
  left_join(mn_powerplants_nonrenewable, by = c("plant_code" = "plant_code")) %>%
  distinct(plant_code, eia_model_estimates_of_n_ox_emissions_tons, .keep_all = TRUE) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  group_by(plant_code, plant_name.x, zip, longitude, latitude) %>%
  summarise(`eia_model_estimates_of_n_ox_emissions_tons` = sum(as.numeric(eia_model_estimates_of_n_ox_emissions_tons))) %>%
  filter(zip %in% metro_zips) %>%
  arrange(desc(`eia_model_estimates_of_n_ox_emissions_tons`))



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


# Animations for powerplant
max_year <- 2025

mn_powerplants_over_time <- mn_powerplants %>%
  select(plant_code, longitude, latitude, first_op_date, first_op_year) %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  rowwise() %>%
  mutate(year = list(first_op_year:max_year)) %>% 
  unnest(year) %>%                                 
  mutate(is_current_year = ifelse(year == first_op_year, "New", "Old")) %>%
  mutate(display = year >= first_op_year) %>%
  arrange(year)


p_map <-ggplot() +
  geom_sf(data = mn_counties, fill = NA, color = "#454545", size = 0.1) +
  geom_point(
    data = mn_powerplants_over_time %>% filter(display),
    aes(longitude, latitude, color = is_current_year),
    size = 1.75, alpha = 0.9) +
  scale_color_manual(values = c("New" = "#F2447E", "Old" = "#A19F9F")) +
  coord_sf() +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    strip.background = element_blank(),
    legend.background = element_rect(fill = "transparent", color = NA),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(title = "Year: {current_frame}", color = "") +
  transition_manual(year) 


animate(
  p_map,
  fps = 10,
  duration = 30,
  width = 600,
  height = 400,
  renderer = gifski_renderer("www/animations/powerplants_animation.gif"),
  bg = 'transparent'
)


###==================== Write as csv ===================###

# overall
write.csv(mn_powerplants, "mn_powerplants.csv", row.names = FALSE)

# air quality
saveRDS(AirData_allyears, "Data/aq_data_clean/AirData_allyears.rds")
save(mn_pp_sf, AirData_sf, air_buffers, grouped_summ_pm25_allyears, all_avgs, file="Data/aq_data_clean/wrangled_airdata.rds")

# emissions
write_csv(all_emissions_data, "all_emissions_data.csv")

# health
st_write(zcta_joined_asthma, "zcta_joined.shp", row.names = FALSE)
# schools
write.csv(distinct_schools_metro, "distinct_schools_metro.csv")

# ej areas
st_write(mn_tracts, "mn_tracts.shp", row.names = FALSE)
st_write(ej_sf, "ej_sf.shp", row.names = FALSE)
st_write(plants_in_ej_counts, "plants_in_ej_counts.shp", row.names = FALSE)
# st_write(plants_per_pop, "plants_per_pop.shp", row.names = FALSE)

st_write(metro_area_pp, "metro_area_pp.shp", row.names = FALSE)

st_write(powerplants_sf, "powerplants_sf.shp", row.names = FALSE)

