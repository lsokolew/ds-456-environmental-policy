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
library(tigris)


###==================== Load in data ===================###

  ###=== Needed Overall ===###
  # power plant
    mn_powerplants <- read_csv('Data/Power_Plants.csv') %>%
      filter(State == "Minnesota") %>%
      mutate(fossil_fuel = ifelse(PrimSource %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable"),
             PrimSource = ifelse(PrimSource == "other", "waste heat", PrimSource))  %>% # change 'other' to 'waste heat'
      janitor::clean_names() 
  # Quick Change: Making the HERC non reneable
    mn_powerplants$fossil_fuel[mn_powerplants$plant_code == "10013"] <- "Fossil Fuel" 
  
  # power plant dates
    powerplant_dates <- read_csv("Data/powerplant_data_eia_2024_generator_operable.csv") %>% 
      janitor::clean_names() 

  ###=== Air Quality ===###
  # air quality 
    AirData_allyears <- read_csv("Data/airdata_clean.csv") 

  ###=== Healthcare ===###
  # Asthma
  MN_asthma_ED <- read_csv("Data/MN_asthma_ED.csv")
  zcta_pop_data <- read.csv("Data/zcta_pop_data.csv")
  
  # Zip codes
  mn_zctas <- readRDS("Data/mn_zctas_2020.rds")
  
  # Schools
  schools_sf <- sf::read_sf("Data/shp_struc_school_program_locs/school_program_locations.shp")

  ###=== EJ Areas ===###
  
    # Environmental justice areas (subseted)
    ej_spaces <- read_csv("Data/ej_mpca/ej_mpca_census.csv") %>%
      select(-Shape_Area, -Shape_Length, -source, -statefp, -funcstat, -name, 
             -namelsad, -mtfcc, -intptlat, -intptlon, -geography, -countyfp, 
             -aland, -awater)
    
    ej_shp <- st_read("Data/ej_mpca/ej_mpca_census.shp")
    
    # tracts
    mn_tracts <- tracts(state = "MN", cb = TRUE, year = 2023) %>%
      st_transform(crs = 4326)%>%
      mutate(GEOID = as.double(GEOID),
             County = gsub(" County", "", NAMELSADCO))  %>%
      select(GEOID, geometry, County) 
    
    # MN Counties
    mn_counties <- counties(state = "MN", cb = TRUE, class = "sf")

    # census ACS Tract information (full)
    CensusACSTract <- read_xlsx("Data/CensusACSTract.xlsx")
    

###==================== Wrangling ====================###

  # getting operating dates
    powerplant_dates_mn <- powerplant_dates %>% 
      filter(state == "MN") %>% 
      mutate(full_date = make_date(year = operating_year, month = operating_month, day = 1)) %>% 
      group_by(plant_code) %>% 
      summarise(first_op_date = min(full_date))
  
  # add years to mn_powerplants (main dataset we're using for powerplants)
    mn_powerplants <- mn_powerplants %>% left_join(powerplant_dates_mn)
  
  # fill in missing dates(via sunshare website):
    # 1.Buffalo Sun CSG - active since 2/1/2025
    # 2.Oster Sun CSG	- active since 1/1/2025
    # 3.Quarry Sun CSG - active since 1/1/2025
  
  mn_powerplants <- mn_powerplants %>%
    mutate(first_op_date = case_when(plant_code == 66070 ~ as.Date(mdy("2/1/2025")),
                                     plant_code == 66072 ~ as.Date(mdy("1/1/2025")),
                                     plant_code == 66073 ~ as.Date(mdy("1/1/2025")),
                                     TRUE ~ first_op_date), 
           first_op_year = year(first_op_date)) 

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

# POC and Asthma

points_sf_crs <- st_as_sf( # power plants
  mn_powerplants,
  coords = c("longitude", "latitude"),
  crs = 4326) %>%
  st_transform(26915)  

zcta_pop_data <-zcta_pop_data %>% 
  mutate(pct_poc = (blackE + asianE + hispanicE) / total_popE)


asthma_poc_joined <- zcta_joined_asthma %>%
  mutate(`_ZIP` = as.numeric(`_ZIP`)) %>%
  left_join(zcta_pop_data, by = c("_ZIP" = "GEOID"))  %>%
  filter(`Age group` == "All ages")

asthma_poc_joined_sf <- st_as_sf(asthma_poc_joined)

zips_proj <- st_transform(asthma_poc_joined_sf, 26915)

NR_points_sf_crs <- points_sf_crs %>% filter(fossil_fuel == "Fossil Fuel") %>% st_transform(26915)

zipcode_buffer_mile <- st_buffer(zips_proj, dist = 1609.34)


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

zip_plant_counts_mile <- st_join(zipcode_buffer_mile, NR_points_sf_crs, join = st_intersects) %>%
  filter(`_ZIP` %in% metro_zips) %>%
  group_by(`_ZIP`) %>%      
  summarise(num_plants = n())

asthma_poc_powerplant <- asthma_poc_joined %>%
  st_drop_geometry() %>%
  left_join(zip_plant_counts_mile, by = c("_ZIP" = "_ZIP")) %>%
  mutate(near_plant = if_else(is.na(num_plants), FALSE, TRUE))


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

  #--------------------------------------------------------------
  # Find if a power plant is by an EJ area
  
  # Manually enter crs
  st_crs(ej_shp) <- 26915
  
  # merge the ej areas relabeled with the ej shp
  EJ_stacked <- EJ_stacked %>%
    mutate(geoid = as.character(geoid))
  
  # join data
  ej_tracts_sf <- ej_shp %>%
    left_join(EJ_stacked %>% select(geoid, EJ_OR_NOT),by = "geoid") %>%
    mutate(EJ_OR_NOT = if_else(is.na(EJ_OR_NOT), FALSE, TRUE))
  
  # make sure power plants sf is sf
  powerplants_sf <- mn_powerplants %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
    st_transform(26915)
  ej_tracts_sf <- st_transform(ej_tracts_sf, st_crs(powerplants_sf))
  
  # Join data
  powerplants_with_ej <- st_join(
    ej_tracts_sf,
    powerplants_sf %>% select(plant_name, total_mw, fossil_fuel, county, zip, plant_code),
    join = st_contains) %>%
    mutate(power_plant_or_not = !is.na(plant_name))
  
  #-------------------------
  # Metro Area Analysis
  
  # filter out metro area counties
  metro_area_pp<- powerplants_with_ej %>% filter(countyfp %in% c("123", "053", "003", "019", "025", "037", "049", "139", "163")) 
  
  # transform data to correct crs
  metro_area_pp <- st_transform(metro_area_pp, crs = 4326)
  
  # find distribution of power plants in metro area (and by enviromental justice area and type of plants)
  metro_EJ_x_PP <- metro_area_pp %>%
    group_by(EJ_OR_NOT, power_plant_or_not, fossil_fuel) %>%
    summarize(count = n())
  
  # find distribution of power plants in MN (and by enviromental justice area and type of plants)
  mn_EJ_x_PP <- powerplants_with_ej %>%
    group_by(EJ_OR_NOT, power_plant_or_not, fossil_fuel) %>%
    summarize(count = n())
  
  #-------------------------
  # HERC Analysis
  
  # HERC Information: Conclusion is that it's not an EJ areas
  herc <- powerplants_with_ej %>%
    filter(plant_code == 10013) %>%
    select(plant_name, total_mw, fossil_fuel, county, zip, plant_code, prp200x, tractce, prppoc, prplep)
  
  # calculate the population demographics that make up the 1 mile radious (areas the herc impacts within one mile)
  herc_area_poo <- metro_area %>%
    filter(tractce %in% c("126201", "126202", "103000", "103600", "126101", "126102", "104400", "105204",
                          "105201", "105500", "104100","103400", "003300","126300")) %>%
    select(tractce, prp200x, prppoc, prplep, totpov, totpoc, totlep, totlan, totnnng, poc) %>%
    summarise(mean_pct_200x = mean(prp200x), 
              sum_lan_pop = sum(totlan),
              sum_land_nng = sum(totnnng),
              sum_pop = sum(totpoc),
              sum_poc = sum(poc),
              sum_pop = sum(totpov)) %>%
    mutate(prp_eng = sum_land_nng/sum_lan_pop, 
           prp_poc =  sum_poc/sum_pop)
  
  # just select the population demographics that make up the 1 mile radius no recalculating 
  herc_ind_area <-metro_area %>%
    filter(tractce %in% c("126201", "126202", "103000", "103600", "126101", "126102", "104400", "105204",
                          "105201", "105500", "104100","103400", "003300","126300")) %>%
    select(tractce,prppoc, prp200x, prplep, -geometry)%>%
    arrange(desc(prppoc))%>% 
    rename(`Census Tract` = tractce,
           `POC Proportion` = prppoc,
           `Under Poverty Level Proportion` = prp200x,
           `Non-English Speakers Proprtion` = prplep)
  
  #-------------------------
  # Household Income Analysis
  
  # Select only household related information
  census_tract_income <- CensusACSTract %>%
    mutate(GEOID2 = as.double(GEOID2)) %>%
    dplyr::select(1:20, MEANHHINC, MEDIANHHI)  

  # transform it to the matching crs (needed to join)
  mn_tracts <- tracts(state = "MN", cb = TRUE, year = 2023) %>%
    st_transform(crs = 4326)%>%
    mutate(GEOID = as.double(GEOID),
           County = gsub(" County", "", NAMELSADCO))  
  
  # change crs of mn_tracts
  mn_tracts_diff <- st_transform(mn_tracts, 26915)   
  
  # join powerplant and mn tracts
  powerplant_with_tract <- st_join(powerplants_sf, mn_tracts_diff,join = st_within) %>%
    group_by(GEOIDFQ, fossil_fuel) %>%
    summarize(count = n())
  
  # Join power plant now with census (possible because powerplant has tract info)
  census_with_pp <- census_tract_income %>%
    left_join( powerplant_with_tract,
      by = c("GEOID" = "GEOIDFQ")) %>%
      mutate(count = if_else(is.na(count), 0, count))
    
  # create "breaks" or sports in the Mean Household Income you want to split for analysis
  # Then create labels so they are easier to interpret (big numbers read weird)
  breaks <- seq(0, max(census_with_pp$MEANHHINC, na.rm = TRUE) + 50000, by = 50000)
  labels <- paste0(breaks[-length(breaks)] / 1000, "–",breaks[-1] / 1000, "k")
  
  # now actually apply the breaks to mean household income then find the total_powerplants
  # and number of tracts income bin (and fossil fuel)
  pp_by_income <- census_with_pp %>%
    mutate(income_bin = cut(MEANHHINC,breaks = breaks,labels = labels,include.lowest = TRUE)) %>%
    group_by(income_bin, fossil_fuel) %>%
    summarise(total_powerplants = sum(count, na.rm = TRUE), count_tracts = n())
  
  #-------------------------
  # 1 Mile buffer
  
  # grab the geoid for each EJ Census tract
  ej_geoids <- as.numeric(ej_sf$geoid)
  
  # in the full mn_tracts, if the geoid is in the EJ_sf, mark it as an EJ_Area
  mn_tracts_flagged <- mn_tracts %>%
    mutate(EJ_Area = GEOID %in% ej_geoids)
  
  # transform the data tha will be joined to matching crs 
  mn_tracts_flagged <- st_transform(mn_tracts_flagged, 26915)   
  powerplants_sf <- st_transform(powerplants_sf, 26915)
  
  # Create buffer around each powerplant for 1 mile (1609.34 = meter)
  powerplants_buffer <- st_buffer(powerplants_sf, dist = 1609.34)
  
  # Join mn_tracts with powerplants (and the buggers)
  tracts_near_pp <- st_join(mn_tracts_flagged, powerplants_buffer, join = st_intersects)  
  
  # Find how many tracts are within 1 mile from a powerplant (and seperate. by type of fuel)
  pp_summary <- tracts_near_pp %>%
    group_by(plant_name, fossil_fuel, COUNTYFP) %>%
    summarise(number_tracts = n(),
              number_ej_tracts = sum(EJ_Area),
              prop = number_ej_tracts/number_tracts) 
  
  
  pp_summary %>%
    filter(fossil_fuel == "Fossil Fuel") %>%
    ggplot(aes(x =prop)) +
    geom_histogram(fill = "darkblue") +
    theme_classic() +
    labs(x = "Proportion", y = "Count",
         title = "Proportion of Enviromental Justice areas affected per Powerplant (1 mile radius)",
         subtitle = "For Fossil Fuel Plants")
  
  
  pp_summary %>%
    filter(fossil_fuel == "Fossil Fuel") %>%
    filter(COUNTYFP %in% c("123", "053", "003", "019", "025", "037", "049", "139", "163")) %>%
    ggplot(aes(x =prop)) +
    geom_histogram(fill = "darkgreen") +
    theme_classic() +
    labs(x = "Proportion", y = "Count",
         title = "Proportion of Enviromental Justice areas affected per Powerplant (1 mile radius)",
         subtitle = "For Fossil Fuel Plants in Metro Area (Twin Cities)")

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
  
  school_proj$nearest_pp_id <- points_sf_crs$plant_code[nearest_pp]
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
  

###=== Animation of Powerplants over the years ===###
  
  # define the last year of data
    max_year <- 2025
  
  # for each year, if a power plant was already isntalled, have it as old, if not have it as new
  mn_powerplants_over_time <- mn_powerplants %>%
    select(plant_code, longitude, latitude, first_op_date, first_op_year) %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>%
    rowwise() %>%
    mutate(year = list(first_op_year:max_year)) %>% 
    unnest(year) %>%                                 
    mutate(is_current_year = ifelse(year == first_op_year, "New", "Old")) %>%
    mutate(display = year >= first_op_year) %>%
    arrange(year)
  
  # Plot the data, new vs old power plants
  p_map <-ggplot() +
    geom_sf(data = mn_counties, fill = NA, color = "#454545", size = 0.1) +
    geom_point(data = mn_powerplants_over_time %>% filter(display),
               aes(longitude, latitude, color = is_current_year), size = 1.75, alpha = 0.9) +
    scale_color_manual(values = c("New" = "#F2447E", "Old" = "#A19F9F")) +
    coord_sf() +
    theme_void() +
    theme(panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid = element_blank(),
          strip.background = element_blank(),
          legend.background = element_rect(fill = "transparent", color = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    labs(title = "Year: {current_frame}", color = "") +
    transition_manual(year) 
  
  # Actually Animate the leaflet (and save it, so it's just a graph for every year)
    animate(p_map, fps = 10, duration = 30, width = 600,height = 400,
            renderer = gifski_renderer("www/animations/powerplants_animation.gif"),
            bg = 'transparent')

###==================== Write as csv ===================###

# overall
write.csv(mn_powerplants, "Data/cleaning_data/mn_powerplants.csv", row.names = FALSE)

# air quality
saveRDS(AirData_allyears, "Data/aq_data_clean/AirData_allyears.rds")
save(mn_pp_sf, AirData_sf, air_buffers, grouped_summ_pm25_allyears, all_avgs, file="Data/aq_data_clean/wrangled_airdata.rds")

# emissions
write_csv(all_emissions_data, "all_emissions_data.csv")

# health
st_write(zcta_joined_asthma, "Data/cleaning_data/zcta_joined.shp", row.names = FALSE)
write.csv(asthma_poc_powerplant, "Data/asthma_poc_ppowerplant.csv", row.names = FALSE)
write.csv(distinct_schools_metro, "Data/distinct_schools_metro.csv", row.names = FALSE)

# ej areas
st_write(mn_tracts, "Data/cleaning_data/mn_tracts.shp", row.names = FALSE)
st_write(ej_sf, "Data/cleaning_data/ej_sf.shp", row.names = FALSE)
st_write(plants_in_ej_counts, "Data/cleaning_data/plants_in_ej_counts.shp", row.names = FALSE)
st_write(metro_area_pp, "Data/cleaning_data/metro_area_pp.shp", row.names = FALSE)
st_write(powerplants_sf, "Data/cleaning_data/powerplants_sf.shp", row.names = FALSE)