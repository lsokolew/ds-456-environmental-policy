# Libraries
library(shiny)
library(bslib)
library(leaflegend)
library(sf)
library(tigris)

options(tigris_use_cache = TRUE)


# Sourcing ui to know what plot is being called
source("global.R")
source("ui.R")

server = function(input, output, session){
  
###================================ Opening ===============================###
  

  map_df = reactive({

    mn_powerplants %>%
      filter(first_op_year == as.numeric(input$numeric[1]))%>%
      filter(!is.na(longitude) & !is.na(latitude)) %>%
      group_by(plant_code, longitude,latitude) %>%
      summarize (count = n()) %>%
      st_as_sf(coords = c('longitude', 'latitude')) %>%
      st_set_crs(4326)

  })


  map_lp = reactive({

    mn_powerplants %>%
      filter(first_op_year < as.numeric(input$numeric[1]))%>%
      filter(!is.na(longitude) & !is.na(latitude)) %>%
      group_by(plant_code, longitude,latitude) %>%
      summarize (count = n()) %>%
      st_as_sf(coords = c('longitude', 'latitude')) %>%
      st_set_crs(4326)
    })

  output$map = renderLeaflet({

    leaflet() %>%
      addTiles() %>%
      setView(lng = -94.6, lat = 46.4, zoom = 6) %>%
      addCircleMarkers(data = map_df(), color = "red", radius = 3) %>%
      addCircleMarkers(data = map_lp(), color = "grey", radius = 1) %>%
      addCircleMarkers(data = map_lp(), color = "grey", radius = 1) %>%
      addLegendFactor(
        pal = colorFactor(my_colors, domain = values),
        values = values,
        orientation = "horizontal",
        opacity = 0.75,
        position = "topright",
        width = 12,
        height = 12)
    })
  
  ###================================ Power Plants ===============================###
  
  output$intereactive_pp_types = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -94.6, lat = 46.4, zoom = 6) %>%
      addCircleMarkers(data = mn_powerplants, 
                       color = ~if_else(fossil_fuel == "Fossil Fuel", "#d95f02", "#1b9e77"), 
                       radius = ~rescale(total_mw, to = c(1, 16)),
                       #stroke = FALSE,
                       label = ~paste0(plant_name, " (", prim_source, " - ", total_mw, " megawatt(s))")
      ) %>%
      addLegend(
        position = "topright",
        title = "Power Plants by \nProduction (MW)",
        colors = c("#d95f02", "#1b9e77"),
        labels = c(
          "Fossil Fuel",
          "Renewable"
        )
      )
  })
  
  output$pp_dates_barplot = renderPlot({

    mn_powerplants %>%
      ggplot(aes(x = first_op_year, fill = fossil_fuel)) +
      geom_bar() +
      facet_wrap(~fossil_fuel) +
      labs(title = "When Have Different Kinds of Power Plants Been Built in Minnesota?",
           x="First Year of Operation",
           y = "Number of Powerplants",
           fill = "Powerplant Type") +
      theme_classic() +
      theme_1 +
      fuel_colors

  }, bg = "transparent")
  
  output$pp_type_barplot = renderPlot({
    
    mn_powerplants %>% 
      #mutate(fossil_fuel = ifelse(prim_source %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable")) %>% 
      ggplot(aes(x = fct_infreq(prim_source), fill = fossil_fuel)) +
      geom_bar() +
      labs(x = "Energy Source", y = "Number of Powerplants", title = "Minnesota Powerplants' Energy Sources",
           fill = "Powerplant Type") +
      theme_classic() +
      theme_1    +
      fuel_colors
  }, bg = "transparent")
  
  output$pp_type_by_mw_barplot = renderPlot({
    
    mn_powerplants %>% 
      group_by(prim_source) %>% 
      summarise(prod_by_type = sum(total_mw)) %>% 
      mutate(fossil_fuel = ifelse(prim_source %in% c("coal", "petroleum", "natural gas"), "Fossil Fuel", "Renewable")) %>% 
      ggplot(aes(x = fct_reorder(prim_source, prod_by_type, .desc = TRUE), y = prod_by_type, fill = fossil_fuel)) +
      geom_bar(stat = 'identity') +
      fuel_colors +
      labs(x = "Energy Source", y = "Megawatts Electricity Produced", title = "Electricity Produced by Energy Source in MN Powerplants",
           fill = "Powerplant Type") +  
      theme_classic() +
      theme_1
    }, bg = "transparent")
  
  ###================================ Air Quality ===============================###

  # Map the Buffers
  
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
  
  output$monitor_buffers = renderLeaflet({
    
    leaflet()  %>%
      addPolygons(data = mn_tracts,
                  color = "black",
                  fillOpacity = 0,
                  weight = 0.5) %>%
      addPolygons(
        data = ej_sf,
        fillColor = ~ifelse(EJ_OR_NOT, "darkgreen", "white"),
        fillOpacity = 0.3,
        color = "white",
        weight = 0.15
      ) %>% 
      setView(lng = -93.265, lat = 44.9778, zoom = 9) %>%
      # --- 3-mile buffers around monitors ---
      addPolygons(
        data = air_buffers %>% filter(year == 2015, nearby_pp_count > 0),
        fillColor = "lightblue",
        fillOpacity = 0.4,
        color = "steelblue",
        weight = 1,
        label = ~paste0("Monitor: ", local_site_name, " - ", nearby_pp_count, " plants nearby")
      ) %>%
      # --- Power plants ---
      addCircleMarkers(
        data = mn_pp_sf,
        radius = 2,
        color = "#838383",
        fillOpacity = 0.8,
        label = ~paste0("Power Plant: ", plant_name)
      ) %>%
      # --- Air monitors ---
      addCircleMarkers(
        data = AirData_sf %>% filter(year == 2015),
        radius = .5,
        color = "steelblue",
        fill = TRUE,
        fillOpacity = 1
      ) %>%
      addLegend(
        position = "topright",
        title = "Where Are Air Monitors?",
        colors = c(
          "darkgreen",   # EJ areas
          "lightblue",   # Monitor buffers
          "#838383",    # Power plants
          "steelblue"    # Air monitors
        ),
        labels = c(
          "Environmental Justice Area",
          "3-mile Buffer (≥1 Nearby Plant)",
          "Power Plant",
          "Air Monitor"
        ),
        opacity = 1
      )
  })
  
  
  # plot the pollutant concentrations grouped by number of nearby plants
  
  calc_avg_pm25 <- function(year_spec){
    air_buffers %>% 
      filter(year == year_spec) %>% 
      group_by(nearby_pp_count) %>% 
      summarise(avg_pm25_grouped = mean(avg_pm25)) %>% 
      mutate(year = year_spec)
  }
  
  years <- 1999:2024 # incomplete data for 2025; would be inaccurate comparison
  grouped_summ_pm25_allyears <- map(years, calc_avg_pm25) %>% list_rbind()

  output$grouped_summ_lineplot = renderPlot({
  ggplot(grouped_summ_pm25_allyears) +
    geom_line(aes(x = year, y = avg_pm25_grouped, group = fct_rev(as.factor(nearby_pp_count)), color = fct_rev(as.factor(nearby_pp_count)))) +
    labs(x = "Year", y = "Average PM2.5 Concentration (µg/m3)",
         color = "Number of Plants \nNear Monitor",
         title = "Air Monitors Near More Plants Report Higher Pollutant Concentrations") +
    theme_minimal()
  }, bg = "transparent")
  
  
  # compare the pollutant concentration the year before and after new plant built
  
  plant_monitor_pairs <- st_intersects(air_buffers, mn_pp_sf)
  
  monitor_pp <- tibble(
    site_num = air_buffers$site_num,
    plant_index = pp_within_each_monitor
  ) %>%
    unnest(plant_index) %>%                             
    mutate(
      plant_id = mn_pp_sf$plant_code[plant_index],
      plant_year = mn_pp_sf$first_op_year[plant_index]
    ) %>%
    select(-plant_index) %>% 
    distinct() # keep only unique rows
  
  # Join to the air quality time series
  aq_with_pp <- AirData_allyears %>%
    left_join(monitor_pp, by = "site_num")
  
  # ---- 1 year change -----
  aq_changes <- aq_with_pp %>%
    filter(!is.na(plant_year)) %>%     # monitors near a plant
    mutate(period = case_when(
      year == plant_year - 1 ~ "before",
      year == plant_year + 1 ~ "after",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period))
  
  aq_changes_summ <- aq_changes %>%
    group_by(site_num, plant_id) %>%
    summarize(
      before = avg_pm25[period == "before"],
      after  = avg_pm25[period == "after"],
      change = after - before
    ) %>% 
    pivot_longer(3:4, names_to = "period", values_to = "avg_pm25_annual")

  output$one_yr_aq_change_lineplot = renderPlot(
    {ggplot(aq_changes_summ) +
    geom_point(aes(x = fct_relevel(period, c("before", "after")),  
                   y = avg_pm25_annual, 
                   group = site_num, color = site_num)) +
    geom_line(aes(x = fct_relevel(period, c("before", "after")),  
                  y = avg_pm25_annual, 
                  group = site_num, color = site_num)) +
      scale_color_discrete(labels = c("B.F. Pearson School", "Ramsey Health Center", "Near Road I-35/I-94", "Andersen School")) +
    theme_minimal() +
    labs(title = "PM2.5 Concentration from Air Monitors Near New Plants",
         x = "Year (Relative to Plant Beginning Operations)",
         y = "PM2.5 Concentration (µg/m3)",
         color = "Air Monitor Site")
  }, bg = "transparent")
  
  ff_status <- mn_powerplants %>%
  group_by(county) %>%
  summarize(has_fossil = any(fossil_fuel == "Fossil Fuel")) %>%
  mutate(plant_group = ifelse(has_fossil, "Has Fossil Fuel", "Only Renewable/None"))

  output$aq_by_county_type_lineplot = renderPlot({
    AirData_allyears %>%
      left_join(ff_status, by = c("county_name" = "county")) %>% 
      group_by(year, county_name, plant_group) %>% 
      summarize(avg_pm25_grouped = mean(avg_pm25)) %>% 
      mutate(plant_group = ifelse(is.na(plant_group), "Only Renewable/None", plant_group)) %>%
      ggplot(aes(x = year, y = avg_pm25_grouped, group = county_name, color = plant_group)) +
        geom_line(alpha = 0.4) + #  individual counties
        stat_summary(aes(group = plant_group), fun = mean, geom = "line", size = 1.5) + # mean trend
        labs(
          title = "Average PM2.5 Concentration by County Type",
          color = "County Plant(s) Type",
          y = "Average PM2.5 Concentration (µg/m3)",
          x = "Year"
        ) +
        ylim(0, 12) +
        scale_color_manual(values = c("Has Fossil Fuel" = "#d95f02",
                                      "Only Renewable/None" = "#1b9e77")) +
        theme_minimal()
  }, bg = "transparent")
  
  
  

  ###================================ Health ===============================###
  
  output$asthma_map <- renderLeaflet({
    
    # ---- Color palette for polygons ----
    pal <- colorFactor(
      palette = c("lightblue", "steelblue", "royalblue4", "navy"),
      levels = c("0-2", "2-4", "4-7", "7+"),
      na.color = "grey"
    )
    
    # ---- Palette for power plants ----
    leaflet(zcta_joined) %>%
      setView(lng = -93.265, lat = 44.9778, zoom = 8) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(valu_ct),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        opacity = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "white"
        ),
        # label = ~paste0(
        #   "Zipcode: ", ZCTA5CE20, "<br>",
        #   "Rate: ",
        #   ifelse(is.na(`Age-adjusted rate per 10,000`),
        #          "Not given due to small population",
        #          `Age-adjusted rate per 10,000`)
        # ),
        labelOptions = labelOptions(
          style = list("white-space" = "pre-line")
        )
      ) %>%
      addLegend(
        pal = pal,
        values = zcta_joined$valu_ct,
        opacity = 0.7,
        title = "Asthma hospitalizations per 10,000 (2017–2021)",
        position = "bottomright",
        na.label = "Not given"
      ) %>%
      addLegend(
        data = mn_powerplants,
        pal = pal3,
        values = ~fossil_fuel,
        opacity = 0.7,
        title = "Renewable or Fossil Fuel",
        position = "bottomright",
        na.label = "Not given") %>%
      addCircleMarkers(
        data = mn_powerplants,
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal3(fossil_fuel),
        radius = 3,        # 0.25 is too small to see
        fillOpacity = 1,
        label = ~paste0(
          "Plant Name: ", plant_name, "<br>",
          "Plant Code: ", plant_code
        )
      )
  })


###================================ EJ ===============================###

# # Plot Fossil Fuel
# 
# output$pp_ej_ff <- renderLeaflet({
# leaflet() %>%
#   addPolygons(data = mn_tracts,
#               color = "black",
#               fillOpacity = 0,
#               weight = 0.5) %>%
#   addPolygons(
#     data = ej_sf,
#     fillColor = ~pal1(EJ_area),
#     fillOpacity = 0.7,
#     color = "white",
#     weight = 0.15
#   ) %>%
#   addLegend(
#     pal = pal1, values = ej_sf$EJ_area, title ="Environmental Justice Area")  %>%
#   addCircleMarkers(
#     data = fossil_power_plants,
#     lng = ~longitude,
#     lat = ~latitude,
#     radius = 1.75,
#     fillOpacity = 0.75,
#     opacity = 0.1,
#     color = "#000000")
# 
# })
# # 
# # 
# # # Plot renable Fuel 
# # 
# output$pp_ej_re <- renderLeaflet({
#   leaflet() %>%
#   addPolygons(data = mn_tracts,
#               color = "black",
#               fillOpacity = 0,
#               weight = 0.5) %>%
#   addPolygons(
#     data = ej_sf,
#     fillColor = ~pal1(EJ_area),
#     fillOpacity = 0.7,
#     color = "white",
#     weight = 0.15
#   ) %>%
#   addLegend(
#     pal = pal1, values = ej_sf$EJ_area, title ="Enviromental Justice Area")  %>%
#   addCircleMarkers(
#     data = Renewable_power_plants,
#     lng = ~longitude,
#     lat = ~latitude,
#     radius = 1.75,
#     fillOpacity = 0.75,
#     opacity = 0.1,
#     color = "#000000")
# 
# })
# 
# ## Counts of power plants per census tracts
# 
# output$pp_count_all = renderPlot({
# 
#   plants_in_ej_counts %>%
#     ggplot(aes(x = plant_count)) +
#     geom_histogram(binwidth = 1, fill = "#c44900", color = "white") +
#     theme_classic() +
#     facet_wrap(~fossil_fuel + EJ_OR_NOT) +
#     labs(title = "Distribution of Power Plants per Census Tract",
#          subtitle = "Comparison by energy type and Environmental Justice area status",
#          x = "Number of Power Plants per Census Tract", y = "Number of Census Tracts") +
#     theme_1
#     }, bg = "transparent")
# 
# 
#   output$pp_count_ej = renderPlot({
#     
#     plants_in_ej_counts %>%
#     filter(EJ_OR_NOT == TRUE) %>%
#     ggplot(aes(x = plant_count)) +
#     geom_histogram(binwidth = 1, fill = "#22577a", color = "white") +
#     theme_classic() +
#     facet_wrap(~fossil_fuel) +
#     labs(title = "Distribution of Power Plants per Census Tract",
#        subtitle = "Comparison by energy type for Enviromental Justice Areas",
#        x = "Number of Power Plants per Census Tract", y = "Number of Census Tracts") +
#       theme_1
#     }, bg = "transparent")
#   
# ## Populations to where power plants are at
# 
# # Plot all
# 
# output$pp_pop_all = renderPlot({
#   
#   plants_per_pop %>%
#     ggplot(aes(x = plants_per_10k)) +
#     geom_histogram( fill = "#c44900", color = "white") +
#     theme_classic() +
#     facet_wrap(~fossil_fuel + EJ_OR_NOT) +
#     labs(title = "Distribution of Power Plants per 10,000 People",
#          subtitle = "For type of power plants and environmental justice designation",
#          x = "Power Plants per 10,000 Resident in Census Tract", y = "Count of Census Tracts") +
#     theme_1
#   }, bg = "transparent")
# 
# # Plot on EJ
# output$pp_pop_ej = renderPlot({
#   plants_per_pop %>%
#     filter(EJ_OR_NOT == TRUE) %>%
#     ggplot(aes(x = plants_per_10k)) +
#     geom_histogram( fill = "#22577a", color = "white") +
#     theme_classic() +
#     facet_wrap(~fossil_fuel) +
#     labs(title = "Distribution of Power Plants per 10,000 People",
#          subtitle = "For each type of power plants in enviromental Justice Areas",
#          x = "Power Plants per 10,000 Resident in Census Tract", y = "Count of Census Tracts") +
#     theme_1
# }, bg = "transparent")
# 
  
  ###================================ AQ + EJ ===============================###
  
  # 
  # output$pp_aq_ej <- renderLeaflet({
  #   leaflet() %>%
  #     addPolygons(data = mn_tracts,
  #                 color = "black",
  #                 fillOpacity = 0,
  #                 weight = 0.5) %>%
  #     addPolygons(
  #       data = ej_sf,
  #       fillColor = ~ifelse(EJ_OR_NOT, "darkgreen", "gray"),
  #       fillOpacity = 0.7,
  #       color = "white",
  #       weight = 0.15
  #     ) %>%
  #     addCircleMarkers(
  #       data = AirData_sf, #%>% filter(year == 2015),
  #       radius = .5,
  #       color = "blue",
  #       fill = TRUE,
  #       fillOpacity = 1
  #     ) %>% 
  #   # addCircleMarkers(
  #   #   data = fossil_power_plants,
  #   #   lng = ~longitude,
  #   #   lat = ~latitude,
  #   #   radius = 1.75,
  #   #   fillOpacity = 0.75,
  #   #   opacity = 0.1,
  #   #   color = "red")%>%
  #     addLegend(
  #       position = "topright",
  #       title = "AQ Monitors & EJ tracts",
  #       colors = c("darkgreen", "blue"),
  #       labels = c(
  #         "EJ Tract",
  #         "AQ Monitor"
  #       ))
  #   
  # })
}


