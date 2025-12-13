# Libraries
library(shiny)
library(bslib)
library(leaflegend)
library(sf)
library(tigris)
library(scales)
library(gganimate)


options(tigris_use_cache = TRUE)


# Sourcing ui to know what plot is being called
source("global.R")
source("ui.R")

server = function(input, output, session){
  
###================================ Opening ===============================###

  output$animated_map <- renderImage({
    list(
      src = "www/animations/powerplants_animation.gif",
      contentType = "image/gif",
      width = 600,
      height = 500
    )
  }, deleteFile = FALSE)
  
  
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
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
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
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
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
      theme_1 +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    }, bg = "transparent")
  
  ###================================ Air Quality ===============================###

  # Map the Buffers
  
  output$monitor_buffers = renderLeaflet({
    pal_buffers <- colorNumeric(
      palette = "Blues",        # built-in blue gradient
      domain = air_buffers$nearby_pp_count
    )
    
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
      addPolygons(
        data = tribal_shp_wgs,
        color = "darkgreen",         
        fillOpacity = 0.3,     
        weight = 1,           
      ) %>%
      setView(lng = -93.265, lat = 44.9778, zoom = 9) %>%
      # --- 3-mile buffers around monitors ---
      addPolygons(
        data = air_buffers %>% group_by(site_num) %>% slice(which.max(nearby_pp_count)),
        fillColor = ~pal_buffers(nearby_pp_count),
        fillOpacity = 0.5,
        color = "darkblue",
        weight = 1,
        label = ~paste0("Monitor: ", local_site_name, " - ", nearby_pp_count, " plants nearby")
      ) %>%
      # --- Power plants ---
      addCircleMarkers(
        data = mn_pp_sf,
        radius = 2,
        color = "black",
        fillOpacity = 0.8,
        label = ~paste0("Power Plant: ", plant_name, " (", prim_source, ")")
      ) %>%
      # --- Air monitors ---
      addCircleMarkers(
        data = AirData_sf,
        radius = .5,
        color = "darkblue",
        fill = TRUE,
        fillOpacity = 1
      ) %>%
      addLegend(
        position = "topright",
        title = "What's in the Twin Cities?",
        colors = c(
          "darkgreen",   # EJ areas
          "black" ,  # Power plants
          "lightblue",   # Monitor buffers
          "darkblue"    # Air monitors
        ),
        labels = c(
          "Environmental Justice Area",
          "Power Plant",
          "3-mile Buffer",
          "Air Monitor"
        ),
        opacity = 1
      ) 
  })
  
  # plot the pollutant concentrations grouped by number of nearby plants

  output$grouped_summ_lineplot = renderPlot({
    ggplot(grouped_summ_pm25_allyears) +
      geom_line(aes(x = year, y = avg_pm25_grouped, group = fct_rev(as.factor(grouped_nearby_pp_count)), color = fct_rev(as.factor(grouped_nearby_pp_count)))) +
      geom_hline(yintercept = 9, linetype = "dashed", color = "darkgray") +
      annotate("text", x = 2015,
               y = 9.3, hjust = 0,
               label = "National Standard",
               color = "darkgray") +
      labs(x = "Year", y = "Average PM2.5 Concentration (µg/m3)",
         color = "Number of Plants \nNear Monitor",
         title = "Air Monitors Near More Plants Report Higher Pollutant Concentrations") +
      ylim(0, 12) +
      theme_minimal()
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
      geom_hline(yintercept = 9, linetype = "dashed", color = "darkgray") +
      annotate("text", x = 2015,
               y = 9.3, hjust = 0,
               label = "National Standard",
               color = "darkgray") +
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
  
  mn_powerplants_nonrenewable <- mn_powerplants %>% filter(fossil_fuel == "Fossil Fuel")
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
         label = ~paste0(
           "Zipcode: ", ZCTA5CE, "<br>",
           "Rate: ",
           ifelse(is.na(A.rp10.),
                  "Not given due to small population",
                  A.rp10.)
         ),
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
        colors = "red",
        labels = "Fossil Fuel Plant",
        opacity = 0.7,
        position = "bottomright"
      ) %>%
      addCircleMarkers(
        data = mn_powerplants_nonrenewable,
        lng = ~longitude,
        lat = ~latitude,
        color = "red",
        radius = 3,
        fillOpacity = 1,
        label = ~paste0(
          "Plant Name: ", plant_name, "<br>",
          "Plant Code: ", plant_code
        )
      )
  })
  
  ###================================ Schools ===============================###
  if (is.na(st_crs(schools_sf))) { # Only set CRS if missing
    schools_sf <- st_set_crs(schools_sf, 26915)}
  
  points_sf_crs <- st_as_sf(
    mn_powerplants,
    coords = c("longitude", "latitude"),
    crs = 4326) %>%
    st_transform(26915) 
  
  schools_sf <- schools_sf %>%
    filter(is.na(GRADERANGE))
  
  school_proj <- st_transform(schools_sf, 26915)
  
  # 1 mile buffer around schools
  school_buffer <- st_buffer(school_proj, dist = 1609.34)
  
  school_pp_dist <- st_distance(school_proj, points_sf_crs)
  
  # schools within 1 mile of any power plant
  school_proj$within_1mile_pp <- apply(school_pp_dist, 1, function(x) any(x <= 1609.34))
  table(school_proj$within_1mile_pp)

  # New Line- ALICA
  school_proj <- st_transform(school_proj, st_crs(ej_sf))
  
  schools_in_ej <- st_within(school_proj, ej_sf)
  
  school_proj$schools_in_ej <- lengths(schools_in_ej) > 0
  
  schools_near_pp_and_in_ej <- school_proj %>%
    filter(within_1mile_pp == TRUE & schools_in_ej == TRUE)
  nrow(schools_near_pp_and_in_ej)
  
  nearest_pp_index <- apply(school_pp_dist, 1, which.min)
  
  school_proj$nearest_pp_id <- points_sf_crs$plant_code[nearest_pp_index]
  school_proj$nearest_pp_dist_m <- apply(school_pp_dist, 1, min)
  school_proj$nearest_pp_dist_mi <- school_proj$nearest_pp_dist_m / 1609.34
  
  schools_near_pp_and_in_ej <- school_proj %>%
    filter(within_1mile_pp & schools_in_ej)
  
  distinct_schools <- school_proj %>% 
    distinct(GISADDR, .keep_all = TRUE)

  distinct_schools_metro <- distinct_schools %>% 
    mutate(zip_code = str_extract(GISADDR, "\\b\\d{5}(?:-\\d{4})?(?=\\D|$)")) %>% 
    filter(zip_code %in% metro_zips) 
  
  output$school_pp_plot = renderPlot({
    distinct_schools_metro %>%
      ggplot(aes(x = schools_in_ej, y = nearest_pp_dist_mi)) +
      geom_boxplot() +
      labs(title = "Distance to Nearest Power Plant by EJ Status",
           x = "Environmental Justice Area", y = "Distance (miles)") +
      stat_summary(fun = median, geom = "point", size = 2, color = "red")+
      theme_1 +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  }, bg = "transparent")
  

###================================ EJ ===============================###

 # EJ Areas
  output$pp_ej_ff = renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = ej_sf,
        fillColor = ~pal1(EJ_area),
        fillOpacity = 0.7,
        color = "white",
        weight = 0.15
      ) %>%
      addPolygons(
        data = tribal_shp_wgs,
        color = "red",         
        fillOpacity = 0.3,    
        weight = 1,            
      ) %>%
      addLegend(
        pal = pal1, values = ej_sf$EJ_area, title ="Enviromental Justice Area")  %>%
      setView(lng = -94.6, lat = 46.4, zoom = 6) 
  })


# # Plot renable Fuel
# 
# mn_tracts_wgs <- st_transform(mn_tracts, crs = 4326)
# ej_sf_wgs <- st_transform(ej_sf, crs = 4326)
# tribal_shp_wgs <- st_transform(tribal_shp, crs = 4326)
# 
# output$pp_ej_re <- renderLeaflet({
#   leaflet() %>%
#     addProviderTiles("CartoDB.Positron") %>%
#     addPolygons(
#       data = ej_sf,
#       fillColor = ~pal1(EJ_area),
#       fillOpacity = 0.7,
#       color = "white",
#       weight = 0.15
#     ) %>%
#     addPolygons(
#       data = tribal_shp_wgs,
#       color = "red",         
#       fillOpacity = 0.3,     
#       weight = 1,           
#     ) %>%
#     addLegend(
#       pal = pal1, values = ej_sf$EJ_area, title ="Enviromental Justice Area")  %>%
#     setView(lng = -94.6, lat = 46.4, zoom = 6) %>%
#     addCircleMarkers(data = mn_powerplants, 
#                      color = ~if_else(fossil_fuel == "Fossil Fuel", "#d95f02", "#1b9e77"), 
#                      radius = ~rescale(total_mw, to = c(1, 16)),
#                      #stroke = FALSE,
#                      label = ~paste0(plant_name, " (", prim_source, " - ", total_mw, " megawatt(s))")
#     ) %>%
#     
#     addLegend(
#       position = "topright",
#       title = "Power Plants by \nProduction (MW)",
#       colors = c("#d95f02", "#1b9e77"),
#       labels = c(
#         "Fossil Fuel",
#         "Renewable"
#       )
#     )
# 
# })


output$pp_ej_re <- renderLeaflet({
  leaflet() %>%
    setView(lng = -93.265, lat = 44.9778, zoom = 13) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      data = metro_area,
      fillColor = ~herc_cols(prppoc), 
      fillOpacity = 0.7, 
      color = "white", 
      weight = 1
    ) %>%
    addLegend(pal = herc_cols, values = metro_area$prppoc) %>%

    addPolygons(
      data = herc_buffer ,
      fillColor = "lightblue",
      fillOpacity = 0.4,
      color = "steelblue",
      weight = 1
    )  %>%
    addCircleMarkers(data = mn_powerplants %>% filter (plant_code == 10013), 
                     color = "#d95f02", radius = 1) 
  })

# choose the specific power plant

output$herc_lineplot <- renderPlot({
  # diff between avg of all air monitors in the state and air monitors just near the plant
  ggplot(all_avgs) +
    geom_line(aes(y=pm25_avg_by_year, x=year, color = id)) +
    geom_hline(yintercept = 9, linetype = "dashed", color = "darkgray") +
    annotate("text", x = 2016,
             y = 9.3, hjust = 0,
             label = "National Standard",
             color = "darkgray") +
    ylim(0,12)+
    scale_color_manual(values = c("Near HERC" = "red", "Whole State" = "black")) +
    labs(title = "PM2.5 Concentrations Are Consistently Higher Near the HERC",
         y = "Annual Average PM2.5 Concentration (µg/m3)",
         x = "Year",
         color = "Location of Monitor") +
    theme_1
}, bg = "transparent")
}