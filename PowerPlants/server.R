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
  top_NOpolluters_metro <- all_emissions_data %>%
    filter(zip %in% metro_zips) %>%
    arrange(desc(as.numeric(eia_model_estimates_of_n_ox_emissions_tons)))
  
  top_NOpolluters_metro_sf <- all_emissions_data %>%
    filter(zip %in% metro_zips) %>%
    arrange(desc(as.numeric(eia_model_estimates_of_n_ox_emissions_tons))) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  polluters <- top_NOpolluters_metro_sf %>%
    arrange(desc(eia_model_estimates_of_n_ox_emissions_tons)) %>%
    st_transform(crs = 4326)
  
  polluters_buffer <- st_buffer(polluters, dist = 1609.34)
  
  zcta_joined_children <- zcta_joined_asthma %>%
    filter(`Age group` == "0-17")
  
  zcta_joined_children <- st_as_sf(zcta_joined_asthma, crs = 4326)
  
  
  output$asthma_map <- renderLeaflet({

    # ---- Color palette for polygons ----
    pal2 <- colorFactor(
      palette = c("lightblue", "lightblue3","skyblue3", "royalblue3", "midnightblue"),
      levels = c("0-15", "15-30", "30-45", "45-60", "60+"),
      na.color = "grey"
    )

    leaflet(zcta_joined_children) %>%
      setView(lng = -93.265, lat = 44.9778, zoom = 9) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal2(value_cat),
        color = "black",
        weight = 1,
        opacity = 1,
        fillOpacity = 0.9,
        highlight = highlightOptions(
          weight = 2,
          color = "white"),
        label = ~paste0(
          "Zipcode: ", ZCTA5CE20, "\n",
          "Rate: ", ifelse(is.na(`Age-adjusted rate per 10,000`), "Not given due to small population", `Age-adjusted rate per 10,000`)),
        labelOptions = labelOptions(
          style = list("white-space" = "pre-line")
        )) %>%
      
      addLegend(
        pal = pal2,
        values = zcta_joined_children$value_cat,
        opacity = 0.9,
        title = "Asthma-related Emergency Department Visit",
        position = "bottomright",
        na.label = "Not given"
      ) %>%
      addLegend(
        colors = "red",
        labels = "Non-renewable Power Plant",
        opacity = 0.8,
        position = "bottomright"
      ) %>%
      addCircleMarkers(
        data = top_NOpolluters_metro,
        lng = ~longitude,
        lat = ~latitude,
        color = "red",
        radius = 3,
        fill = "red",
        fillOpacity = 1,
        label = ~paste0(
          "Plant Name: ", plant_name.x, "<br>",
          "Plant Code: ", plant_code)) %>%
      addPolygons(data = polluters_buffer,
                  fillColor = "transparent",
                  fillOpacity = 0.4,
                  color = "red",
                  weight = 2,
                  label = ~paste0("Plant: ", plant_name.x))
    })
  
  output$asthma_poc_plot = renderPlot({
    
    asthma_poc_powerplant %>%
      ggplot(aes(x = pct_poc, y = `Age-adjusted rate per 10,000`)) +
      geom_point() +
      facet_wrap(~near_plant, labeller = labeller(near_plant = 
                                                    c("TRUE" = "Within Mile of Power Plant",
                                                      "FALSE" = "Not Within Mile of Power Plant"))) +
      labs(title = "Asthma-related Emergency Department Visits & Percent POC by Zip Code", subtitle = "Does proximity to power plants affect asthma?", x = "Percent People of Color", y = "Emergency Department Visits") +
      theme_bw()
  }, bg = "transparent")
    
  ###================================ Schools ===============================###

  
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
      addPolygons(data = ej_sf, fillColor = "blue",
                  fillOpacity = 0.7,color = "white", weight = 0.15) %>%
      addPolygons(data = tribal_shp_wgs,color = "blue",         
                  fillOpacity = 0.3, weight = 1) %>%
      addCircleMarkers(data = mn_powerplants %>% filter(fossil_fuel == "Fossil Fuel"), 
                       radius = 1, color =  "#962A0C")  %>%
      addPolygons(data = plants_buffer%>% filter(fssl_fl =="Fossil Fuel"), fillColor = "#F58766",
                  fillOpacity = 0.4, color = "#F26338",weight = 1) 
  })


# # Plot renable Fuel
# 
# mn_tracts_wgs <- st_transform(mn_tracts, crs = 4326)
# ej_sf_wgs <- st_transform(ej_sf, crs = 4326)
 tribal_shp_wgs <- st_transform(tribal_shp_wgs, crs = 4326)
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

  # choose the specific power plant (example: row 1)
  target_pp <- mn_pp_proj %>% filter(str_detect(plant_name, "Covanta"))
  # find which air buffers intersect this plant's location
  hits <- st_intersects(air_buffers_proj, target_pp)
  # keep only buffers that intersect
  air_buffers_near_target <- air_buffers_proj %>% filter(lengths(hits) > 0)


output$pp_ej_re <- renderLeaflet({
  leaflet() %>%
    setView(lng = -93.265, lat = 44.9778, zoom = 13) %>%
    addTiles() %>%
    addPolygons(data = herc_buffer , fillColor = "#F58766",
                fillOpacity = 0.4, color = "#F26338",weight = 1) %>%
    addPolygons(data = metro_area,
                fillColor = ~herc_cols(prppoc*100), fillOpacity = 0.7, 
                color = "white", weight = 1, label = ~tractce,   
                labelOptions = labelOptions(style = list("font-weight" = "bold"),
                                            textsize = "14px",direction = "auto")) %>%
    addLegend(pal = herc_cols, values = metro_area$prppoc,
              labFormat = labelFormat(suffix  = "%"),
              title = "Percent POC") %>%
  addCircleMarkers(data = mn_powerplants %>% filter (plant_code == 10013), 
                   radius = 8, color =  "#962A0C")  
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