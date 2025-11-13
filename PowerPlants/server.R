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
      theme_1    

  }, bg = "transparent")
  
  output$pp_type_barplot = renderPlot({
    
    mn_powerplants %>% 
      ggplot(aes(x = fct_infreq(prim_source))) +
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
      labs(x = "Energy Source", y = "Megawatts Electricity Produced", title = "Energy produced by Source in MN Powerplants",
           fill = "Powerplant Type") +  
      theme_classic() +
      theme_1 
    }, bg = "transparent")
  
  ###================================ Air Quality ===============================###

  
  # ----Reactive Plot------
  filtered_mn_aq = reactive({
    mn_counties_aq %>% filter(year <= input$year_for_aq_viz, pollutant == "PM2.5") 
    })
  filtered_mn_powerplants = reactive({
    mn_powerplants %>% 
      filter(first_op_year <= input$year_for_aq_viz, is.na(last_retire_year) | last_retire_year > input$year_for_aq_viz) 
    })

  
  output$aq_choropleth_by_year = renderLeaflet({
    
    pal_pp <- colorFactor(
      c("black",
        "darkgreen"),
      domain = filtered_mn_powerplants()$fossil_fuel
    )
    
    pal_aqi <- colorNumeric("YlOrRd", domain = c(4, 12))
    
    leaflet() %>%
      # putting layer of counties
      addPolygons(
        data = filtered_mn_aq(),
        fillColor = ~ pal_aqi(average_concentration),
        weight = 1,
        color = "black",
        fillOpacity = .6,
        group = "Counties"
      ) %>%
      # putting circles for each power plant
      addCircleMarkers(
        data = filtered_mn_powerplants(),
        lng = ~ longitude,
        lat = ~ latitude,
        radius = 0.25,
        color = ~pal_pp(fossil_fuel),
        opacity = 0.35
      ) %>% 
      addLegendFactor(
        pal = pal_pp,
        values = filtered_mn_powerplants()$fossil_fuel,
        title = "Power Plants: Energy Type",
        orientation = "horizontal",
        position = "topright",
        width = 12,
        height = 12,
        opacity = 0.75
      )  %>% 
      addLegend(
        pal = pal_aqi,
        values = c(4, 12),
        title = str_c("PM2.5", " Concentration (", "µg/m3", ")"),
        opacity = 0.75
      ) 
  })
  
  # ----Static Plot-----
  
  ff_status <- mn_powerplants %>%
    group_by(county) %>%
    summarize(has_fossil = any(fossil_fuel == "Fossil Fuel")) %>%
    mutate(plant_group = ifelse(has_fossil, "At Least One", "Only Renewable/None"))
  
  output$pm_by_pp_type = renderPlot({
    mn_counties_aq %>%
      filter(pollutant == "PM2.5") %>%
      left_join(ff_status, by = c("name" = "county")) %>%
      mutate(plant_group = ifelse(is.na(plant_group), "Only Renewable/None", plant_group)) %>%
      ggplot(aes(x = year, y = average_concentration, group = name, color = plant_group)) +
      geom_line(alpha = 0.2) + # faint individual counties
      stat_summary(aes(group = plant_group), fun = mean, geom = "line", size = 1.5) + # bold mean trend
      labs(
        title = "Counties With Fossil Fuel Plants Tend to Have Worse Air Quality",
        color = "Fossil Fuel Plant(s)?",
        y = "Average PM2.5 Concentration by County (µg/m3)",
        x = "Year"
      ) +
      ylim(0, 12) +
      scale_color_manual(values = c("At Least One" = "#d95f02",
                                    "Only Renewable/None" = "#1b9e77")) +
      theme_classic()
  }, bg = "transparent")
    
  output$ozone_by_pp_type = renderPlot({
    mn_counties_aq %>%
      filter(pollutant == "Ozone") %>%
      left_join(ff_status, by = c("name" = "county")) %>%
      mutate(avg_conc_jittered = average_concentration + runif(n(), min=0, max=.5)) %>% # jitter so lines don't cover each other
      mutate(plant_group = ifelse(is.na(plant_group), "Only Renewable/None", plant_group)) %>%
      ggplot(aes(x = year, y = avg_conc_jittered, group = name, color = plant_group)) +
      geom_line(alpha = 0.15) + # faint individual counties
      stat_summary(aes(group = plant_group), fun = mean, geom = "line", size = 1) + # bold mean trend
      labs(
        title = "",
        color = "County Power Plants",
        y = "Average Ozone Concentration by County (ppb)",
        x = "Year"
      ) +
      ylim(0, 42) +
      scale_color_manual(values = c("At Least One" = "#d95f02",
                                    "Only Renewable/None" = "#1b9e77")) +
      theme_classic() 
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
        fillColor = ~pal(value_cat),
        color = "black",
        weight = 1,
        fillOpacity = 0.7,
        opacity = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "white"
        ),
        label = ~paste0(
          "Zipcode: ", ZCTA5CE20, "<br>",
          "Rate: ",
          ifelse(is.na(`Age-adjusted rate per 10,000`),
                 "Not given due to small population",
                 `Age-adjusted rate per 10,000`)
        ),
        labelOptions = labelOptions(
          style = list("white-space" = "pre-line")
        )
      ) %>%
      addLegend(
        pal = pal,
        values = zcta_joined$value_cat,
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

# Plot Fossil Fuel 

output$pp_ej_ff <- renderLeaflet({
leaflet() %>%
  addPolygons(data = mn_tracts,
              color = "black",
              fillOpacity = 0,
              weight = 0.5) %>%
  addPolygons(
    data = ej_sf,
    fillColor = ~pal1(EJ_area), 
    fillOpacity = 0.7, 
    color = "white", 
    weight = 0.15
  ) %>%
  addLegend(
    pal = pal1, values = ej_sf$EJ_area, title ="Enviromental Justice Area")  %>%
  addCircleMarkers(
    data = fossil_power_plants,
    lng = ~longitude,
    lat = ~latitude,
    radius = 1.75,         
    fillOpacity = 0.75,  
    opacity = 0.1,      
    color = "#000000") 
  
})


# Plot renable Fuel 

output$pp_ej_re <- renderLeaflet({ 
  leaflet() %>%
  addPolygons(data = mn_tracts,
              color = "black",
              fillOpacity = 0,
              weight = 0.5) %>%
  addPolygons(
    data = ej_sf,
    fillColor = ~pal1(EJ_area), 
    fillOpacity = 0.7, 
    color = "white", 
    weight = 0.15
  ) %>%
  addLegend(
    pal = pal1, values = ej_sf$EJ_area, title ="Enviromental Justice Area")  %>%
  addCircleMarkers(
    data = Renewable_power_plants,
    lng = ~longitude,
    lat = ~latitude,
    radius = 1.75,         
    fillOpacity = 0.75,  
    opacity = 0.1,      
    color = "#000000")  
  
})

## Counts of power plants per census tracts

output$pp_count_all = renderPlot({

  plants_in_ej_counts %>%
    ggplot(aes(x = plant_count)) +
    geom_histogram(binwidth = 1, fill = "#c44900", color = "white") +
    theme_classic() +
    facet_wrap(~fossil_fuel + EJ_OR_NOT) +
    labs(title = "Distribution of Power Plants per Census Tract",
         subtitle = "Comparison by energy type and Environmental Justice area status",
         x = "Number of Power Plants per Census Tract", y = "Number of Census Tracts") +
    theme_1
    }, bg = "transparent")


  output$pp_count_ej = renderPlot({
    
    plants_in_ej_counts %>%
    filter(EJ_OR_NOT == TRUE) %>%
    ggplot(aes(x = plant_count)) +
    geom_histogram(binwidth = 1, fill = "#22577a", color = "white") +
    theme_classic() +
    facet_wrap(~fossil_fuel) +
    labs(title = "Distribution of Power Plants per Census Tract",
       subtitle = "Comparison by energy type for Enviromental Justice Areas",
       x = "Number of Power Plants per Census Tract", y = "Number of Census Tracts") +
      theme_1
    }, bg = "transparent")
  
## Populations to where power plants are at

# Plot all

output$pp_pop_all = renderPlot({
  
  plants_per_pop %>%
    ggplot(aes(x = plants_per_10k)) +
    geom_histogram( fill = "#c44900", color = "white") +
    theme_classic() +
    facet_wrap(~fossil_fuel + EJ_OR_NOT) +
    labs(title = "Distribution of Power Plants per 10,000 People",
         subtitle = "For type of power plants and environmental justice designation",
         x = "Power Plants per 10,000 Resident in Census Tract", y = "Count of Census Tracts") +
    theme_1
  }, bg = "transparent")

# Plot on EJ
output$pp_pop_ej = renderPlot({
  plants_per_pop %>%
    filter(EJ_OR_NOT == TRUE) %>%
    ggplot(aes(x = plants_per_10k)) +
    geom_histogram( fill = "#22577a", color = "white") +
    theme_classic() +
    facet_wrap(~fossil_fuel) +
    labs(title = "Distribution of Power Plants per 10,000 People",
         subtitle = "For each type of power plants in enviromental Justice Areas",
         x = "Power Plants per 10,000 Resident in Census Tract", y = "Count of Census Tracts") +
    theme_1
}, bg = "transparent")

}