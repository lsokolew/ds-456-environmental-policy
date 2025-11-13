
# Libraries
library(shiny)
library(bslib)
library(leaflet)
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  ###=========================theme=========================###
  
  # theme
  theme = bs_theme(
    # background color
    bg = light_blue,
    # font color
    fg = "#0C181F",
    base_font = font_google("Tinos"),
    code_font = font_google("JetBrains Mono")
  ),
  
  ###=========================Introduction=========================###
  

    # Application title
    br(),
    br(), 
    br(),
    br(),
    h1(style = "text-align:center; font-size:80px;", strong("Power Plants in Minnesota")),
    h2(style = "text-align:center; font-size:22px;", "Where are they? Whom do they impact? How do they impact people?"),
    h2(style = "text-align:center; font-size:13px;", "By: Alicia Severiano Perez, Sydney Ohr, and Lilabeth Sokolewicz"),
    

    # main
      column(12, align = "center", leafletOutput(outputId = 'map', height = 400, width = 600)),
      column(12, align = "center", sliderInput('numeric', 'Year', min = 1906, max = 2025, value = c(1907), sep = "")),


  ##=================Context=================##
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(context)),
  
  
  ##=================Power Plants=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("Power Plants in Minnesota")),

  column(12, align = "center", plotOutput(outputId = "pp_type_barplot", height = 400, width = 600)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_type_by_mw_barplot", height = 400, width = 600)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_dates_barplot", height = 400, width = 600)),
 
  
  ##=================Air Quality=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("Air Quality")),
  
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(aq_blurb)),
  br(),
  
  column(12, align = "center", leafletOutput(outputId = 'aq_choropleth_by_year', height = 400, width = 600)),
  column(12, align = "center", sliderInput('year_for_aq_viz', 'Year', min = 2008, max = 2021, value = c(2008), sep = "")),
  
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(interactive_aq_plot_descrip)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pm_by_pp_type", height = 400, width = 600)),
  column(12, align = "center", plotOutput(outputId = "ozone_by_pp_type", height = 400, width = 600)),
  
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(aq_line_plot_descrip)),
  br(),
  
  ##=================Health=================##
  
  br(),
  br(),
  
  h2(style = "text-align:center; font-size:22px;", strong("Asthma and Power Plants")),
  
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(health_blurb)),
  
  column(12, align = "center", leafletOutput(outputId = "asthma_map", height = 400, width = 800)),
  
  
  ##=================EJ areas=================##
  br(),
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("Demographics")),
  
  # DIV 1: choose how the text should be formated
  div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
      
      # highlight the source 
      HTML(paste0(
        "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
        ej_areas))), # close div
  
  h2(style = "text-align:center; font-size:18px;", "Fossil Fuel Power Plants vs Enviromental Justice Areas"),
  
  column(12, align = "center", leafletOutput(outputId = 'pp_ej_ff', height = 400, width = 600)),
  br(),
  br(),

  h2(style = "text-align:center; font-size:18px;", "Renewable Energy Power Plants vs Enviromental Justice Areas"),
  column(12, align = "center", leafletOutput(outputId = 'pp_ej_re', height = 400, width = 600)),
  br(),
  br(),
  br(),
  h2(style = "text-align:center; font-size:18px;", "Distribution of Power Plants"),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_count_all", height = 400, width = 600)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_count_ej", height = 400, width = 600)),
  br(),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_pop_all", height = 400, width = 600)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_pop_ej", height = 400, width = 600)),
  
  
  ##=================Data=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px; font-weight:900;",strong("About Our Data")),
  
  # DIV 1: choose how the text should be formated
    div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
    
    # highlight the source 
    HTML(paste0(
      "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
      data_intro))), # close div
  
  
  # DIV 2: data source logos
    div(style = "text-align: center;",
    tags$img(
      src = "data_sources.png",
      alt = "logos for EIA, EPA, US Census Bureau, MPCA, and MN Department of Health",
      width = 650,
      height = 250), # close ta
    
    br(),
    
    tags$p(
      style = "font-family: 'Tinos', serif;font-size: 16px;color: #4a4a4a; margin-top: 5px;",
      "Our Data Sources"))# close div
  
) # closing UI