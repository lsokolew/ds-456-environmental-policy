
# Libraries
library(shiny)
library(bslib)
library(leaflet)
source("set_code.R")

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
 
  ##=================EJ areas=================##
  
  
  
  ##=================Air Quality=================##
  
  
  
  
  ##=================Health=================##
  
  
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
  
  ##=================Data=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px; font-weight:900;",strong("About Our Data")),
  
  # choose how the text should be formated
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;
  ",
    # highlight the source 
    
    HTML(paste0(
      "<style> 
      b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;
        font-weight: 700;
        padding: 1px 3px;
        border-radius: 2px;
      } 
    </style>",
      data_intro
    ))
  ),
  
  # data source logos
  div(
    style = "
    text-align: center;
  ",
    tags$img(
      src = "data_sources.png",
      alt = "logos for EIA, EPA, US Census Bureau, MPCA, and MN Department of Health",
      width = 650,
      height = 250
    ), # close ta
    br(),
    tags$p(
      style = "
      font-family: 'Tinos', serif;
      font-size: 16px;
      color: #4a4a4a;
      margin-top: 5px;
    ",
      "Our Data Sources") # close tag
  )# close div
  
) # closing UI