
# Libraries
library(shiny)
library(bslib)
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

  fluidRow(column(width = 12, h3(style = "text-align:center; font-size:18px;", context))),
  
  ##=================Power Plants=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("Power Plants in Minnesota")),
  
  column(12, align = "center", plotOutput(outputId = "pp_type_barplot", height = 400, width = 600)),
  
  column(12, align = "center", plotOutput(outputId = "pp_type_by_mw_barplot", height = 400, width = 600)),
  
  column(12, align = "center", plotOutput(outputId = "pp_dates_barplot", height = 400, width = 600)),
 
  
  ##=================Air Quality=================##
  
  
  
  ##=================Data=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("About Our Data")),

  fluidRow(column(width = 12, h3(style = "text-align:center; font-size:18px;", data_intro))),
  
  div(
    style = "text-align: center;",
    tags$img(
      src = "data_sources.png",
      alt = "logos for EIA, EPA, US Census Bureau, MPCA, and MN Department of Health",
      width = 650,
      height = 250
    ),
    tags$p(
      "Our Data Souces"
    )
  )

  ) # closing UI