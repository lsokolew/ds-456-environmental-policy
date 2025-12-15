
# Libraries
library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
source("global.R")


# Define UI for application that draws a histogram
ui <- navbarPage(
  ###=========================theme=========================###
  # theme
  title = "", 
  theme = bs_theme(
    # background color
    bg = light_blue,
    # font color
    fg = "#0C181F",
    base_font = font_google("Tinos"),
    code_font = font_google("JetBrains Mono")
  ),
  
  ###=============================================    Tab #1   =================================================###
  
  
  ###=========================Start Story=========================###
  
  tabPanel(
    title = "Article",
    fluidPage(
    
    # story title
      h1(style = "text-align:center; font-size:80px;", strong("Power Plants in Minnesota")),
    # main
      column(12, align = "center", imageOutput(outputId = 'animated_map', height = 400, width = 600)),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    h2(style = "text-align:center; font-size:22px;", "Where are they? Whom do they impact? How do they impact people?"),
    h2(style = "text-align:center; font-size:13px;", "By: Alicia Severiano Perez, Sydney Ohr, Lilabeth Sokolewicz"),
    br(),
    br(),

  ##=================Introduction=================##
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(intro)),
  
  ##=================Power Plants=================##

  
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("How do the Plants Work? and what types are in MN")),

  column(12, align = "center", leafletOutput(outputId = "intereactive_pp_types", height = 400, width = 600)),
  br(),
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(pp_locations)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_type_barplot", height = 400, width = 600)),
  br(),
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(pp_energy_sources)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_type_by_mw_barplot", height = 400, width = 600)),
  br(),
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(pp_elec_by_source)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "pp_dates_barplot", height = 400, width = 600)),
  br(),
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(pp_by_year)),
  br(),
  
  ##=================EJ areas=================##
  br(),
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("Where Are Minnesota's Most Vulnerable Communities in Relation to Power Plants?")),

  
  # DIV 1: choose how the text should be formated
  div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
      
      # highlight the source 
      HTML(paste0(
        "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
        ej_text))), # close div
  
  h2(style = "text-align:center; font-size:18px;", "Fossil Fuel Power Plants vs Enviromental Justice Areas"),
  
  column(12, align = "center", leafletOutput(outputId = 'pp_ej_ff', height = 400, width = 600)),
  br(),
  br(),

  column(12, align = "center", leafletOutput(outputId = 'pp_ej_re', height = 400, width = 600)),
  
  
  ##=================Air Quality=================##
  
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("How do power plants affect the air people breath?")),
  
  div(
    style = "
    max-width: 900px;
    margin: 0 auto;
    text-align: justify;
    font-size: 18px;
    font-family: 'Tinos', serif;
    color: #4a4a4a;",HTML(aq_text_1)),
  br(),
  
  column(12, align = "center", leafletOutput(outputId = 'monitor_buffers', height = 400, width = 600)),

  div(
    style = "
    max-width: 900px;
    margin: 0 auto;
    text-align: justify;
    font-size: 18px;
    font-family: 'Tinos', serif;
    color: #4a4a4a;",HTML(aq_text_2)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "grouped_summ_lineplot", height = 400, width = 600)),

  div(
    style = "
    max-width: 900px;
    margin: 0 auto;
    text-align: justify;
    font-size: 18px;
    font-family: 'Tinos', serif;
    color: #4a4a4a;",HTML(aq_text_3)),
  br(),
  
  column(12, align = "center", plotOutput(outputId = "aq_by_county_type_lineplot", height = 400, width = 600)),

  
  div(
    style = "
    max-width: 900px;
    margin: 0 auto;
    text-align: justify;
    font-size: 18px;
    font-family: 'Tinos', serif;
    color: #4a4a4a;",HTML(aq_text_4)),
  div(
    style = "
    max-width: 900px;
    margin: 0 auto;
    text-align: justify;
    padding-left: 40px;
    padding-right: 40px;
    font-size: 18px;
    font-family: 'Tinos', serif;
    color: #4a4a4a;",HTML(aq_text_5)),
  div(
    style = "
    max-width: 900px;
    margin: 0 auto;
    text-align: justify;
    font-size: 18px;
    font-family: 'Tinos', serif;
    color: #4a4a4a;",HTML(aq_text_6)),
  
  ##=================Health=================##
  
  br(),
  br(),
  
  h2(style = "text-align:center; font-size:22px;", strong("How is people's health impacted?")),
  
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(health_blurb)),
  br(),
 
  
  column(12, align = "center", leafletOutput(outputId = "asthma_map", height = 400, width = 800)),
  br(),
  div(
    style = "
    max-width: 900px; 
    margin: 0 auto; 
    text-align: justify; 
    font-size: 18px; 
    font-family: 'Tinos', serif; 
    color: #4a4a4a;",HTML(asthma_plot_one)),
  br(),
  
   column(12, align = "center", plotOutput(outputId = "asthma_poc_plot", height = 400, width = 800)),
   br(),
   div(
     style = "
     max-width: 900px; 
     margin: 0 auto; 
     text-align: justify; 
     font-size: 18px; 
     font-family: 'Tinos', serif; 
     color: #4a4a4a;",HTML(asthma_plot_two)),
  br(),
   
   column(12, align = "center", plotOutput(outputId = "school_pp_plot", height = 400, width = 600)),
  br(),
  div(
    style = "
     max-width: 900px; 
     margin: 0 auto; 
     text-align: justify; 
     font-size: 18px; 
     font-family: 'Tinos', serif; 
     color: #4a4a4a;",HTML(school_plot)),
 
  ###================================ HERC ===============================###
  br(),
  br(),
  h2(style = "text-align:center; font-size:22px;", strong("Diving into the HERC")),
  
  h2(style = "text-align:center; font-size:18px;", "Where is it located"),

  
   column(12, align = "center", leafletOutput(outputId = 'herc_map', height = 400, width = 600)),
  
  column(12, align = "center", plotOutput(outputId = 'herc_lineplot', height = 400, width = 600)),
  
  h2(style = "text-align:center; font-size:22px;", strong("So what now?")),
  
    ) # closing fluidpage
  ), # Closing tab #1

###=============================================    Tab #2  =================================================###

tabPanel(
  title = "Methods & Sources",
  fluidPage(
    ##=================Data=================##
    h2(style = "text-align:center; font-size:22px; font-weight:900;",strong("About Our Data & Our Work")),
    
    # DIV 1: choose how the text should be formated
    div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
        
        # highlight the source 
        HTML(paste0(
          "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
          pp_data_methods))), # close div
    
    br(),
    br(),
    
    # DIV 1: choose how the text should be formated
    div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
        
        # highlight the source 
        HTML(paste0(
          "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
          ej_data_methods))), # close div
    
    br(),
    br(),
    
    # DIV 1: choose how the text should be formated
    div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
        
        # highlight the source 
        HTML(paste0(
          "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
          aq_data_methods))), # close div
    
    br(),
    br(),
    
    # DIV 1: choose how the text should be formated
    div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
        
        # highlight the source 
        HTML(paste0(
          "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
          health_data_methods))), # close div
    
    br(),
    
    # DIV 1: choose how the text should be formated
    div(style = "max-width: 900px;  margin: 0 auto; text-align: justify;  font-size: 18px; 
      font-family: 'Tinos', serif; color: #4a4a4a;",
        
        # highlight the source 
        HTML(paste0(
          "<style>  b { 
        background-color: #FFF59D;  /* soft yellow highlight */
        color: #000000;font-weight: 700;  padding: 1px 3px; border-radius: 2px;} </style>",
          data_methods_conclusion))), # close div
    
    br(),
    
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
          "Our Data Sources")), # close div
    
    ###================================ End Matter ===============================###
    
    br(),
    h2(style = "text-align:center; font-size:22px; font-weight:900;",strong("Acknowlegements")),
    
    div(
      style = "max-width: 900px; 
               margin: 0 auto; 
               text-align: justify; 
               font-size: 18px; 
               font-family: 'Tinos', serif; 
               color: #4a4a4a;",
      HTML(acknowledgements)
      ), #  close div
    
    br(),
    h2(style = "text-align:center; font-size:22px; font-weight:900;",strong("AI Statement")),
    
    div(
      style = "max-width: 900px; 
               margin: 0 auto; 
               text-align: justify; 
               font-size: 18px; 
               font-family: 'Tinos', serif; 
               color: #4a4a4a;",
      HTML(ai_statement)
    ), #  close div
    
    ) #closing navpage
  ), # closing tab #2

###=============================================    Tab #3  =================================================###

tabPanel(
  title = "References",
  fluidPage(
    includeHTML("references.html")
    ) #closing navpage
  ) # closing tab #3

) # closing UI