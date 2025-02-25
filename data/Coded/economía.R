# global.R
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(scales)    
library(shinyjs) 
library(DT)   
library(bslib)
# ui.R
economia <- 
card(
 card_header('Mapa de la ciudad'),
 card_body(
   layout_sidebar(
    sidebar = sidebar(
      checkboxGroupInput("map_gender", "Filtrar por Género:",
                       choices = NULL),
      checkboxGroupInput("map_age", "Filtrar por Edad:",
                      choices = NULL)
    ),
    plotlyOutput('razonmap')
  )
)
)
      
