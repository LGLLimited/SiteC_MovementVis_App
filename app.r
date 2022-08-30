library(tidyverse)
library(lubridate)
library(sf)
library(shiny)
library(leaflet)
library(shinyWidgets)
library(htmlwidgets)
library(leaflegend)
#library(reactlog)

# Get the underlying data
source("gather_data.r")

# Source the app
source("functions.r")
source("ui_code.r")
source("server_code.r")

# Run the application 

shinyApp(ui = ui, server = server)


