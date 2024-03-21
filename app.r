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
#source("gather_data.r") # the data processing doesn't need to  be done every time the app loads, only when operational data are updated.
load("data/app_data.rda", envir = .GlobalEnv) # load the environment with objects needed instead of processing every time at startup.

# Source the app
source("functions.r")
source("ui_code.r")
source("server_code.r")

# Run the application 

#runApp() - Use this if images won't load. 
shinyApp(ui = ui, server = server)