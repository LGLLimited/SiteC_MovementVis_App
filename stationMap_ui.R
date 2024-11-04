library(shiny)

stationMap_ui <- tabPanel(
  "Map",
  titlePanel(h1("Fixed Receiver Locations", align = 'center')),
  div(h2("The fixed array in the Peace River Basin has been operated from April through October each year since 2019.", style = 'text-align: center;')),  # Changed to h2
  br(),
  # STUDY AREA MAP FROM REPORT WITH FIXED RECEIVER LOCATIONS
  div(img(src = '2021Site C Report.png', height = "55%", width = "55%"), style = "text-align: center;"),
  br(),
  br(),
  br(),
  br()
)


# If you want to include this in your server logic, you might also define a server function here
