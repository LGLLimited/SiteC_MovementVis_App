library(shiny)
library(leaflet)  # Ensure to include required packages


movementMap_ui <- 
 tabPanel("Plots",
 lapply(list.files("www/css", pattern = "\\.css$", full.names = TRUE), includeCSS),
 #lapply(list.files("www/js", pattern = "\\.js$", full.names = TRUE), includeScript),
  sidebarLayout(
     sidebarPanel(width=3, class="moveMapSideBar",
       fluidRow(
         tags$head(tags$style(HTML(" hr {border-top: 0.5px solid #000000;}"))),
         # play button size
         tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 12pt !important;}")),
         # shift play button to left and pad away from slider
         tags$head(tags$style(type='text/css', ".slider-animate-container { text-align: left; margin-top: -2px; padding:1}")),
         # remove minor ticks on animation slider
         tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
         h1("Plot settings"),
         hr(),
         radioButtons("basemap", "Select basemap.", choices = c("Terrain", "Satellite"), selected = "Terrain"),
         actionButton("center_map", "Re-center Map"),
         checkboxInput("receivers",label=HTML("<b>Show fixed-station receivers.</b>"),value=FALSE),
         #br(),
         #br(),
         uiOutput("choiceUI"),
         uiOutput("lifestageUI"),
         #br(),
         sliderTextInput("anim_speed","Select animation speed.",choices = c("Slowest","Slower","Faster","Fastest"), 
                         selected ="Faster",grid = TRUE),
         #br(),
         uiOutput("dateUI"),
       ),
       fluidRow(HTML("<b>Plot description.</b>"), tags$div(style = "font-size: 12px;",htmlOutput("plot_desc")),
       br(),tags$div(style = "font-size: 12px;",textOutput("data_desc")))
     ),
     mainPanel(width=9, #Size of map controled by main.css .leafleft-container
       tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max{visibility: hidden !important;}'))),
       tabsetPanel(id="tabs",
            tabPanel("Individual Movements", mainPanel(leafletOutput("map1",width="71vw",height="87vh"))),
            tabPanel("Seasonal Distribution", mainPanel(leafletOutput("map2",width="71vw",height="87vh")))
       )
     )
   )
 )
