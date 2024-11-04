library(shiny)
library(leaflet)  # Ensure to include required packages


movementMap_ui <- 
    tabPanel("Plots",
      sidebarLayout(
        sidebarPanel(width=3,
          fluidRow(
            # make hr() black
            tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
            # play button size
            tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important;}")),
            # shift play button to left and pad away from slider
            tags$head(tags$style(type='text/css', ".slider-animate-container { text-align: left; margin-top: -2px}")),
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
          fluidRow(HTML("<b>Plot description.</b>"), htmlOutput("plot_desc"),br(),textOutput("data_desc"))
        ),
        mainPanel(width=9,
          tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max{visibility: hidden !important;}'))),
          tabsetPanel(id="tabs",
            tabPanel("Individual Movements", mainPanel(leafletOutput("map1",width="71vw",height="87vh"))),
            tabPanel("Seasonal Distribution", mainPanel(leafletOutput("map2",width="71vw",height="87vh")))
          )
        )
      )
    )


# movementMape_ui <- tabPanel("Plots",
#   includeCSS("www/css/main.css"),
#   includeCSS("www/css/base.css"),
#   sidebarLayout(
#     sidebarPanel(width=3,
#       fluidRow(
#         tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
#         tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important;}")),
#         tags$head(tags$style(type='text/css', ".slider-animate-container { text-align: left; margin-top: -2px}")),
#         tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
#         h1("Plot settings"),
#         hr(),
#         radioButtons("map_choice",
#           label = "Select Map to Display:",
#           choices = c("Individual Movements" = "map1", "Seasonal Distribution" = "map2"),
#           selected = "map1", # Default selection
#           inline = TRUE
#         ),
#         radioButtons("basemap", "Select basemap.", choices = c("Terrain", "Satellite"), selected = "Terrain"),
#         actionButton("center_map", "Re-center Map"),
#         checkboxInput("receivers", label = HTML("<b>Show fixed-station receivers.</b>"), value = FALSE),
#         uiOutput("choiceUI"),
#         uiOutput("lifestageUI"),
#         sliderTextInput("anim_speed", "Select animation speed.", choices = c("Slowest", "Slower", "Faster", "Fastest"), 
#                         selected = "Faster", grid = TRUE),
#         uiOutput("dateUI"),
#       ),
#       fluidRow(HTML("<b>Plot description.</b>"), htmlOutput("plot_desc"), br(), textOutput("data_desc"))
#     ),
#     mainPanel(width=9,
#       tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max{visibility: hidden !important;}'))),
#       # Conditional rendering of maps based on selection
#       leafletOutput("map", width="71vw", height="87vh")
#     )
#   )
# )
