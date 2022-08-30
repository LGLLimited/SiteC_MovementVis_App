# UI ####
ui <- fluidPage(titlePanel(title=img(src="BC-Hydro-Logo.png",height="56px",width="100px",style="float: right;"),
                           windowTitle="Site C Fish Movement"),
                tabsetPanel(id="main",
                            tabPanel("Data description",
                                     fluidPage(
                                       titlePanel(h1("Site C Fish Movement Assessment",align='center')),
                                     fluidRow(column(width=4, data_description()),
                                     column(width=8,img(src="Project.png",width="50%",style="float: right; padding-right: 200px",title="Site C Project"))),
                                     br(),
                                     fluidRow(column(width=4),
                                     column(width=8,img(src="Antenna.png",title="A fixed-station antenna on the Peace River.", width="50%",style="float: right; padding-right: 200px"))))),
                            # TEXT OUTPUT FOR WELCOME PAGE
                            tabPanel("Map",
                                     titlePanel(h1("Fixed Receiver Locations in the Peace River Basin",align='center')),
                                     div("The fixed array has been operated from April through October each year since 2019.",style='text-align: center;'), 
                                     br(), 
                                     div(img(src='2021Site C Report.png',height="55%",width="55%"),style="text-align: center;")), # STUDY AREA MAP FROM REPORT WITH FIXED RECEIVER LOCATIONS
                            tabPanel("Plots",
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                        # make hr() black
                                       tags$head(tags$style(HTML("hr {border-top: 1px solid #000000;}"))),
                                       # remove minor ticks on animation slider
                                       tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                                       h1("Plot settings"),
                                       hr(),
                                       radioButtons("basemap", "Choose basemap.", choices = c("Terrain", "Satellite"), selected = "Terrain"),
                                       actionButton("center_map", "Re-center Map"),
                                       br(),
                                       br(),
                                       uiOutput("choiceUI"),
                                       uiOutput("lifestageUI"),
                                       br(),
                                       sliderTextInput("anim_speed","Choose animation speed.",choices = c("Slowest","Slower","Faster","Fastest"), selected ="Faster",grid = TRUE),
                                       br(),
                                       uiOutput("dateUI"),
                                       ),
    mainPanel(
      tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-min, .irs-max{visibility: hidden !important;}'))),
      tabsetPanel(id="tabs",
        tabPanel("Individual Movements",
                mainPanel(leafletOutput("map1",height="85vh",width = "85vh"))),
        tabPanel("Seasonal Distribution",
                mainPanel(leafletOutput("map2",height="85vh",width = "85vh")))
                )
             )
        )
    )
)
)