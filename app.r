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

# functions.R #########################################################################################################
########################################################################################################################
########################################################################################################################
make_map <- function(basemap, peace_network, location_pts){
  leaflet(options=leafletOptions(minZoom=7, maxZoom=11, zoomControl=FALSE)) %>%
    onRender("function(el,x) {
             L.control.zoom({ position:'bottomright' }).addTo(this)}") %>% 
    setView(lng=-120.9141, lat=  56.19851, zoom=8.4) %>%
    setMaxBounds(lng1=-124.12656439,
              lat1=57.64047692,
              lng2=-117.88295361,
              lat2=54.27808912) %>%
    addLegend(colors=paste0(c('white','black'),";border:2px solid black; border-radius: 50%; width:",12,"px; height:",12,"px;"),
              title = "Site C",
              labels=c("Pre-Diversion","Diverted"),
              opacity=1,
              position = "topright") %>% 
    addProviderTiles(isolate(basemap()),layerId = 'base') %>%
    addPolylines(data = peace_network,
                 opacity=case_when(isolate(basemap())=="Esri.WorldImagery"~0.5,
                                   TRUE ~ 0.3),
                 label = ~StreamName,
                 weight = ~if_else(lwd=="Peace",8,2),
                 labelOptions = labelOptions(textOnly = TRUE, noHide = FALSE,textsize = 12,direction = 'top',style=list("color"=if_else(isolate(basemap())=="Esri.WorldImagery","white","black"))),
                 group="Rivers",
                 highlightOptions = highlightOptions(bringToFront = TRUE, weight = 5,color ='#1c8eff', sendToBack = FALSE)) %>%
    #groupOptions("Rivers",zoomLevels = 7:10) %>% 
    addCircleMarkers(data=location_pts,
                     lng=~long,
                     lat=~lat,
                     group = "ref",
                     stroke=FALSE,
                     fillOpacity = 1,
                     fillColor = 'black',
                     label = ~StreamName,
                     labelOptions = labelOptions(direction = 'top'),
                     radius=5) %>%
    addScaleBar(position=c("bottomright"), options=scaleBarOptions(imperial = FALSE))
}

center_map <- function(map_id){
  leafletProxy(map_id) %>% setView(lng=-120.9141, lat=  56.19851, zoom=8.4)
}

# addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.75){
#   colorAdditions <- paste0(colors, ";border-radius:50%; width:", sizes, "px; height:", sizes, "px")
#   labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
#   
#   return(addLegend(map, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
# }

data_description <- function(){
HTML('
  <p>
  <h2>Background</h2>
  Since 2019, BC Hydro has tagged six fish species (Bull Trout, Arctic Grayling, Rainbow Trout, Walleye, Mountain Whitefish and Burbot) with radio transmitters to assess movement patterns in the Peace River and its tributaries. 
  Tagged fish are detected at an array of fixed receivers located throughout the Peace River and its tributaries (see Map tab) as well as during mobile surveys by fixed-wing aircraft or helicopter.
  Fixed receivers are operated from April through October and data are downloaded monthly. 
  Mobile surveys target Arctic Grayling in the Moberly River from May to June and Bull Trout in the Halfway River from August to September to identify likely spawning locations and movement timing. 
  Mobile surveys are also conducted throughout the watershed several times during winter months to supplement detection data while the majority of the fixed-receiver array is offline. 
  Additional information on the Site C Fish Movement Assessment can be found in the
  <a href="https://sitecproject.com/sites/default/files/fish-tracking-brochure.pdf" target="_blank"> Site C Fish Tracking Brochure</a>
  and
  <a href="https://www.sitecproject.com/sites/default/files/fish-tracking-notification-2022.pdf" target="_blank"> 2022 Fish Tracking Notification</a>.
  </p> 
  <p>
  <h2> About this Shiny App </h2>
  Two visualizations of the detection data are provided on the Plots tab. The <b>Individual Movements</b> tab plots detection locations for select individual fish through time. The <b>Seasonal Distribution</b> tab
  shows counts of unique individuals at detection locations during either weekly or monthly time intervals with circles sized proportionally to the number of individuals detected. 
  Detection locations are colored to distinguish release locations,  detections at fixed stations, mobile survey detections, and PIT tag detections.
  </p>
  <p>
  <h2> Acknowledgement </h2>
  We acknowledge this research was conducted on the traditional territory of Treaty 8 First Nations of Dunne Zaa, Cree, and Tse’khene cultural descent. 
  </p>
  <p>
  <h2> Annual Reports </h2>
  <a href="https://sitecproject.com/sites/default/files/Mon-1b-Task-2d-Site-C-Fish-Movement-Assessment-2019-Annual-Report.pdf" target="_blank">2019</a>
  <br>
  <a href="https://sitecproject.com/sites/default/files/Mon-1b-Task-2d-Site-C-Fish-Movement-Assessment-2020-Annual-Report.pdf" target="_blank">2020</a>
  <br>
  <a href="https://sitecproject.com/sites/default/files/Mon-1b-Task-2d-Site-C-Fish-Movement-Assessment-2021-Annual-Report.pdf" target="_blank">2021</a>
  <br>
  <a href="https://sitecproject.com/sites/default/files/Mon-1b-Task-2d-Site-C-Fish-Movement-Assessment-2022-Annual-Report.pdf" target="_blank">2022</a>
  <br>
  <a href="https://www.sitecproject.com/sites/default/files/Mon-1b-Task-2d-SiteC-Fish-Movement-Assessment-2023-Annual-Report.pdf" target="_blank">2023</a>
  </p>
 ')
}


#stationMap_ui.R #######################################################################################################
########################################################################################################################
########################################################################################################################

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


#movementMap_ui.R #######################################################################################################
########################################################################################################################
########################################################################################################################

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
#ui.R ##############################################################################################################
########################################################################################################################
########################################################################################################################

ui <- shiny::htmlTemplate(
  "www/index.html",
  stationMap = stationMap_ui,
  movementMap = movementMap_ui)

# server.r ###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
server <- function(input, output, session) {
  
  output$data_desc <- renderText(paste0("Data current as of ",max_data_date,"."))
  
# Basemap parameters ########################################################################################################
  
  basemap <- reactive({if_else(input$basemap=="Satellite", "Esri.WorldImagery", "Esri.WorldStreetMap")})
  # selected tab
  map <- reactive({if_else(input$tabs=="Individual Movements","map1","map2")})
  
  fps <- reactive({case_when(input$anim_speed=="Slowest" ~ 2000,
                             input$anim_speed=="Slower"  ~ 1500,
                             input$anim_speed=="Faster"  ~ 1000, # 1 second per row of data
                             input$anim_speed=="Fastest" ~ 500)
  })
  
  observe(center_map(map()))
  
  observeEvent(input$center_map, {
    center_map(map())
  })

  observeEvent(fps(),{
    if(input$tabs=="Individual Movements"){
    updateSliderTextInput(session,inputId = "index",selected = input$index)}
    else{updateSliderTextInput(session,inputId = "month",selected = input$month)}
    })
  # 
# Individual Map Params ###############################################################################################################
  
  output$map1 <- renderLeaflet({make_map(basemap, peace_network, location_pts) %>% 
      addLegendAwesomeIcon(iconSet, orientation = 'horizontal', position="topright") 
    })

  # Basemap observer ===============================================================================================================  
  observe({ 
    leafletProxy(map())  %>% 
      addProviderTiles(basemap(),layerId = "base") %>% 
      clearGroup("Rivers") %>% 
      addPolylines(data = peace_network,
                   opacity=case_when(basemap()=="Esri.WorldImagery"~.6, TRUE ~ 0.3),
                   label = ~StreamName,
                   weight = ~if_else(lwd=="Peace",8,2),
                   labelOptions = labelOptions(textOnly = TRUE, noHide = FALSE,textsize = 12,direction = 'top',style=list("color"=if_else(basemap()=="Esri.WorldImagery","white","black"))),
                   group="Rivers",
                   highlightOptions = highlightOptions(bringToFront = TRUE, weight = 8,color ='#1c8eff', sendToBack = TRUE)) %>%
      addCircleMarkers(data=location_pts,
                       lng=~long,
                       lat=~lat,
                       group = "ref",
                       #stroke=FALSE,
                       fillOpacity = 1,
                       color='black',
                       weight=2,
                       fillColor = 'white',
                       label = ~StreamName,
                       labelOptions = labelOptions(direction = 'top'),
                       radius=5)
      #groupOptions("Rivers",zoomLevels = 7:10)
      
  })
  
  # Receiver observer  ===============================================================================================================
  observe({
    color <- if_else(input$basemap=="Terrain","#545353","white")
    
    if(input$receivers){
      leafletProxy(map()) %>% 
      addCircleMarkers(data=receivers,group="receivers",color = color, lng = ~lon,lat = ~lat,label=~dp_StatnNm,radius = 1,opacity=1) %>% 
        addLegend(colors = paste0(color,"; border-radius: 50%; width:",8,"px; height:",8,"px;"),opacity = 1,
                  labels="Fixed-station receivers",
                  layerId="receiver_legend",
                  group = "receivers",
                  position='topright')} 
    else{
      leafletProxy(map()) %>% clearGroup("receivers") %>% removeControl("receiver_legend")}
  })
  
  filterFish <- reactive({
    req(input$fish)
    ind_d %>% dplyr::filter(fish_name==input$fish)
    
  })

  filterDate <- reactive({
    req(input$index)
    filterFish() %>% dplyr::filter(Datetime==as.POSIXct(input$index, tz="UTC"))
  })
  
  date_title <- reactive({
      sp <- unique(filterFish()$Species)
       tag <- unique(filterFish()$tagcode)
       ls <- unique(filterFish()$Life_Stage)
    tags$div(map_title_tag,HTML(sp,"<br> Tag #",tag,"<br>",ls,"<br>",format(as.POSIXct(input$index,tz="UTC"),"%b %d, %Y")))
    })
  
  iconSet <- awesomeIconList(
    `Hauled`=makeAwesomeIcon(icon="fish", library = "fa", text=fontawesome::fa('fish'), markerColor = "red", iconColor = "black"),
    `Movement`=makeAwesomeIcon(icon="fish", library="fa",text=fontawesome::fa('fish'),markerColor = "lightgray", iconColor = "black")
  )

  output$plot_desc <- renderText({
    req(input$tabs)
    if(input$tabs=="Individual Movements"){
      text <- "This plot animates detections of tagged individuals through time. 
      The fish marker appears as gray for volitional movements, and red for movements after the individual has been trap and hauled at the temporary upstream fish passage facility (2020 to 2024) or the permanent upstream fish passage facility (2025 onwards)."
    }else{
      text <- "This plot animates counts of tagged individuals at each detection location through time at the selected time step. 
      The size of the circle is proportional to the number of unique individuals detected at a location. If <b>'Show detections by type'</b> is selected, circles are coloured to distinguish between release locations, detections at fixed receivers, mobile detections, and recapture PIT tag detections."
    }
      text
  
  })

  observeEvent({input$index
    input$basemap
    input$tabs}, {
      leafletProxy("map1", data = filterDate()) %>%
        addAwesomeMarkers(layerId = "fish",
                          lng = ~ Longitude,  
                          lat = ~ Latitude,
                          icon= ~ iconSet[haul],
                          label= ~HTML(Species,"<br>", tagcode,"<br>",Life_Stage),
                          group="fish") %>% 
        addCircleMarkers(layerId="points",
                         data=location_pts,
                         lng=~long,
                         lat=~lat,
                         color = 'black',
                         weight = 2,
                         opacity = 1,
                         #stroke='black',
                         fillOpacity = 1,
                         fillColor = ifelse(filterDate()$Datetime>=as.POSIXct("2020-10-03",tz="UTC"),'black','white'),
                         label = ~StreamName,
                         labelOptions = labelOptions(direction = 'top'),
                         radius=5) %>% 
        addControl(layerId="date_title",
                   html = date_title(),
                   className = 'map-title')
       
    })
  
# Seasonal Map Params #######################################################################################################################################
  
  output$map2 <- renderLeaflet({
   make_map(basemap, peace_network,  location_pts) 
})
  
  # Det_type observer  ================================================================================================================================
  observe({
    req(!is.null(input$det_type))
   if(input$det_type & map()=="map2"){
     leafletProxy(map()) %>%
      addLegend(title = "Detection Type",layerId = "det_type",
                labels=factor(c("Release", "Station", "Mobile", "PIT"), levels=c("Release", "Station", "Mobile", "PIT")),
                colors=paste0(colors(),"; border-radius: 50%; width:",15,"px; height:",15,"px;"),opacity = 1)}else{
                  leafletProxy(map()) %>% removeControl("det_type")}
    
})
  
    dataset <- reactive({
      req(input$interval)
      
      d_seas[[input$interval]]
      
      })
 
dat1 <-  reactive({
  req(input$species)
  dataset() %>% filter(Species==input$species)})

choices <- reactiveValues()

observeEvent(input$species, {
  choices$lifestage <- sort(unique(dat1()$Life_Stage))
  })

dat <- reactive({
  req(input$lifestage)
  dat1() %>% filter(Life_Stage==input$lifestage) %>% 
    # Point radius on map- scale n from 5px to 30 px max
    mutate(radius=((n-min(n)) / (max(n)-min(n))) *(40-8) + 8)
  })

filterTime <- reactive({
     req(input$month)
    dat() %>% dplyr::filter(Time==input$month)
  })
  
n <- reactive({
  req(input$species,input$month,input$interval)
  n_seas[[input$interval]] %>% 
    dplyr::filter(Species==input$species, Time==input$month, Life_Stage==input$lifestage) %>% 
    pull(n)
  })

seas_title <- reactive({
    tags$div(map_title_tag,HTML(input$lifestage, input$species,"<br>",input$month,"<br>", paste0("Number of tagged fish = ",n())))})
  
colors <- reactive({
  req(input$basemap,!is.null(input$det_type))
  if(input$det_type){
    # if(input$basemap=="Terrain"){
    #   c("#1aeb02","#ff9305","#ff66fc")#"#ff9305")
    # }else{
      c("#1aeb02","#ff9305","#ff66fc", "#e70505")
      # c("#1aeb02","#ff66fc","#f0ea4d")
  } else if (input$basemap=="Terrain"){c("blue","blue","blue", "blue")}else{c("yellow","yellow","yellow" ,"yellow")}
})

  observeEvent({filterTime()
                colors()
                input$basemap
                input$tabs
                }, {
                  
pal <- colorFactor(colors(),
                   levels=factor(c("Release","Station","Mobile", "PIT"),levels = c("Release","Station","Mobile", "PIT")),
                   ordered = TRUE)
                         
      leafletProxy("map2") %>%
        clearGroup("fish") %>%
        addCircleMarkers(group="fish",
                         data = filterTime(),
                         lng = ~Longitude,  
                         lat = ~Latitude,
                         fillOpacity = .7,
                         stroke=FALSE,
                         label = paste0("",filterTime()$n), # causes crash when changing species found the paste0("") solution on SO:https://gis.stackexchange.com/questions/333997/error-while-rendering-leaflet-map-on-shiny
                         fillColor = ~pal(Type),#if_else(input$basemap=="Terrain",'blue',"#f0ea4d"),
                         radius = ~radius) %>% #~suppressWarnings(((n/max(n))*20)+5)) %>% # would throw warnings when switching certain species
        addCircleMarkers(layerId="points",
                         data=location_pts,
                         lng=~long,
                         lat=~lat,
                         group = "ref",
                         color='black',
                         opacity=1,
                         fillOpacity = 1,
                         weight=2,
                         fillColor = case_when(nrow(filterTime())==0~"white",
                           input$interval=="Weekly" & all(filterTime()$Timestart >= as.POSIXct("2020-10-03",tz="UTC")) | 
                                               input$interval=="Monthly" & all(filterTime()$Timestart >= as.POSIXct("2020-10-01 00:00:00",tz="UTC")) ~  'black',
                                               TRUE ~ 'white'),
                         label = ~StreamName,
                         labelOptions = labelOptions(direction = 'top'),
                         radius=5) %>%
              addControl(layerId="seas_title",html = seas_title(), className = 'map-title')
    })
  
# Map title HTML style #########################################################################################################################################
map_title_tag <-  tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: relative !important;
    left: 50%;
    width: 275px;
    text-align: center;
    padding-left: 5px;
    padding-right: 5px;
    background: rgba(255,255,255,1);
    font-weight: bold;
    font-size: 18px;
  }
"))
  
# UI outputs ######################################################################################################################################################
  
  output$choiceUI <- renderUI({
    if(input$tabs=="Individual Movements"){
      selectInput("fish","Select individual.", choices=sort(unique(ind_d$fish_name)), selected = "Bull Trout 149.360 496", size=1, selectize=FALSE)
    }else{
      selectInput("species","Select species.",choices=unique(dataset()$Species), size=1, selectize=FALSE)#, selected="Bull Trout")
    }
  })
  
  output$lifestageUI <- renderUI({
    if(input$tabs=="Seasonal Distribution"){
      tagList(
        selectInput("lifestage","Select life stage.", choices=choices$lifestage, size=1, selectize=FALSE),
        selectInput("interval","Select time step.",choices=c("Monthly", "Weekly"), selected="Monthly", size=1, selectize=FALSE),
        checkboxInput("det_type",HTML("<b>Show detections by type.</b>"),value = FALSE)
      )
    } 
    else {NULL}
    
  })

  output$dateUI <- renderUI({
    if(input$tabs=="Individual Movements"){
      sliderTextInput(inputId = "index", "Play animation.",
                      choices=unique(filterFish()$Datetime),
                      animate=animationOptions(interval=fps(),loop=FALSE),
                      dragRange=FALSE,
                      hide_min_max = TRUE,
                      force_edges = TRUE)
    }else{
      sliderTextInput("month",label="Play animation.",
                      choices=unique(dat()$Time),
                      force_edges = TRUE,
                      animate=animationOptions(interval=fps(),loop=FALSE))
    }
    
  })
  
}


shinyApp(ui = ui, server = server)