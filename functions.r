make_map <- function(basemap,plot_desc, peace_network, location_pts){
  leaflet(options=leafletOptions(minZoom=7, maxZoom=13, zoomControl=FALSE)) %>%
    onRender("function(el,x) {
             L.control.zoom({ position:'bottomright' }).addTo(this)}") %>% 
    setView(lng=-120.9141, lat=  56.19851, zoom=7.4) %>%
    setMaxBounds(lng1=-124.12656439,
              lat1=57.64047692,
              lng2=-117.88295361,
              lat2=54.27808912) %>%
    addPolylines(data = peace_network,
                 opacity=case_when(basemap()=="Esri.WorldImagery"~0.4,
                                   TRUE ~ 0.2),
                 label = ~StreamName,
                 labelOptions = labelOptions(textOnly = TRUE, noHide = FALSE,textsize = 12,direction = 'top',style=list("color"=if_else(basemap()=="Esri.WorldImagery","white","black"))),
                 group="Rivers",
                 highlightOptions = highlightOptions(bringToFront = TRUE, weight = 5,color ='#1c8eff', sendToBack = FALSE)) %>%
    groupOptions("Rivers",zoomLevels = 7:10) %>% 
    addProviderTiles(basemap()) %>%
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
    addScaleBar(position=c("bottomright"), options=scaleBarOptions(imperial = FALSE)) %>% 
    addControl(layerId="plot_desc",
               html=plot_desc(),
               className='map-desc',
               position='topleft')
}

center_map <- function(map_id){
  leafletProxy(map_id) %>% setView(lng=-120.9141, lat=  56.19851, zoom=7.4)
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
  Since 2019, BC Hydro has tagged six species with radio transmitters to assess movement patterns in the Peace River and its tributaries. 
  Study species include Bull Trout, Arctic Grayling, Rainbow Trout, Walleye, Mountain Whitefish and Burbot. 
  Tagged fish are detected at an array of fixed receivers located throughout the Peace River and its tributaries (see Map tab) as well as during mobile surveys by fixed-wing aircraft or helicopter.
  Fixed receivers are operated from April through October and data are downloaded monthly. 
  Mobile surveys target Arctic Grayling in the Moberly River from May to June and Bull Trout in the Halfway River from August to September to identify likely spawning locations and movement timing. 
  Mobile surveys are also conducted throughout the watershed several times during winter months to supplement detection data while the majority of the fixed-receiver array is offline. 
  Additional information on the BC Hydro Site C Fish Tracking Program can be found in the
  <a href="https://sitecproject.com/sites/default/files/fish-tracking-brochure.pdf" target="_blank"> Site C Fish Tracking Brochure</a>
  or
  <a href="https://www.sitecproject.com/sites/default/files/fish-tracking-notification-2022.pdf" target="_blank"> 2022 Fish Tracking Notification</a>.
  </p> 
  <p>
  <h2> About this Shiny App </h2>
  Two visualizations of the detection data are provided on the Plots tab. The <b>Individual Movements</b> tab plots detection locations for an individual fish through time. The <b>Seasonal Distribution tab</b> 
  shows counts of unique individuals at detection locations during either weekly or monthly time intervals with circles sized proportionally to the number of individuals detected. 
  Detection locations are colored to distinguish release locations, detections at fixed stations, and mobile survey detections.
  </p>
  <br>
 <h2> Annual Reports </h2>
  <a href="https://sitecproject.com/sites/default/files/Mon-1b-Task-2d-Site-C-Fish-Movement-Assessment-2019-Annual-Report.pdf" target="_blank">2019</a>
  <br>
  <a href="https://sitecproject.com/sites/default/files/Mon-1b-Task-2d-Site-C-Fish-Movement-Assessment-2020-Annual-Report.pdf" target="_blank">2020</a>
  ')
}

# "<h2>Individual Movements</h2>
#   <br>
#   The individual movement tab animates detections of tagged individuals through time. The fish marker appears as gray for volitional movements, and red for movements after the individual has been trap and hauled at the temporary upstream fish passage facility.
# <h2>Seasonal Distribution</h2>
#   <br>
#   The seasonal distribution tab animates counts of unique tagged individuals at each detection location during weekly or monthly time intervals. 
# The size of the circle is proportional to the number of unique individuals detected at a location during the displayed time period.
# Circles are coloured to distinguish between release locations, detections at fixed receivers, and mobile detections.
# <br>"