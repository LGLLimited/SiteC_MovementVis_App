make_map <- function(basemap, peace_network, location_pts){
  leaflet(options=leafletOptions(minZoom=7, maxZoom=13, zoomControl=FALSE)) %>%
    onRender("function(el,x) {
             L.control.zoom({ position:'bottomright' }).addTo(this)}") %>% 
    setView(lng=-120.9141, lat=  56.19851, zoom=7.45) %>%
    setMaxBounds(lng1=-124.12656439,
              lat1=57.64047692,
              lng2=-117.88295361,
              lat2=54.27808912) %>%
    addPolylines(data = peace_network,
                 opacity=case_when(basemap()=="Esri.WorldImagery"~0.4,
                                   TRUE ~ 0.2)) %>%
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
    addScaleBar(position=c("bottomright"), options=scaleBarOptions(imperial = FALSE))
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
HTML("Since 2019 six species have been tagged with radio transmitters to assess movement patterns in the Peace River basin. 
  Tagged species include Bull Trout, Arctic Grayling, Rainbow Trout, Walleye, Mountain Whitefish and Burbot. 
  Fixed receivers are operated from April through October each year and data downloaded monthly. Mobile tracking occurs periodically throughout the year.
  Two visualizations of the detection data are provided in the plots tab.
  <br>
  <h2>Individual Movements:</h2>
  <br>
  The individual movement tab animates detections of tagged individuals through time. The fish marker appears as gray for volitional movements, and red for hauled movements.
  <h2>Seasonal Distribution:</h2>
  <br>
  The seasonal distribution tab animates the counts of unique tagged individuals at each detection location on either weekly or monthly time steps. 
  The size of the circle markers is proportional to the number of unique individuals detected at a location during the displayed time period.
  ")
}