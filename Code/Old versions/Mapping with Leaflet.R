# Create Base Map
m <- leaflet()

# Tiles
m <- m %>%
  #addProviderTiles("Thunderforest.SpinalMap", group = "Inferno!") %>%
  addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
  #addProviderTiles("Stamen.Watercolor", group = "Watercolour") %>% 
  addProviderTiles("Esri.NatGeoWorldMap", group = "National Geographic") %>%
  #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012", group = "World at Night") %>%
  #addProviderTiles("Esri.WorldPhysical", group = "Physical") %>%
  #addProviderTiles("OpenTopoMap", group = "Topographical") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite")

# Legend
m <- m %>%
  addLegend(position = 'bottomright',opacity = 0.4, 
            colors = c("Red","Green","Blue","Purple"), 
            labels = c("Biking", "Hiking", "Public Transport - main route", "Public transport - other"),
            title = "Legend")

# Layers
m <- m %>%
  addLayersControl(position = 'bottomright',
                   #baseGroups = c("Street","Topographical","Physical","Satellite","National Geographic","World at Night","Inferno"),
                   baseGroups = c("Street","Satellite","National Geographic"),
                   #overlayGroups = c("Biking", "Hiking", "Public Transport - main route", "Public transport - other", "Photo markers"),
                   overlayGroups = c("Biking", "Hiking", "Public Transport - main route", "Public transport - other"),
                   options = layersControlOptions(collapsed = FALSE)) 

# Polylines
for (i in 1:datasets.number) {
  m <- m %>% addPolylines(data=get(paste0("geodfcomprwg",i,sep="")), lat = ~ lat, lng = ~ lon, color=paste(colours.vector[i]), group=paste(activity.vector[i]))
}

# Popups

# for (i in 1:nrow(popups.data)) {
#   
#   if (!is.na(popups.data$lon[i])) {
#     m <- addPopups(m, lng=popups.data$lon[i], lat=popups.data$lat[i],  
#                    paste("<b>",
#                          popups.data$name[i],
#                          "</b>",
#                          "<br/>",
#                          popups.data$desc[i]
#                    ),  
#                    options = popupOptions(closeButton = FALSE),
#                    group='Extreme Points')
#   }
# }
