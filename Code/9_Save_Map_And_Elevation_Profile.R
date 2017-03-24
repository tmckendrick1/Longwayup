library(rmarkdown)
library(shiny)
library(leaflet)
library(plotly)
library(plyr)
library(dplyr)
library(gridExtra)
library(scales)
library(htmltools)
library(htmlwidgets)

  # Choose a country and blg to plot map for


  # S1. Load data and functions
    load("C:\\Users\\pss3c\\Desktop\\Longwayup\\R Files\\Longwayup\\Map App Dataset Latest.RData")
    
    # Elevation Profile
    
    blog<-"Overall" ### Would need to change if making separate map for each blog
    
    for (country in c("Overall",countrylevels)) {
      # for blog in XXXXX {
      
        
      
      # Bespoke part to condition
      eleprofile_condition<-TRUE
      
      # Selected country and blog part to condition
      if (country!="Overall") {
        eleprofile_condition<-geodfcomprwg$country==country
        
        if (blog!="Overall") {
          eleprofile_condition<-(geodfcomprwg$country==country &
                                   geodfcomprwg$blog==blog)
        }
      }
      
      distance_vector<-cumsum(geodfcomprwg$distdiff[eleprofile_condition])
      
      eleProfile <- geodfcomprwg[eleprofile_condition,] %>%
        group_by(activity_country.cat) %>%
        plot_ly(x=distance_vector,y=~ele,color=~activity,type = 'scatter', mode = 'lines') %>%
        layout(xaxis = list(title = "Cumulative Distance (km)"), yaxis = list(title = "Elevation (m)"))

    
    # Map
      
      m <- leaflet()
      
      # S6.a. Tiles
      m <- m %>%
        addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
        addProviderTiles("Esri.NatGeoWorldMap", group = "National Geographic") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite")
      
      # S6.b. Legend
      m <- m %>%
        addLegend(position = 'bottomright',opacity = 0.7, 
                  colors = c("Green","Red","Blue"), 
                  labels = c("Biking", "Hiking", "Public Transport"),
                  title = "Legend")
      
      # S6.c. Layers
      m <- m %>%
        addLayersControl(position = 'bottomright',
                         baseGroups = c("Street",
                                        "Satellite",
                                        "National Geographic"),
                         overlayGroups = c("Main Route",
                                           "Side Trips",
                                           "Accommodation",
                                           "Records",
                                           "Photos"),
                         options = layersControlOptions(collapsed = FALSE))
      
      m <- m %>% 
        hideGroup("Accommodation") %>%
        hideGroup("Records") %>%
        hideGroup("Photos")
      
      
      
      # S6.d. Markers/Popups
      
      # Records
      records_condition<-(lookup_table_stats_and_records$country==country
                          & lookup_table_stats_and_records$blog==blog)
      
      for (i in 1:length(unique(lookup_table_stats_and_records$record_id[records_condition]))) {
        
        records_markers_lookup <- 
          lookup_table_stats_and_records$record_id[records_condition]==
          unique(lookup_table_stats_and_records$record_id[records_condition])[i]
        
        m <- addMarkers(m,
                        lng=last(
                          lookup_table_stats_and_records$record_lon[records_condition][records_markers_lookup]
                        ),
                        lat=last(
                          lookup_table_stats_and_records$record_lat[records_condition][records_markers_lookup]
                        ),                         
                        popup=paste("<b>",
                                    last(
                                      lookup_table_stats_and_records$record_name[records_condition][records_markers_lookup]
                                    ),
                                    "</b>",
                                    "<br/>",
                                    last(
                                      lookup_table_stats_and_records$record_desc[records_condition][records_markers_lookup]
                                    )
                        ),
                        icon = makeIcon(iconWidth = 30,
                                        iconHeight = 30,
                                        iconUrl = last(
                                          lookup_table_stats_and_records$record_icon[records_condition][records_markers_lookup]
                                        )
                        ),
                        group="Records"
        )
      }
      
      # Accommodation
      
      # Bespoke part to condition
      accommodation_condition<-lookup_table_stats_and_records$date!="Overall"
      
      # Selected country and blog part to contdition
      if (country!="Overall") {
        accommodation_condition<-(accommodation_condition &
                                    lookup_table_stats_and_records$country==country)
        if (blog!="Overall") {
          accommodation_condition<-(accommodation_condition &
                                      lookup_table_stats_and_records$blog==blog)
        }
      }
      
      
      for (i in 1:length(unique(lookup_table_stats_and_records[accommodation_condition,"date"]))) {
        
        accom_markers_lookup <- 
          lookup_table_stats_and_records[accommodation_condition,"date"]==
          unique(lookup_table_stats_and_records[accommodation_condition,"date"])[i]
        
        m <- addMarkers(m,
                        lng=last(
                          lookup_table_stats_and_records[accommodation_condition,"lon_end"][accom_markers_lookup]
                        ),
                        lat=last(
                          lookup_table_stats_and_records[accommodation_condition,"lat_end"][accom_markers_lookup]
                        ),
                        popup=paste("<b>",
                                    last(
                                      lookup_table_stats_and_records[accommodation_condition,"destination"][accom_markers_lookup]
                                    ),
                                    "</b>",
                                    "<br/>",
                                    "<i>",
                                    last(
                                      lookup_table_stats_and_records[accommodation_condition,"accommodation_type"][accom_markers_lookup]
                                    ),
                                    "</i>",
                                    "<br/>",
                                    last(
                                      lookup_table_stats_and_records[accommodation_condition,"accommodation_name"][accom_markers_lookup]
                                    )
                        ),
                        icon = makeIcon(iconWidth = 30,
                                        iconHeight = 30,
                                        iconUrl = last(
                                          lookup_table_stats_and_records[accommodation_condition,"accommodation_icon"][accom_markers_lookup]
                                        )
                        ),
                        group="Accommodation"
        )
      }
      
      # Photos

      # Bespoke part to condition
      photos_condition <-
        !is.na(photo_data$info_lookup)
      
      
      # Selected country and blog part to contdition
      if (country!="Overall") {
        photos_condition<-photos_condition & photo_data$country==country
        if (blog!="Overall") {
          photos_condition<-photos_condition & photo_data$blog==blog
        }
      }
      
      for (i in 1:nrow(photo_data[photos_condition,])) {
        m <- addMarkers(m, 
                        lng=photo_data$lon[photos_condition][i],
                        lat=photo_data$lat[photos_condition][i],  
                        ### CHECK FIRST LINE OF THIS - SHOULD ADJUST BOX SIZE AND MAKE SEMITRANSPARENT ###
                        popup=paste("<style> div.leaflet-popup-content {width:auto !important;}</style>",
                                    "<img src = \"",
                                    "https://longwayupsite.files.wordpress.com/2017/03/",
                                    tolower(gsub(" ","-",photo_data$filenames_short[photos_condition][i])),
                                    "?w=200",
                                    "\">",
                                    sep=""
                        ),  
                        icon = makeIcon(
                          iconAnchorX = 12,
                          iconAnchorY = 12,
                          iconUrl = "https://www.mapbox.com/maki/renders/camera-12@2x.png"
                        ),
                        group="Photos")
      }
      
      
      
      # S6.e. Polylines
      
      # Bespoke part to condition
      polylines_condition<-TRUE
      
      
      # Selected country and blog part to contdition
      if (country!="Overall") {
        polylines_condition<-(polylines_condition &
                                geodfcomprwg$country==country)
        if (blog!="Overall") {
          polylines_condition<-(polylines_condition &
                                  geodfcomprwg$blog==blog)
        }
      }
      
      
      
      for (j in min(geodfcomprwg$activity_mainroute_country.cat
                    [polylines_condition]):
           max(geodfcomprwg$activity_mainroute_country.cat
               [polylines_condition]))
        
      { 
        m <- m %>% 
          addPolylines(
            data= geodfcomprwg[geodfcomprwg$activity_mainroute_country.cat==j,],
            lat = ~ lat, 
            lng = ~ lon, 
            color=paste(colours.activity_mainroute_country[j]),
            group=paste(group.activity_mainroute_country[j]),
            opacity=0.7
          )
      }

 
      # Save data
      
      wd_local<-"C:\\Users\\pss3c\\Desktop\\Longwayup\\R Files\\HTML Outputs\\"
      wd_github<-"C:\\Users\\pss3c\\Desktop\\Highway Coding\\GitHub\\tmckendrick1.github.io\\"
      
      # Save map - to two directories
      saveWidget(widget=m,
                 file = paste(wd_local,"routemap_",country,"_",blog,".html",sep=""),
                 selfcontained = TRUE)
      
      if (country!="Argentina and Chile") {
        saveWidget(widget=m,
                   file = paste(wd_github,tolower(country),"\\index.html",sep=""),
                   selfcontained = TRUE)
      } else {
          saveWidget(widget=m,
                     file = paste(wd_github,"argentina","\\index.html",sep=""),
                     selfcontained = TRUE)
      }
      
      
      # Save ele profile
      saveWidget(widget=eleProfile,
                 file = paste(wd_local,"eleprofile_",country,"_",blog,".html",sep=""),
                 selfcontained = TRUE)
      # As yet not sure if we need this on Github
      
      # Save both to markdown file
      render("C:\\Users\\pss3c\\Desktop\\Highway Coding\\App\\map_and_eleprofile.Rmd",
             output_file = paste(wd_local,"map_and_eleprofile_",country,"_",blog,".html",sep=""),
             params = list(map = m,
                           eleProfile = eleProfile),
             envir = new.env(parent = globalenv())
      )
    }