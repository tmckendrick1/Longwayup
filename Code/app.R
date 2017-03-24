
library(shiny)
library(leaflet)
library(plotly)
library(plyr)
library(dplyr)
library(gridExtra)
library(scales)
library(htmltools)

shinyApp(
  ui=fluidPage(theme = "stylesheet.css",
    
    # U1. Title
      titlePanel("Tom and Charlie's 2016/17 Bike Tour!"),
      h3(tags$a(href = "http://www.longwayup.org", "Check out my Longwayup blog here", target="_blank"), align="center"),

    # U2. Select country, blog, elevation profile on/off
      fluidRow(
      
      # U2.a. Select country
        column(3, offset=2,
               h3("Select a country"),
               uiOutput("selectacountry")
               ),
      
      # U2.b. Select blog
        column(3,
               h3("Select a section"),
               uiOutput("selectablog")
               ),
      
      # U2.c. Select elevation profile on/off
        column(3,
               h3("Load elevation profile?"),
               uiOutput("selecteleprofile"),
               textOutput("selecteleprofilecaveat")
               )
      ),
      

    # U3. Tabset panels
      tabsetPanel(
        
        # U3.a. Route and elevation profile
          tabPanel(
            h3("Map and Elevation Profile"),
            
            # Map
              fluidRow(
                column(8, align="center", offset=2,
                       h4("Select a date range"),
                       uiOutput("slider")
                )
              ),
              h3("Our Route"),
              leafletOutput("map"),
              h3(" "),
              h3("Download GPS File"),
              downloadButton("downloadData", "Click me!"),
              h3(" "),
            # Elevation Profile
              conditionalPanel(
                condition = "input.showeleprofile == 'Yes'",
                h3("Our Elevation Profile"),
                checkboxGroupInput("mainrouteorsidetrips",
                                   NULL,
                                   c("Main Route","Side Trips"),
                                   selected=c("Main Route","Side Trips")),
                plotlyOutput("eleProfile")
              )
            ),

        # U3.b. Overview and Route Notes
          tabPanel(
            h3("Overview and Route Notes"),
            h5("Coming soon!") ### REMOVE WHEN WE HAVE THEM
            #h3(textOutput("NAME OF BLOG/COUNTRY HERE")),
            #htmlOutput("ROUTE NOTES HERE")
          ),
        
        # U3.c. Charts
        
        ### EVENTUALLY HAVE ORDER: (1) TOTAL DISTANCE, (2) AVERAGE DISTANCE, (3) MAX DAILY DISTANCE,
        ### (4) TOTAL ELEVATION GAIN, (5) AVERAGE ELEVATION GAIN, (6) MAX DAILY ELEVATION GAIN,
        ### (7) ELEMIN, (8) ELEMAX, (9) AVERAGE ELEVATION,
        ### (10) AVERAGE GRADIENT, (11) AVERAGE ASCENT GRADIENT, (12) AVERAGE DESCENT GRADIENT,
        ### (13) MAX TEMPERATURE, (14) MIN TEMPERATURE, (15) MAX SPEED OR AVERAGE TEMPERATURE
        
        ### FOR NOW DO IT QUITE DIFFERENTLY
        
          tabPanel(
            h3("Charts"),
            h5("(Takes a moment to load)"),
            tabsetPanel(
              tabPanel(
                h3("Countries"),
                tabsetPanel(
                  tabPanel(
                    h5("Biking"),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Biking_distance"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_avg_distance"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_daydistmax"),
                                         h5(""))
                             ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Biking_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_avg_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_dayelegainmax"),
                                         h5("")) 
                             ),              
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Biking_elemax"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_elemin"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_wa_ele"),
                                         h5(""))
                            ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Biking_avg_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_avg_ascent_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_avg_descent_gradient"),
                                         h5(""))
                            ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Biking_tempmax"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_tempmin"),
                                         h5(""),
                                         plotOutput("chart_tab1_Biking_wa_temp"),
                                         h5(""))
                    )
                  ),
                  tabPanel(
                    h5("Hiking"),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Hiking_distance"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_avg_distance"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_daydistmax"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Hiking_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_avg_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_dayelegainmax"),
                                         h5("")) 
                    ),              
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Hiking_elemax"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_elemin"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_wa_ele"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Hiking_avg_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_avg_ascent_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_avg_descent_gradient"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab1_Hiking_tempmax"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_tempmin"),
                                         h5(""),
                                         plotOutput("chart_tab1_Hiking_wa_temp"),
                                         h5(""))
                    )
                  )
                )
              ),
              tabPanel(
                h3("Sections (between countries)"),
                tabsetPanel(
                  tabPanel(
                    h5("Biking"),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Biking_distance"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_avg_distance"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_daydistmax"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Biking_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_avg_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_dayelegainmax"),
                                         h5("")) 
                    ),              
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Biking_elemax"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_elemin"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_wa_ele"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Biking_avg_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_avg_ascent_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_avg_descent_gradient"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Biking_tempmax"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_tempmin"),
                                         h5(""),
                                         plotOutput("chart_tab2_Biking_wa_temp"),
                                         h5(""))
                    )
                  ),
                  tabPanel(
                    h5("Hiking"),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Hiking_distance"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_avg_distance"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_daydistmax"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Hiking_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_avg_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_dayelegainmax"),
                                         h5("")) 
                    ),              
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Hiking_elemax"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_elemin"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_wa_ele"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Hiking_avg_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_avg_ascent_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_avg_descent_gradient"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab2_Hiking_tempmax"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_tempmin"),
                                         h5(""),
                                         plotOutput("chart_tab2_Hiking_wa_temp"),
                                         h5(""))
                    )
                  )
                )
              ),
              tabPanel(
                h3("Sections (within country)"),
                tabsetPanel(
                  tabPanel(
                    h5("Biking"),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Biking_distance"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_avg_distance"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_daydistmax"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Biking_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_avg_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_dayelegainmax"),
                                         h5("")) 
                    ),              
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Biking_elemax"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_elemin"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_wa_ele"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Biking_avg_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_avg_ascent_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_avg_descent_gradient"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Biking_tempmax"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_tempmin"),
                                         h5(""),
                                         plotOutput("chart_tab3_Biking_wa_temp"),
                                         h5(""))
                    )
                  ),
                  tabPanel(
                    h5("Hiking"),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Hiking_distance"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_avg_distance"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_daydistmax"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Hiking_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_avg_elegain"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_dayelegainmax"),
                                         h5("")) 
                    ),              
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Hiking_elemax"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_elemin"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_wa_ele"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Hiking_avg_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_avg_ascent_gradient"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_avg_descent_gradient"),
                                         h5(""))
                    ),
                    fluidRow(splitLayout(cellWidths = c("30%","3%", "30%", "3%", "30%", "4%"),
                                         plotOutput("chart_tab3_Hiking_tempmax"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_tempmin"),
                                         h5(""),
                                         plotOutput("chart_tab3_Hiking_wa_temp"),
                                         h5(""))
                    )
                  )
                )
              )
            )
          ),
          
        # U3.c. Blog
          tabPanel(
            h3("Blog"),
            h3(textOutput("blogheader")),
            htmlOutput("Blog")
          )
      )
  ),
    
  server = function(input,output) {
    
    # S1. Load data and functions
      load("Map App Dataset Latest.RData")

    # S2. Create values dataframe
      values <- reactiveValues(country="Overall",
                               blog="Overall",
                               conditiont1=NA,
                               conditiont2=NA,
                               conditiont2=NA,
                               conditiont3=NA)

    # S3. Assign to values dataframe selected country and blog
    
      # S3.a. Country
        observe({
          if (is.null(input$selectedcountry) || input$selectedcountry == 0)
            return()
          values$country <- input$selectedcountry
        })

        # Function to return radio buttons for country selection
        output$selectacountry <- renderUI({
          radioButtons("selectedcountry",
                       NULL,
                       c("Overall",
                         countrylevels)
          )
        })
        
        output$selecteleprofile <- renderUI({
        radioButtons("showeleprofile",
                     NULL,
                     c("No",
                       "Yes")
        )
        })
        
        output$selecteleprofilecaveat <- renderText({
          "(Takes a moment to load)"
        })
        
    
      # S3.b. Blog
    
        # Function to return radio buttons for blog selection once country is selected
          output$selectablog <- renderUI({
              radioButtons("selectedblog",
                           NULL,
                           c("Overall",
                             unique(geodfcomprwg$blog[geodfcomprwg$country==values$country])
                           )
                           )
          })
    
        # Assign selected blog to values dataframe
          observe({
            if (is.null(input$selectedblog) || input$selectedblog== 0)
              return()
            values$blog <- input$selectedblog
          })

    # S4. Render blog
          
      # S4.a. Render blog header
        output$blogheader <- renderText({
          if (values$country=="Overall")
            return()
          "Blog"
        })
      
      # S4.b. Render blog contents
        output$Blog <- renderUI({ 
          if (values$country=="Overall") {
            "Select a country!"
          } else {
            HTML(blogs$blog[blogs$country==values$country])
          }
        })
  
    
    # S5. Render Elevation Profile

      # S5.b. Render Elevation Profile Plot
        output$eleProfile <- renderPlotly({
          if (input$showeleprofile == "No")
            return()
    
          # Bespoke part to condition
          eleprofile_condition<-TRUE
          
          # Slider part to condition
          eleprofile_condition <- eleprofile_condition &
            geodfcomprwg$date>=input$inSlider[1] &
            geodfcomprwg$date<=input$inSlider[2]
          
          # Selected country and blog part to condition
          if (values$country!="Overall") {
            eleprofile_condition<-geodfcomprwg$country==values$country
            
            if (values$blog!="Overall") {
              eleprofile_condition<-(geodfcomprwg$country==values$country &
                                         geodfcomprwg$blog==values$blog)
            }
          }
          
          eleprofile_condition <- 
            (eleprofile_condition & 
               (geodfcomprwg$main_route %in% gsub("Side Trips",
                                                  0,
                                                  gsub("Main Route",
                                                       1,
                                                       input$mainrouteorsidetrips)
                                                  )
                )
             )
            

          distance_vector<-cumsum(geodfcomprwg$distdiff[eleprofile_condition])
          
          ele_profile <- geodfcomprwg[eleprofile_condition,] %>%
            group_by(activity_country.cat) %>%
            plot_ly(x=distance_vector,y=~ele,color=~activity,type = 'scatter', mode = 'lines') %>%
            layout(xaxis = list(title = "Cumulative Distance (km)"), yaxis = list(title = "Elevation (m)"))
          
          ele_profile
          
        })
    
    
    # S6. Render Map
        
      output$map = renderLeaflet({
        
        m <- leaflet()
        
        # S6.a. Tiles
          m <- m %>%
            addProviderTiles("OpenStreetMap.Mapnik", group = "Street") %>%
            addProviderTiles("Esri.NatGeoWorldMap", group = "National Geographic") %>%
            addProviderTiles("Esri.WorldImagery", group = "Satellite")
        
        # S6.b. Legend
          m <- m %>%
            addLegend(position = 'bottomright',opacity = 0.4, 
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
              records_condition<-(lookup_table_stats_and_records$country==values$country
                                  & lookup_table_stats_and_records$blog==values$blog)

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
            
            # Slider part to condition
            # Because this date is in character format, need to work with the subset of 'TRUE's 
            # straight away
            accommodation_condition[accommodation_condition] <-
              as.Date(lookup_table_stats_and_records$date[accommodation_condition])>=input$inSlider[1] &
              as.Date(lookup_table_stats_and_records$date[accommodation_condition])<=input$inSlider[2]
            
            # Selected country and blog part to contdition
            if (values$country!="Overall") {
              accommodation_condition<-(accommodation_condition &
                                  lookup_table_stats_and_records$country==values$country)
              if (values$blog!="Overall") {
                accommodation_condition<-(accommodation_condition &
                                    lookup_table_stats_and_records$blog==values$blog)
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
            
           # Insert Photo Markers/Popups
            
            #if (values$country=="Overall") {
            #  photos_condition<-!is.na(photo_data$info_lookup)
            #} else if (values$blog=="Overall") {
            #  photos_condition<-(!is.na(photo_data$info_lookup) & photo_data$country==values$country)
            #} else {
            #  photos_condition<-
            #    (!is.na(photo_data$info_lookup) &
            #       photo_data$country==values$country &
            #       photo_data$blog==values$blog)
            #}
            
            # Bespoke part to condition
            photos_condition <-
              !is.na(photo_data$info_lookup)
            
            # Slider part to condition
            photos_condition <-
              photos_condition &
              photo_data$date>=input$inSlider[1] &
              photo_data$date<=input$inSlider[2]
            
            # Selected country and blog part to contdition
            if (values$country!="Overall") {
              photos_condition<-photos_condition & photo_data$country==values$country
              if (values$blog!="Overall") {
                photos_condition<-photos_condition & photo_data$blog==values$blog
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
             
             # Slider part to condition
             polylines_condition<-
               polylines_condition &
               geodfcomprwg$date>=input$inSlider[1] &
               geodfcomprwg$date<=input$inSlider[2]
             
             # Selected country and blog part to contdition
             if (values$country!="Overall") {
               polylines_condition<-(polylines_condition &
                                           geodfcomprwg$country==values$country)
               if (values$blog!="Overall") {
                 polylines_condition<-(polylines_condition &
                                             geodfcomprwg$blog==values$blog)
               }
             }
             
             
             
             for (j in min(geodfcomprwg$activity_mainroute_country.cat
                           [polylines_condition]):
                       max(geodfcomprwg$activity_mainroute_country.cat
                          [polylines_condition]))
                  
             { 
               m <- m %>% 
                 addPolylines(
                   data= geodfcomprwg[geodfcomprwg$activity_mainroute_country.cat==j &
                                        geodfcomprwg$date>=input$inSlider[1] &
                                        geodfcomprwg$date<=input$inSlider[2],],
                   lat = ~ lat, 
                   lng = ~ lon, 
                   color=paste(colours.activity_mainroute_country[j]),
                   group=paste(group.activity_mainroute_country[j])
                 )
             }
            

          # S7 Display map
            m
      })
      
      
      # Map Slider
      output$slider <- renderUI({
        condition_slider<-TRUE
        if (values$country!="Overall") {
          condition_slider<-geodfcomprwg$country==values$country
          if (values$blog!="Overall") {
            condition_slider<-(geodfcomprwg$country==values$country &
                              geodfcomprwg$blog==values$blog)
          }
        }
        min_date_slider<-first(geodfcomprwg$date[condition_slider])
        max_date_slider<-last(geodfcomprwg$date[condition_slider])
        
        sliderInput("inSlider",
                    NULL,
                    min=min_date_slider,
                    max=max_date_slider,
                    value=c(min_date_slider,max_date_slider),
                    width = "2000px")
      })
      
    # GRAPHS
      
    for (variable in unique(plot_names$variable)) {
      for (activity in unique(plot_names$activity)) {
        
        local({
          variable<-variable
          activity<-activity
        
        # Tab 1
        output[[paste("chart_tab1","_",activity,"_",variable,sep="")]]<-renderPlot({
          
          conditiont1 <- which(plot_names$comparison=="country" &
                                      plot_names$across=="Overall" &
                                      plot_names$activity==activity &
                                      plot_names$variable==variable &
                                      plot_names$countryorblog==values$country)
          
          plots[[conditiont1]] +
            scale_fill_manual(values = as.character(brewer_pal(type = "qual")(max(
              plot_colours[[conditiont1]]
              ))[
                plot_colours[[conditiont1]]
                 ]))
          
        })
        
        # Tab 2
        output[[paste("chart_tab2","_",activity,"_",variable,sep="")]]<-renderPlot({
          
          if (values$country=="Overall" | values$blog!="Overall") {
            conditiont2 <- which(plot_names$comparison=="blog" &
                                          plot_names$across=="Overall" &
                                          plot_names$activity==activity &
                                          plot_names$variable==variable &
                                          plot_names$countryorblog==values$blog)
          } else {
            conditiont2 <- which(plot_names$comparison=="blog" &
                                   plot_names$across=="Overall" &
                                   plot_names$activity==activity &
                                   plot_names$variable==variable &
                                   plot_names$countryorblog==values$country)
          }
          
          plots[[conditiont2]] +
            scale_fill_manual(values = as.character(brewer_pal(type = "qual")(max(
              plot_colours[[conditiont2]]
            ))[
              plot_colours[[conditiont2]]
              ]))
        })
        
        # Tab 3
        output[[paste("chart_tab3","_",activity,"_",variable,sep="")]]<-renderPlot({
          if (values$country!="Overall") {
            
            conditiont3 <-which(plot_names$comparison=="blog" &
                                        plot_names$across==values$country &
                                        plot_names$activity==activity &
                                        plot_names$variable==variable &
                                        plot_names$countryorblog==values$blog)
            plots[[conditiont3]] +
              scale_fill_manual(values = as.character(brewer_pal(type = "qual")(max(
                plot_colours[[conditiont3]]
              ))[
                plot_colours[[conditiont3]]
                ]))
          }
          })
        })
        }
      }
    
    # Download data
      
      gpx_file<-reactive({
        
        if (values$country=="Overall") {
          condition_download<-TRUE
        } else if (values$blog=="Overall") {
          condition_download<-geodfcomprwg$country==values$country
        } else {
          condition_download<-geodfcomprwg$country==values$country &
                         geodfcomprwg$blog==values$blog
        }
        condition_download<-
          condition_download &
          geodfcomprwg$date>=input$inSlider[1] &
          geodfcomprwg$date<=input$inSlider[2]
        
        paste(
          paste(
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
            "<gpx xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" ",
            "xmlns:gpxdata=\"http://www.cluetrust.com/XML/GPXDATA/1/0\" ",
            "xmlns=\"http://www.topografix.com/GPX/1/0\" ",
            "xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd http://www.cluetrust.com/XML/GPXDATA/1/0 http://www.cluetrust.com/Schemas/gpxdata10.xsd\" ",
            "version=\"1.0\" ",
            "creator=\"http://ridewithgps.com/\">",
            sep=""),
          paste(
            "<author>Tom Mckendrick</author>",
            "<url>www.longwayup.org</url>",
            sep=""
          ),
          paste(
            "<time>",sub(" ","T",as.character(Sys.time())),"Z","</time>",
            sep=""
          ),
          paste(
            "<trk><name>",as.character(first(geodfcomprwg$date[condition_download])),"_",as.character(last(geodfcomprwg$date[condition_download])),"</name>",
            sep=""
          ),
          paste(
            c("<trkseg>",
              geodfcomprwg$body_text[condition_download]),
            collapse=""
          ),
          paste(
            "</trkseg></trk></gpx>"
          ),
          collapse=""
        )
        
      })
      
      output$downloadData <- downloadHandler(
        filename = "download.gpx",
        content = function(file) {
          writeLines(gpx_file(), file)
        }
      )
      
      
  }
)

      ### TRY THIS - SHOULD GENERATE POPUP WITH LAT AND LNG
      # testfunction <- function(id, lati, lngi) {
      #   point_id <- min_distance_obs(geodfcomprwg$lat,
      #                    geodfcomprwg$lon,
      #                    lati,
      #                    lngi,
      #                    geodfcomprwg$id.total
      #                    )[1]
      #   
      #   content <- paste0(round(geodfcomprwg$lat[geodfcomprwg$id.total==point_id],1),
      #                     ", ",
      #                     round(geodfcomprwg$lon[geodfcomprwg$id.total==point_id],1),
      #                     sep="")
      #   leafletProxy("map") %>% addPopups(lng=lngi, lat=lati, content, layerId = id)
      # }
      # 
      # observe({
      #   leafletProxy(m) %>% clearPopups()
      #   event <- input$map_shape_click
      #   if (is.null(event))
      #     return()
      #   
      #   isolate({
      #     testfunction(event$id, event$lat, event$lng)
      #   })
      # })
      
      ### END OF TRY THIS
      
    