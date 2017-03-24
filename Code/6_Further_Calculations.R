
# 1. Cumulative distance and elevation calculations
  geodfcomprwg$id.total <- 1:nrow(geodfcomprwg)

  # 1.a. Create 'levels' vectors
    datelevels<-unique(geodfcomprwg$date)
    activitylevels <- unique(geodfcomprwg$activity)
    countrylevels <- unique(geodfcomprwg$country)
    bloglevels <- unique(geodfcomprwg$blog)

  # 1.b. Cumulative distance
    
    # Turn distances from meters into kilometers
      geodfcomprwg$distdiff <- geodfcomprwg$distdiff/1000
      geodfcomprwg$distdifffit <- geodfcomprwg$distdifffit/1000
    
 
  # 1.c. Elevation gain and loss
    
    # Isolate elevation gains and losses
      geodfcomprwg$elegain <- pmax(firstdiff(geodfcomprwg$ele,0),0)
      geodfcomprwg$eleloss <- -pmin(firstdiff(geodfcomprwg$ele,0),0)

    # Generate distances associated with elevation gains, losses and levels
      ### Presumably a neater way of doing this using conditionals
      geodfcomprwg$elegain.distdiff <- 
        geodfcomprwg$eleloss.distdiff <-
        geodfcomprwg$elezero.distdiff <-
        0
      
      geodfcomprwg$elegain.distdiff[geodfcomprwg$elegain>0] <- 
        geodfcomprwg$distdiff[geodfcomprwg$elegain>0]
      geodfcomprwg$eleloss.distdiff[geodfcomprwg$eleloss>0] <- 
        geodfcomprwg$distdiff[geodfcomprwg$eleloss>0]
      geodfcomprwg$elezero.distdiff[geodfcomprwg$elegain==0 & 
                                              geodfcomprwg$eleloss==0] <- 
        geodfcomprwg$distdiff[geodfcomprwg$elegain==0 & geodfcomprwg$eleloss==0]
      
    
# 2. Records by country and blog (for popups)
    
  # 2.a. Create skeleton table - must be done to some extent manually
    
    # Create a temporary lookup table without blog or country data
      lookup_table_records_temp <- 
        data.frame(
          # Type: for popup icons
          record_type=c("Compass",
                        "Compass",
                        "Compass",
                        "Compass",
                        "Altitude",
                        "Temperature",
                        "Temperature",
                        "Speed"),
          # Name: for reference and for popups
          record_name=c("Furthest North",
                       "Furthest South",
                       "Furthest East",
                       "Furthest West",
                       "Highest Altitude",
                       "Maximum Temperature",
                       "Minimum Temperature",
                       "Fastest Speed"),
          # Function - is it a max or min?
          record_func=c("which.max",
                       "which.min",
                       "which.max",
                       "which.min",
                       "which.max",
                       "which.max",
                       "which.min",
                       "which.max"),
          # Variable to which the function is applied
          record_variable=c("geodfcomprwg$lat",
                            "geodfcomprwg$lat",
                            "geodfcomprwg$lon",
                            "geodfcomprwg$lon",
                            "geodfcomprwg$ele",
                            "geodfcomprwg$temp",
                            "geodfcomprwg$temp",
                            "geodfcomprwg$speed.m.sfit"),
          # Unit - for popup
          record_unit=c("degrees latitude",
                       "degrees latitude",
                       "degrees longitude",
                       "degrees longitude",
                       "m",
                       "degrees C",
                       "degrees C",
                       "m/s") ### Need to xply by 3.6 for km/h - do this much earlier in code
        )
      
      lookup_table_records_temp$record_lat <- 
      lookup_table_records_temp$record_lon <- 
      lookup_table_records_temp$record_date <- 
      lookup_table_records_temp$record_desc <- 
      lookup_table_records_temp$record_cond <-  NA
      
      # Record the number of measures we have
      measures=nrow(lookup_table_records_temp)
      
      # Create a copy of the temporary table with blank country and blog data
      lookup_table_records<-lookup_table_records_temp
      lookup_table_records$country<-NA
      lookup_table_records$blog <-NA
      
      # Create a matrix of unique country and blog combinations, including "overall"
      unique_country_blog<-rbind(unique(geodfcomprwg[,c("country","blog")]),
                                 cbind(country=countrylevels,blog="Overall"),
                                 cbind(country="Overall",blog="Overall"))
      
      # Expand out the lookup table by the number of these unique combinations
      for (i in 1:nrow(unique_country_blog)) {
        lookup_table_records<-rbind(lookup_table_records,cbind(country=unique_country_blog$country[i],
                                                             blog=unique_country_blog$blog[i],
                                                             lookup_table_records_temp))
      }
      rm(i)
      
      # Remove the temporary lookup table
      rm(lookup_table_records_temp)
    
      # Keep only a subset of the table with non-NA entries in country and blog
      lookup_table_records<-lookup_table_records[
        !is.na(lookup_table_records$country)&
          !is.na(lookup_table_records$blog),]

  # 2.b. Evaluate the records
      
    # Populate the 'cond' field
      
      # For overall records, include everything in data in calculations
      lookup_table_records$record_cond[lookup_table_records$country=="Overall"] <- 
        "TRUE"
      
      # For country records, include only data for that country
      lookup_table_records$record_cond[lookup_table_records$country!="Overall" & 
                                 lookup_table_records$blog=="Overall"] <- 
        "geodfcomprwg$country==unique_country_blog$country[country_blog_num]"
      
      # For blog records, include only data for that country and blog
      lookup_table_records$record_cond[lookup_table_records$country!="Overall" & 
                                 lookup_table_records$blog!="Overall"] <- 
        "geodfcomprwg$country==unique_country_blog$country[country_blog_num] & geodfcomprwg$blog==unique_country_blog$blog[country_blog_num]"
      
      # Populating table
      for (measure in 1:measures) {
        for (country_blog_num in 1:nrow(unique_country_blog)) {
          
          # Selecting which group of cells to update
          popups_condition <- (lookup_table_records$country==unique_country_blog$country[country_blog_num] & 
                                 lookup_table_records$blog==unique_country_blog$blog[country_blog_num])
          # Selecting which part of the geodfcomprwg database to look through
          data_condition <- eval(parse(text=lookup_table_records$record_cond[popups_condition][measure]))
          # Generate the code to run
          overall_condition<-eval(parse(text=paste0(lookup_table_records$record_func[popups_condition][measure],
                                                    "(",
                                                    lookup_table_records$record_variable[popups_condition][measure]
                                                    ,"[",lookup_table_records$record_cond[popups_condition],"]",
                                                    ")",
                                                    sep="")))
          
          # If there is no data to look through, give it an NA
          if (length(overall_condition)==0) {
            lookup_table_records$record_lat[popups_condition][measure] <- NA
            lookup_table_records$record_lon[popups_condition][measure] <- NA
            lookup_table_records$record_date[popups_condition][measure] <- NA
            lookup_table_records$record_desc[popups_condition][measure] <- NA
          # Else, evaluate the data
          } else { 
            lookup_table_records$record_lat[popups_condition][measure] <- geodfcomprwg$lat[data_condition][overall_condition]
            lookup_table_records$record_lon[popups_condition][measure] <- geodfcomprwg$lon[data_condition][overall_condition]
            lookup_table_records$record_date[popups_condition][measure] <- as.character(geodfcomprwg$date[data_condition][overall_condition])
            lookup_table_records$record_desc[popups_condition][measure] <- paste0(round(eval(parse(text=paste0(lookup_table_records$record_variable[popups_condition][measure],"[",lookup_table_records$record_cond[popups_condition][measure],"]","[",overall_condition,"]",sep=""))),2)," ",lookup_table_records$record_unit[popups_condition][measure],sep="")
          }
        }
      }
      
      # Remove unneeded vectors
      rm(measure,
         measures,
         overall_condition,
         popups_condition,
         data_condition,
         unique_country_blog,
         activitylevels,
         countrylevels,
         bloglevels,
         datelevels,
         country_blog_num)
      
      # Remove unneeded variables in records lookup table
      
      lookup_table_records <- lookup_table_records[,c("country",
                                                      "blog",
                                                      "record_name",
                                                      "record_desc",
                                                      "record_date",
                                                      "record_lon",
                                                      "record_lat",
                                                      "record_type")]
      
      # Makes collapse easier in app later on
      lookup_table_records$record_id <- 1:nrow(lookup_table_records)
      
# 3. Data by day, over a variety of measures

  # Average Elevation and Temperature: First create two temporary variables
      geodfcomprwg$dist_times_ele<-geodfcomprwg$distdiff*geodfcomprwg$ele
      geodfcomprwg$dist_times_temp<-geodfcomprwg$distdiff*geodfcomprwg$temp
      
      
  stats_activity_date_country_blog<-
    geodfcomprwg %>%
    group_by(date,country,blog,activity) %>%
    summarise(id_start=first(id.total),
              destination=last(destination),
              accommodation_name=last(accommodation_name),
              accommodation_type=last(accommodation_type),
              lat_start=first(lat),
              lon_start=first(lon),
              lat_end=last(lat),
              lon_end=last(lon),
              distance=sum(distdiff,na.rm=TRUE),
              daydistmax=sum(distdiff,na.rm=TRUE),
              elemax=max(ele,na.rm=TRUE),
              elemin=min(ele,na.rm=TRUE),
              elegain=sum(elegain,na.rm=TRUE),
              eleloss=sum(eleloss,na.rm=TRUE),
              dayelegainmax=sum(elegain,na.rm=TRUE),
              dayelelossmax=sum(eleloss,na.rm=TRUE),
              elegain.dist=sum(elegain.distdiff,na.rm=TRUE),
              eleloss.dist=sum(eleloss.distdiff,na.rm=TRUE),
              tempmax=max(temp,na.rm=TRUE),
              tempmin=min(temp,na.rm=TRUE),
              speedmax=max(speed.m.sfit,na.rm=TRUE),
              dist_times_ele=sum(dist_times_ele,na.rm=TRUE),
              dist_times_temp=sum(dist_times_temp,na.rm=TRUE)
    )
  
  # Variables needed for distance and elegain maxima
  stats_activity_date_country_blog$daydistmax_date<- 
    stats_activity_date_country_blog$dayelegainmax_date<-
    stats_activity_date_country_blog$dayelelossmax_date<-
    stats_activity_date_country_blog$date
  
  # Remove temporary variables
    geodfcomprwg$dist_times_ele<-
      geodfcomprwg$dist_times_temp<-NULL
  
  # Total day and rest day analysis
  
  # At the date, country, blog level
    
    # First define it for each activity
    stats_activity_date_country_blog$rest_day_activity <- rest_day(stats_activity_date_country_blog)
    
    # Then collapse across activities
    daydata_blog<-
      stats_activity_date_country_blog %>%
      group_by(date,country,blog) %>%
      summarise(rest_day=min(rest_day_activity) # Min so only if nothing has a zero is it a rest day.
      )
    
    # Then collapse by blog to get the number of rest days per blog entry
    daydata_blog<-
      daydata_blog %>%
      group_by(country,blog) %>%
      summarise(rest_days=sum(rest_day), # Min so only if nothing has a zero is it a rest day.
                days=n()
      )

  # At the date, country level

    # First collapse to this level (including activity)
    tempdata_country<-
      stats_activity_date_country_blog %>%
      group_by(date,country,activity) %>%
      summarise(distance=sum(distance), # Collapsing by date so can't cover more than one date
                elegain=sum(elegain) # Min so only if nothing has a zero is it a rest day.
      )
    
    # Then define a rest day
    tempdata_country$rest_day<-rest_day(tempdata_country)

    # Then collapse across activities
    daydata_country<-
      tempdata_country %>%
      group_by(date,country) %>%
      summarise(rest_day=min(rest_day),
                blog="Overall"
      )
    rm(tempdata_country)

    # Then collapse by country to get the number of rest days per country
    daydata_country<-
      daydata_country %>%
      group_by(country) %>%
      summarise(rest_days=sum(rest_day), # Min so only if nothing has a zero is it a rest day.
                days=n(),
                blog="Overall"
      )
    # At the date level
    
    # First collapse to this level (including activity)
    tempdata_overall<-
      stats_activity_date_country_blog %>%
      group_by(date,activity) %>%
      summarise(distance=sum(distance), # Collapsing by date so can't cover more than one date
                elegain=sum(elegain) # Min so only if nothing has a zero is it a rest day.
      )
    
    # Then define a rest day
    tempdata_overall$rest_day<-rest_day(tempdata_overall)
    
    # Then collapse across activities
    daydata_overall<-
      tempdata_overall %>%
      group_by(date) %>%
      summarise(rest_day=min(rest_day),
                country="Overall",
                blog="Overall"
      )
    rm(tempdata_overall)
    
    # Then sum to get the number of rest days
    daydata_overall<-
      data.frame(
        country="Overall",
        blog="Overall",
        days=nrow(daydata_overall),
        rest_days=sum(daydata_overall$rest_day)
        
      )

    daydata <- bind_rows(data.frame(daydata_blog),
                                    data.frame(daydata_country),
                                    data.frame(daydata_overall))
    daydata$date<-"Overall"
                                    
    rm(daydata_blog, daydata_country, daydata_overall)
    
    
    # Rest of analysis
    
  stats_activity_country_blog<-
    stats_activity_date_country_blog %>%
    group_by(country,blog,activity) %>%
    summarise(id_start=first(id_start),
              distance=sum(distance,na.rm=TRUE),
              daydistmax=max(daydistmax,na.rm=TRUE),
              daydistmax_date=date[which.max(daydistmax)],
              elemax=max(elemax,na.rm=TRUE),
              elemin=min(elemin,na.rm=TRUE),
              elegain=sum(elegain,na.rm=TRUE),
              eleloss=sum(eleloss,na.rm=TRUE),
              dayelegainmax=max(dayelegainmax,na.rm=TRUE),
              dayelelossmax=max(dayelelossmax,na.rm=TRUE),
              dayelegainmax_date=date[which.max(dayelegainmax)],
              dayelelossmax_date=date[which.max(dayelelossmax)],
              elegain.dist=sum(elegain.dist,na.rm=TRUE),
              eleloss.dist=sum(eleloss.dist,na.rm=TRUE),
              tempmax=max(tempmax,na.rm=TRUE),
              tempmin=min(tempmin,na.rm=TRUE),
              speedmax=max(speedmax,na.rm=TRUE),
              dist_times_ele=sum(dist_times_ele,na.rm=TRUE),
              dist_times_temp=sum(dist_times_temp,na.rm=TRUE),
              date="Overall"
    )
  
  # Two blogs can happen on the same day, so alter the 'days' variable to compensate
  
  stats_activity_country<-
    stats_activity_date_country_blog %>%
    group_by(country,activity) %>%
    summarise(id_start=first(id_start),
              distance=sum(distance,na.rm=TRUE),
              daydistmax=max(daydistmax,na.rm=TRUE),
              daydistmax_date=date[which.max(daydistmax)],
              elemax=max(elemax,na.rm=TRUE),
              elemin=min(elemin,na.rm=TRUE),
              elegain=sum(elegain,na.rm=TRUE),
              eleloss=sum(eleloss,na.rm=TRUE),
              dayelegainmax=max(dayelegainmax,na.rm=TRUE),
              dayelelossmax=max(dayelelossmax,na.rm=TRUE),
              dayelegainmax_date=date[which.max(dayelegainmax)],
              dayelelossmax_date=date[which.max(dayelelossmax)],
              elegain.dist=sum(elegain.dist,na.rm=TRUE),
              eleloss.dist=sum(eleloss.dist,na.rm=TRUE),
              tempmax=max(tempmax,na.rm=TRUE),
              tempmin=min(tempmin,na.rm=TRUE),
              speedmax=max(speedmax,na.rm=TRUE),
              dist_times_ele=sum(dist_times_ele,na.rm=TRUE),
              dist_times_temp=sum(dist_times_temp,na.rm=TRUE),
              date="Overall",
              blog="Overall"
    )
  
  stats_activity<-
    stats_activity_date_country_blog %>%
    group_by(activity) %>%
    summarise(id_start=first(id_start),
              distance=sum(distance,na.rm=TRUE),
              daydistmax=max(daydistmax,na.rm=TRUE),
              daydistmax_date=date[which.max(daydistmax)],
              elemax=max(elemax,na.rm=TRUE),
              elemin=min(elemin,na.rm=TRUE),
              elegain=sum(elegain,na.rm=TRUE),
              eleloss=sum(eleloss,na.rm=TRUE),
              dayelegainmax=max(dayelegainmax,na.rm=TRUE),
              dayelelossmax=max(dayelelossmax,na.rm=TRUE),
              dayelegainmax_date=date[which.max(dayelegainmax)],
              dayelelossmax_date=date[which.max(dayelelossmax)],
              elegain.dist=sum(elegain.dist,na.rm=TRUE),
              eleloss.dist=sum(eleloss.dist,na.rm=TRUE),
              tempmax=max(tempmax,na.rm=TRUE),
              tempmin=min(tempmin,na.rm=TRUE),
              speedmax=max(speedmax,na.rm=TRUE),
              dist_times_ele=sum(dist_times_ele,na.rm=TRUE),
              dist_times_temp=sum(dist_times_temp,na.rm=TRUE),
              date="Overall",
              country="Overall",
              blog="Overall"
    )
  
  stats_activity_date_country_blog$date <- as.character(stats_activity_date_country_blog$date)
  lookup_table_stats <- bind_rows(data.frame(stats_activity_date_country_blog),
                        data.frame(stats_activity_country_blog),
                        data.frame(stats_activity_country),
                        data.frame(stats_activity))
  
  rm(stats_activity_date_country_blog,
     stats_activity_country_blog,
     stats_activity_country,
     stats_activity)
  
  lookup_table_stats <-
    merge(lookup_table_stats,
          daydata,
          by=c("country","blog","date"),
          all=TRUE)
  rm(daydata)
  
  lookup_table_stats$rest_days[is.na(lookup_table_stats$rest_days)]<-
    lookup_table_stats$rest_day_activity[is.na(lookup_table_stats$rest_days)]
  lookup_table_stats$days[is.na(lookup_table_stats$days)]<-1
  lookup_table_stats$non_rest_days<-lookup_table_stats$days-lookup_table_stats$rest_days
    
  lookup_table_stats$rest_day_activity<-NULL
  
    # Ensure a useful ordering
    lookup_table_stats<-
      lookup_table_stats[order(
        lookup_table_stats$id_start),
        ]
  
    # Calculations that require an average across multiple observations
    lookup_table_stats$avg_gradient <- 
      (lookup_table_stats$elegain +
         lookup_table_stats$eleloss)/
      lookup_table_stats$distance/10 # Divide by 1000, x by 100
    
    lookup_table_stats$avg_ascent_gradient <- 
      (lookup_table_stats$elegain)/
      lookup_table_stats$elegain.dist/10
    
    lookup_table_stats$avg_descent_gradient <- 
      (lookup_table_stats$eleloss)/
      lookup_table_stats$eleloss.dist/10

    
    lookup_table_stats$wa_ele<-
      lookup_table_stats$dist_times_ele/lookup_table_stats$distance
    lookup_table_stats$wa_temp<-
      lookup_table_stats$dist_times_temp/lookup_table_stats$distance
    

    lookup_table_stats$avg_distance<-
      lookup_table_stats$distance/lookup_table_stats$non_rest_days
    lookup_table_stats$avg_elegain <-
      lookup_table_stats$elegain/lookup_table_stats$non_rest_days
    lookup_table_stats$avg_eleloss <-
      lookup_table_stats$eleloss/lookup_table_stats$non_rest_days  
    
  # Merge daily stats with records
    lookup_table_stats_and_records <- 
      merge(lookup_table_stats,
            lookup_table_records,
            by=c("country","blog"),
            all=TRUE
            )
    
    lookup_table_stats_and_records<-
      lookup_table_stats_and_records[order(lookup_table_stats_and_records$id_start,
                                           lookup_table_stats_and_records$record_id),]
    
    rm(lookup_table_stats, lookup_table_records)
    
    lookup_table_stats_and_records$record_name<-as.character(lookup_table_stats_and_records$record_name)
    lookup_table_stats_and_records$record_type<-as.character(lookup_table_stats_and_records$record_type)
    

    
    
  # Data for creating GPS profile
    geodfcomprwg$body_text<-
      paste(
        "<trkpt lat=\"",
        geodfcomprwg$lat,
        "\" lon=\"",
        geodfcomprwg$lon,
        "\"><ele>",
        geodfcomprwg$ele,
        "</ele></trkpt>",
        sep=""
      )
