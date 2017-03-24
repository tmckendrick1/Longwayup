
### NEED TO ADD DAILY DISTANCE AND ELEVATION. To do this first break down daily stats into three
### - daily overall, daily by country, daily by country and blog (each by activity)
### ADD ACCOMMODATION DATA IN SEPARATE DATA FRAME
### BLOGS TO BE INCORPORATED MUCH EARLIER IN DATA CLEANING
### THEN BLOG ALSO TO BE ADDED HERE

# 1. Daily Records
# m <- addMarkers(m, lng=wp_match$lon, lat=wp_match$lat,  
#                 popup=paste0("DETAILS")
#                 icon = photoIcon, # function providing custom marker-icons
#                 group="Daily Records)


  popups_records_data <- data.frame(name=c("Furthest North",
                                           "Furthest South",
                                           "Furthest East",
                                           "Furthest West",
                                           "Highest Altitude",
                                           "Maximum Temperature",
                                           "Minimum Temperature",
                                           "Fastest Speed"
                                           ),
                                    func=c("which.max",
                                           "which.min",
                                           "which.max",
                                           "which.min",
                                           "which.max",
                                           "which.max",
                                           "which.min",
                                           "which.max"
                                           ),
                                 measure=c("geodfcomprwg$lat",
                                           "geodfcomprwg$lat",
                                           "geodfcomprwg$lon",
                                           "geodfcomprwg$lon",
                                           "geodfcomprwg$ele",
                                           "geodfcomprwg$temp",
                                           "geodfcomprwg$temp",
                                           "geodfcomprwg$speed.m.sfit"
                                           ),
                                    unit=c("degrees latitude",
                                           "degrees latitude",
                                           "degrees longitude",
                                           "degrees longitude",
                                           "m",
                                           "degrees C",
                                           "degrees C",
                                           "m/s" ### Need to xply by 3.6 for km/h - do this much earlier in code
                                           )
  )
    popups_records_data$lat <- 
    popups_records_data$lon <- 
    popups_records_data$date <- 
    popups_records_data$desc <- NA
    
    popups_records_data$blog <- "Overall" ### ONLY FOR NOW

    popups_records_data_temp <- popups_records_data
    popups_records_data$country<-"Overall"
    
    measures=nrow(popups_records_data)
    
    for (country in countrylevels) {
      popups_records_data<-rbind(popups_records_data,cbind(country=country,popups_records_data_temp))
    }
    rm(country, popups_records_data_temp)

    for (measure in 1:measures) {
      for (country in c("Overall",countrylevels)) {
      
        if (country=="Overall") {
          sub_condition_text <- "TRUE"
        } else {
          sub_condition_text <- "geodfcomprwg$country==country"
        }
          sub_condition <- eval(parse(text=paste0(sub_condition_text)))
          condition_2 <- popups_records_data$country==country
          condition<-eval(parse(text=paste0(popups_records_data$func[condition_2][measure],
                                            "(",
                                            popups_records_data$measure[condition_2][measure]
                                            ,"[",sub_condition_text,"]",
                                            ")",
                                            sep="")))
        

        popups_records_data$lat[condition_2][measure] <- geodfcomprwg$lat[sub_condition][condition]
        popups_records_data$lon[condition_2][measure] <- geodfcomprwg$lon[sub_condition][condition]
        popups_records_data$date[condition_2][measure] <- as.character(geodfcomprwg$date[sub_condition][condition])
        popups_records_data$desc[condition_2][measure] <- paste0(round(eval(parse(text=paste0(popups_records_data$measure[condition_2][measure],"[",sub_condition_text,"]","[",condition,"]",sep=""))),2)," ",popups_records_data$unit[condition_2][measure],sep="")
        
      }
    }
  


    
    # Greatest Daily Distance
    for (i in 1:length(activitylevels)) {
      row <- row+1 
      condition<-geodfcomprwg$id.total==popups.data$id.total[row]
      popups.data = rbind(popups.data,c(rep(NA,4)))
      popups.data$name[row]<-paste0("Greatest Daily Distance",activitylevels[i],sep="")
      
      
      if (nrow(daily.stats[daily.stats$activity==activitylevels[i],])>0) {
  
        popups.data$date[row]<-as.character(daily.stats[daily.stats$activity==activitylevels[i],
                                           ]$date[
                                             which.max(daily.stats[
                                               daily.stats$activity==activitylevels[i],
                                               ]$distance)])
        
        popups.data$id.total[row] <- min(geodfcomprwg$id.total[geodfcomprwg$date==popups.data$date[row]
                                                           & 
                                                             geodfcomprwg$activity==activitylevels[i]])
        
        popups.data$lat[row] <- geodfcomprwg$lat[condition]
        popups.data$lon[row] <- geodfcomprwg$lon[condition]
        popups.data$desc[row] <- paste0(round(daily.stats$distance
                                              [daily.stats$date==popups.data$date[row] & 
                                                  daily.stats$activity == activitylevels[i]]/1000
                                              ,1),"km"," (",popups.data$date[row],")", sep="")
      }
    }
    # Greatest Daily Elevation Gain
    for (i in 1:length(activitylevels)) {
      row <- row+1
      condition<-geodfcomprwg$id.total==popups.data$id.total[row]
      popups.data = rbind(popups.data,c(rep(NA,4)))
      popups.data$name[row]<-paste0("Greatest Daily Elevation Gain",activitylevels[i],sep="")
      
      if (nrow(daily.stats[daily.stats$activity==activitylevels[i],])>0) {
        
        popups.data$date[row]<-as.character(daily.stats[daily.stats$activity==activitylevels[i],
                                           ]$date[
                                             which.max(daily.stats[
                                               daily.stats$activity==activitylevels[i],
                                               ]$elegain)])
        popups.data$id.total[row] <- min(geodfcomprwg$id.total[geodfcomprwg$date==popups.data$date[row]
                                                               & 
                                                                 geodfcomprwg$activity==activitylevels[i]])
        
        popups.data$lat[row] <- geodfcomprwg$lat[condition]
        popups.data$lon[row] <- geodfcomprwg$lon[condition]
        popups.data$desc[row] <- paste0(round(daily.stats$elegain
                                              [daily.stats$date==popups.data$date[row]& 
                                                  daily.stats$activity == activitylevels[i]],1),"m"," (",
                                        popups.data$date[row],")", sep="")
      }
    }
    
  
  
  
  
    
  
  
