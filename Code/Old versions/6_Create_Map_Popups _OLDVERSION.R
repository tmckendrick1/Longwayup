
### NEED TO ADD DAILY DISTANCE AND ELEVATION
### NEED TO ADD ACCOMMODATION DATA IN SEPARATE DATA FRAME
### BLOGS TO BE INCORPORATED MUCH EARLIER IN DATA CLEANING

# 1. Daily Records
# m <- addMarkers(m, lng=wp_match$lon, lat=wp_match$lat,  
#                 popup=paste0("DETAILS")
#                 icon = photoIcon, # function providing custom marker-icons
#                 group="Daily Records)

### DAILY DISTANCE AND ELE WONT BE RIGHT AS NEED TO RETURN DATE AND LOOK UP FROM THERE, NOT
### OBS NUMBER, AS USING TWO DIFFERENT DATASETS
  popups_data_overall <- data.frame(name=c("Furthest North",
                                   "Furthest South",
                                   "Furthest East",
                                   "Furthest West",
                                   "Highest Altitude",
                                   "Maximum Temperature",
                                   "Minimum Temperature",
                                   "Fastest Speed",
                                   "Maximum Daily Distance",
                                   "Maximum Daily Elevation Gain"),
                            func=c("which.max",
                                   "which.min",
                                   "which.max",
                                   "which.min",
                                   "which.max",
                                   "which.max",
                                   "which.min",
                                   "which.max",
                                   "which.max",
                                   "which.max"),
                            measure=c("geodfcomprwg$lat",
                                      "geodfcomprwg$lat",
                                      "geodfcomprwg$lon",
                                      "geodfcomprwg$lon",
                                      "geodfcomprwg$ele",
                                      "geodfcomprwg$temp",
                                      "geodfcomprwg$temp",
                                      "geodfcomprwg$speed.m.sfit",
                                      "daily.stats$distance[daily.stats$activity==\"C\"]", ### TECHNICALLY SHOULD DO THIS BY DATE AND ACTIVITY, NOT DATE ACTIVITY AND COUNTRY
                                      "daily.stats$elegain.vanilla[daily.stats$activity==\"C\"]"),
                            unit=c("degrees",
                                   "degrees",
                                   "degrees",
                                   "degrees",
                                   "m",
                                   "degrees",
                                   "degrees",
                                   "m/s", ### Need to xply by 3.6 for km/h
                                   "km", ### NEED TO MAKE SURE DATA IS IN KM BY THIS POINT
                                   "m")
  )
  popups_data_overall$lat <- popups_data_overall$lon <- popups_data_overall$date <- NA

  for (i in 1:nrow(popups_data_overall)) {
    condition<-eval(parse(text=paste0(popups_data_overall$func[i],"(",popups_data_overall$measure[i],")",sep="")))
    popups_data_overall$lat[i] <- geodfcomprwg$lat[condition]
    popups_data_overall$lon[i] <- geodfcomprwg$lon[condition]
    popups_data_overall$date[i] <- as.character(geodfcomprwg$date[condition])
    popups_data_overall$desc[i] <- paste0(round(eval(parse(text=paste0(popups_data_overall$measure[i],"[",condition,"]",sep=""))),2)," ",popups_data_overall$unit[i],sep="")
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
    
  
  
  
  
    
  
  
