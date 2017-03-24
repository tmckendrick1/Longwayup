### TRY THIS INSTEAD

geodfcomprwg<-data.frame()
# Observation number immediately after breakpoint
breakpoint.after.cell<-which(geodfcomp$distdiff>max.break.distance &
                               geodfcomp$filename != 
                               lag(geodfcomp$filename,geodfcomp$filename[1]))
# Obvservation number immediately before breakpoint
breakpoint.before.cell<-breakpoint.after.cell-1

after.id.total<-as.vector(NA) ### WAS breakpoint.after.rwgmatch.id.total
before.id.total<-as.vector(NA) ### As above for before
after.dist<-as.vector(NA)
before.dist<-as.vector(NA)

if (length(breakpoint.after.cell)>0) {
  
  for(k in 1:length(breakpoint.after.cell)){
    
    condition <- (ridewithgpsdata$date<=geodfcomp$date[breakpoint.after.cell[k]] &
                    ridewithgpsdata$date>=geodfcomp$date[breakpoint.before.cell[k]])
    
    if (nrow(ridewithgpsdata[
      condition,
      ])>0) {
      
      after.id.total[k] <- min_distance_obs(ridewithgpsdata$lat,
                                            ridewithgpsdata$lon,
                                            geodfcomp$lat[breakpoint.after.cell[k]],
                                            geodfcomp$lon[breakpoint.after.cell[k]],
                                            ridewithgpsdata$id.total,
                                            condition)[1]
      after.dist[k] <- min_distance_obs(ridewithgpsdata$lat,
                                        ridewithgpsdata$lon,
                                        geodfcomp$lat[breakpoint.after.cell[k]],
                                        geodfcomp$lon[breakpoint.after.cell[k]],
                                        ridewithgpsdata$id.total,
                                        condition)[2]
      
      
      before.id.total[k] <- min_distance_obs(ridewithgpsdata$lat,
                                            ridewithgpsdata$lon,
                                            geodfcomp$lat[breakpoint.before.cell[k]],
                                            geodfcomp$lon[breakpoint.before.cell[k]],
                                            ridewithgpsdata$id.total,
                                            condition)[1]
      before.dist[k] <- min_distance_obs(ridewithgpsdata$lat,
                                             ridewithgpsdata$lon,
                                             geodfcomp$lat[breakpoint.before.cell[k]],
                                             geodfcomp$lon[breakpoint.before.cell[k]],
                                             ridewithgpsdata$id.total,
                                             condition)[2]
    } else {
      after.id.total[k]<-before.id.total[k]<-after.dist[k]<-before.dist[k]<-NA
    }
    
  # Create the new dataset
    
    # Assign to dataframe "geodfcompk" the (uniquely specified data from geodfcomp 
    # we want to have immediately before RidewithGPS data (if valid)
    if(k==1) {
      assign(paste0("geodfcomp",k,sep=""),
             geodfcomp[1:breakpoint.before.cell[k],])
    }
    if(k>1) {
      assign(paste0("geodfcomp",k,sep=""),
             geodfcomp[breakpoint.after.cell[k-1]:breakpoint.before.cell[k],])
    }
    
    # Need to tail the new dataset with the last bit of geodf data
    # To do this, create every time and overwrite every time we have a valid breakpoint
    geodfcomptail<-geodfcomp[breakpoint.after.cell[k]:nrow(geodfcomp),]
    
    # Extract relevant RidewithGPS data, but
    # only if matching points are less than 300m away, and they are in order
  
    # If not, just merge the datasets (less the tail) together without inserting new data.
    # Remember the head is the geodfcomprwg dataset, which starts out as empty
    
    if(is.na(before.dist[k])==FALSE) {
    
      if((before.dist[k]<max.rwg.match.distance & 
          after.dist[k]<max.rwg.match.distance & 
          before.id.total[k]<after.id.total[k])==FALSE) {
        
        data.to.append<-list("geodfcomprwg",paste0("geodfcomp",k,sep=""))
        geodfcomprwg<-do.call(rbind,lapply(data.to.append,get))
        
        # Remove the temporary datasets
        rm(list=as.character(paste0("geodfcomp", k, sep = "")))
        rm(data.to.append)
        
      }
      
      
      # If valid, instead insert thw RidewithGPS data in the middle
      if((before.dist[k]<max.rwg.match.distance & 
          after.dist[k]<max.rwg.match.distance & 
          before.id.total[k]<after.id.total[k])==TRUE) {
        
        assign(paste0("matchdata",k,sep=""),
               ridewithgpsdata[(ridewithgpsdata$id.total>=before.id.total[k]
                               & ridewithgpsdata$id.total<=after.id.total[k]),])
                 
        # Leave the tail for now
        data.to.append<-list("geodfcomprwg",paste0("geodfcomp",k,sep=""),paste0("matchdata",k,sep=""))
        geodfcomprwg<-do.call(rbind,lapply(data.to.append,get))
  
        # Remove the temporary datasets
        rm(list=as.character(paste0("matchdata", k, sep = "")))
        rm(list=as.character(paste0("geodfcomp", k, sep = "")))
        rm(data.to.append)
      }
    }
    
    if(is.na(before.dist[k])==TRUE) {
      
        data.to.append<-list("geodfcomprwg",paste0("geodfcomp",k,sep=""))
        geodfcomprwg<-do.call(rbind,lapply(data.to.append,get))
        
        # Remove the temporary datasets
        rm(list=as.character(paste0("geodfcomp", k, sep = "")))
        rm(data.to.append)
        
    }
      
      
  }
  rm(k, before.dist, after.dist, before.id.total, after.id.total)
  
  # Now attach the tail
  data.to.append<-list("geodfcomprwg","geodfcomptail")
  geodfcomprwg<-do.call(rbind,lapply(data.to.append,get))
    
  # Remove the vectors used
  rm(geodfcomptail,data.to.append,list=ls(pattern="breakpoint"))

} else {
  geodfcomprwg<-geodfcomp
}

# Remove old datasets
rm(geodfcomp,ridewithgpsdata)

# Create id total field for new data
geodfcomprwg$id.total<-1:nrow(geodfcomprwg)

# REPORTING

# Run Haversine on amended dataset

geodfcomprwg$distdiff <- haversine_path(geodfcomprwg$lat,geodfcomprwg$lon)
geodfcomprwg$distdiff[1]<-NA
breakpoint.after.cell<-which(geodfcomprwg$distdiff>max.break.distance &
                               geodfcomprwg$filename != 
                               lag(geodfcomprwg$filename,geodfcomprwg$filename[1]))
breakpoint.before.cell<-breakpoint.after.cell-1
geodfcomprwg$distdiff[1]<-0

if (length(breakpoint.after.cell)>0) {
  # Get vectors of dates, lats and longs to plot on map
  breakpoint.data<-cbind(geodfcomprwg[breakpoint.after.cell,c("date")],
                         geodfcomprwg[breakpoint.before.cell,c("lat","lon")],
                         geodfcomprwg[breakpoint.after.cell,c("lat","lon")]
                         )
  names(breakpoint.data)<-c("date","before_lat","before_lon","after_lat","after_lon")
} else {
  breakpoint.data<- NA
}
# Remove remaining unnecessary variables
rm(breakpoint.after.cell, breakpoint.before.cell)