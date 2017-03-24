
# Create empty data frame to populate
  data.gpx.temp <- data.frame()

# Vectors of variables
  
  fitvariables <- c("distdifffit","speed.m.sfit")

  variable_renames_gpx <- matrix(c("","coords","xmlAttrs",
                             "/ele","elevations","xmlValue",
                             "/time","times","xmlValue",
                             "/extensions","temp","xmlValue"),
                             ncol=3,byrow=T)  
  
  
    ### AT PRESENT EXTENSIONS ONLY TEMP. NEED TO REVISIT MUCH LATER ON
  
for(i in 1:length(file.names.long.gpx)){
  
  # 1. Extract useful information from gpx file
  pfile <- htmlTreeParse(file.names.long.gpx[i],
                         error = function(...) {}, 
                         useInternalNodes = T)
  
  for (rownumber in 1:nrow(variable_renames_gpx)) {
    
    pathvalue<-paste0("//trkpt",variable_renames_gpx[rownumber,1],sep="")
    ### WONT WORK IF WE USE GPS FILES WITH A TIMESTAMP
    
    if (variable_renames_gpx[rownumber,2]!="times") {
      assign(variable_renames_gpx[rownumber,2],
            as.numeric(xpathSApply(pfile, path = pathvalue, variable_renames_gpx[rownumber,3])))
    } else {
      assign(variable_renames_gpx[rownumber,2],
             xpathSApply(pfile, path = pathvalue, variable_renames_gpx[rownumber,3]))
    }
    
    if (rownumber==1) numberofrows<-length(get(variable_renames_gpx[rownumber,2]))/2 # Coords assumed to exist
    
    if (assertthat::not_empty(get(variable_renames_gpx[rownumber,2]))==FALSE)
        assign(variable_renames_gpx[rownumber,2],
               as.numeric(rep(NA,numberofrows))) 
    ### Need to think about returning error if coordinates don't exist and stop code
        
  }
  rm(rownumber)
  
  # 2. Variable creation and modification
    
    # File name
      filename <- rep(file.names.short.gpx[i],numberofrows)
      
    # Latitude and longitude
      lats <- matrix(coords,nrow=2)[1,]
      lons <- matrix(coords,nrow=2)[2,]
      
    # fit variables - so we can merge with FIT data
      for (fitvariable in 1:length(fitvariables)) {
        assign(fitvariables[fitvariable],as.numeric(rep(NA,numberofrows)))
      }
      rm(fitvariable)
    
    # Date and time

      date<-as.numeric(rep(NA,numberofrows))
      datetime<-as.numeric(rep(NA,numberofrows))
      
      if (!all(is.na(times))) {
        datetemp <- strptime(times,format = "%Y-%m-%dT%H:%M:%OS")
      }
      
      if (all(is.na(times))) {
        datetemp <- rep(as.POSIXlt(
          dmy(file_lookup_table$Date[
            file_lookup_table$File.name.full==file.names.short.gpx[i]])),
          numberofrows)
        #datetemp <- as.POSIXlt(ymd(substring(filename,1,8)))
      }
      
      datestr <- sprintf("%02d/%02d/%04d",
                                      day(datetemp),
                                      month(datetemp),
                                      year(datetemp))
      timestr<-sprintf("%02d:%02d:%02d",
                                    hour(datetemp),
                                    minute(datetemp),
                                    second(datetemp))
      date<-dmy(paste(datestr))
      datetime<-dmy_hms(paste(datestr,timestr))
      
      rm(datetemp,times,timestr,datestr)
      
      # ID
      id.file<-1:numberofrows
      
      # If unmatched in table, set to cycling
      activity<- rep("Biking",numberofrows)
      main_route<-rep(1, numberofrows)
      order_relative_to_cycle <- rep(2,numberofrows)
      daily_order <- rep(1,numberofrows)
      
      # If matched, change
      if (length(file_lookup_table$Activity[
        file_lookup_table$File.name.full==file.names.short.gpx[i]])>0) {
        
        filetype <- rep(
          "GPX",
          numberofrows
        )
        
        activity <- rep(
          file_lookup_table$Activity[
            file_lookup_table$File.name.full==file.names.short.gpx[i]],
          numberofrows)
        
        order_relative_to_cycle <- recode(
          rep(file_lookup_table$Before.After.FIT.File..Default...B.[
            file_lookup_table$File.name.full==file.names.short.gpx[i]],
          numberofrows),
          "B"=1,"A"=3
        )
      
        daily_order <- rep(
          file_lookup_table$Daily.Order..Order.in.File.if.Blank.[
            file_lookup_table$File.name.full==file.names.short.gpx[i]],
          numberofrows)
        
        main_route <- rep(
          file_lookup_table$Main.Route[
            file_lookup_table$File.name.full==file.names.short.gpx[i]],
          numberofrows)
      
      }

    
    # 3. Create data frame and merge with existing uploaded data
    
    #assign(file.names.short.gpx[i],data.frame(lat = lats, lon = lons, distdifffit = distdifffit, speed.m.sfit = speed.m.sfit, ele = elevations, temp=temp, date=date, datetime=datetime, id=id, activity=activity, order=order))
    assign(file.names.short.gpx[i],
           data.frame(lat = lats,
                      lon = lons,
                      distdifffit = distdifffit,
                      speed.m.sfit = speed.m.sfit,
                      ele = elevations,
                      temp = temp,
                      date = date,
                      datetime = datetime,
                      id.file = id.file,
                      filetype = filetype,
                      activity = activity,
                      order_relative_to_cycle = order_relative_to_cycle,
                      daily_order = daily_order,
                      main_route = main_route,
                      filename = filename
                      )
           )
    
    ### TRY TO LINK THIS TO THE VECTORS UP FRONT AS MUCH AS POSSIBLE
    rm(list=c("elevations", "coords","temp","speed.m.sfit","distdifffit","lats", "lons", "id.file","filetype","activity","order_relative_to_cycle","daily_order","main_route","filename","date","datetime"))
    
    dataframes<-list("data.gpx.temp",file.names.short.gpx[i])
    data.gpx.temp<-do.call(rbind,lapply(dataframes,get))
    
    remove.list<-as.character(file.names.short.gpx[i]) ### NEED TO DO THESE TWO LINES BETTER
    rm(list=remove.list,remove.list,pathvalue,numberofrows,dataframes)
}
  rm(i,variable_renames_gpx,pfile,fitvariables)
