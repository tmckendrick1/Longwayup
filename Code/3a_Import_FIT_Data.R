
# Create empty data frame to populate
  data.fit <- data.frame()

# Vector of original names followed by new names
  variable_renames_fit <- matrix(c("position_lat.semicircles","lat",
                               "position_long.semicircles","lon",
                               "speed.m.s","speed.m.sfit",
                               "altitude.m","ele",
                               "temperature.C","temp",
                               "distance.m","distance.mfit",
                               "timestamp.s","time"),
                               ncol=2,byrow=T)  
  
for(i in 1:length(file.names.long.fit)){
  # 1. Extract useful information from FIT file
    data_mesgs <- try(read_fit(file.names.long.fit[i]))
    
    if (class(data_mesgs)!="try-error") {
      is_record <- function(mesg) mesg$name == "record"
      records <- Filter(is_record, data_mesgs)
      
      format_record <- function(record) {
        out <- record$fields
        names(out) <- paste(names(out), record$units, sep = ".")
        out
      }
      records <- lapply(records, format_record)
      
      colnames_full <- names(records[[which.max(lengths(records))]])
      empty <- setNames(
        as.list(rep(NA, length(colnames_full))),
        colnames_full)
      merge_lists <- function(ls_part, ls_full) {
        extra <- setdiff(names(ls_full), names(ls_part))
        append(ls_part, ls_full[extra])[names(ls_full)] # order as well
      }
      records <- lapply(records, merge_lists, empty)
      records <- data.frame(
        do.call(rbind, records))
      rm(data_mesgs)
      
      if (nrow(records)==1) {
        assign("records",data.frame(records))
      }
      if (nrow(records)>1) {
        assign("records",data.frame(sapply(records,unlist),stringsAsFactors = FALSE))
      }
    
    
    # 2. Variable creation and modification
    
      # Only run the code on FIT files that have latitude and longitude. If not, something is
      # already wrong
      
      records$filename <- file.names.short.fit[i]
      
      if ((exists("position_lat.semicircles",where=records) & 
           exists("position_long.semicircles",where=records))==TRUE) {
        
          for (rownumber in 1:nrow(variable_renames_fit)) {
            
            # 2.a. Ensure variables exist
            if (exists(variable_renames_fit[rownumber,1],where=records)==FALSE) records[[variable_renames_fit[rownumber,1]]]<-NA
            records[[variable_renames_fit[rownumber,1]]]<-as.numeric(records[[variable_renames_fit[rownumber,1]]])
            
            # 2.b. Rename variables
            records[[variable_renames_fit[rownumber,2]]]<-records[[variable_renames_fit[rownumber,1]]]
            records[[variable_renames_fit[rownumber,1]]]<-NULL
            
          }
        rm(rownumber)
  
          # Create ID variable
          records$id.file<-1:nrow(records)
            
          # 2.c. Modify variables where needed
            
            # 2.c.1. Distance
          
              # Replace NAs with 0s in Garmin cumulative distance variable
              records$distance.mfit[as.vector(which(is.na(records$distance.mfit)))]<-0
            
              # Generate variable as difference between distance records
              if(nrow(records) > 1) {
                records$distdifffit <- firstdiff(records$distance.mfit,0)
              }
              # Unless only one record
              if (nrow(records)==1) {
                records$distdifffit <- NA
              }
              
              # Remove Garmin cumulative distance variable
              records$distance.mfit<-NULL
            
          # 2.c.2 Latitude and Longitude
              
              # Convert to radians
              records$lat <- records$lat*(180/(2^31))
              records$lon <- records$lon*(180/(2^31))
      
          # 2.c.3. Date and Time (date, datetime)
              ### NEED TO CHECK TIME ZONES LATER
      
            records$datetemp <- as.POSIXct(records$time, origin = "1989-12-31")
            
            records$datestr <- sprintf("%02d/%02d/%04d",
                                        day(records$datetemp),
                                        month(records$datetemp),
                                        year(records$datetemp))
            records$timestr<-sprintf("%02d:%02d:%02d",
                                      hour(records$datetemp),
                                      minute(records$datetemp),
                                      second(records$datetemp))
            records$date<-dmy(paste(records$datestr))
            records$datetime<-dmy_hms(paste(records$datestr,records$timestr))
            
            records$time<-NULL
            records$datetemp<-NULL
            records$datestr<-NULL
            records$timestr<-NULL
            
          # 2.c.4. File type, Activity and Order
            
            records$filetype <- "FIT"
            records$activity<- "Biking"
            records$order_relative_to_cycle <- 2
            records$daily_order <- 1
            records$main_route <- 1
            
        # 3. Create data frame and merge with existing uploaded data
            assign(file.names.short.fit[i],records)
        
            rm(records,colnames_full,empty,format_record,is_record,merge_lists)
            
            dataframes<-list("data.fit",file.names.short.fit[i])
            data.fit<-do.call(rbind,lapply(dataframes,get))
            
            remove.list<-as.character(file.names.short.fit[i])
            rm(list=remove.list,remove.list,dataframes)
          }
          if (exists("records")==TRUE) {
            rm(records,colnames_full,empty,format_record,is_record,merge_lists)
          }
    }
}

  rm(i,variable_renames_fit)