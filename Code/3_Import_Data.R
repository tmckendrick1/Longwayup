
# 1. FIT and GPX Files to go in map

  # 1.a. Generate file names
    true_type <- matrix(c("T","F",
                        "long","short"),
                      ncol=2,byrow=F)
  
    for (file_type in c("fit","gpx")) {
      for (type in 1:nrow(true_type)) {
      assign(paste0("file.names.",true_type[type,2],".",file_type,sep=""),
             list.files(path="Files",
                        pattern=paste0("*.",file_type,sep=""),
                        full.names=as.logical(true_type[type,1]),
                        recursive=FALSE))
      }
    }
    rm(file_type,type)
  
  ### Merge these two together or at least coordinate the way the cleaning is done
  # 1.b. Load and clean FIT files
    if (assertthat::not_empty(file.names.long.fit)==TRUE) {
      source ("Code/3a_Import_FIT_Data.R") ### STILL NEED TO DO: TIME ZONES
    }
    
    if (assertthat::not_empty(file.names.long.fit)==FALSE) {
      data.fit<-data.frame()
    }
  
  # 1.c. Load and clean GPX files
    if (assertthat::not_empty(file.names.long.gpx)==TRUE) {
      file_lookup_table<-read.csv("Files/Extra File Lookups.csv",stringsAsFactors = FALSE)
      file_lookup_table$File.name.full<-paste0(file_lookup_table$File.name,".",file_lookup_table$File.Type,sep="")
      source ("Code/3b_Import_GPX_Data.R")
      data.gpx<-data.gpx.temp
      rm(data.gpx.temp,file_lookup_table)
    }
    
    if (assertthat::not_empty(file.names.long.gpx)==FALSE) {
      data.gpx<-data.frame()
    }
  
    
  # 1.d. Merge the FIT and GPX data frames
    geodf<-do.call(rbind,lapply(as.list(ls(pattern="data.")),get))
      geodfcomp=geodf[!is.na(geodf$lat) & !is.na(geodf$lon),] # Need files with coordinates
    rm(geodf,list=ls(pattern="data."))
    
  # 1.e. Order merged dataset and generate distances beteen points
    geodfcomp <- geodfcomp[order(
      geodfcomp$date,
      geodfcomp$order_relative_to_cycle,
      geodfcomp$daily_order,
      geodfcomp$datetime,
      geodfcomp$id.file),
    ]
    geodfcomp$id.total <- 1:nrow(geodfcomp)
    
    geodfcomp$distdiff <- haversine_path(geodfcomp$lat,geodfcomp$lon)
    
  # 1.f. Process activity changes
    source("Code/3c_Activity_Changes.R")
  
# 2. RidewithGPS files to go in map depending on breakpoints
    
  # 2.a. Generate file names
    for (file_type in c("gpx")) {
      for (type in 1:nrow(true_type)) {
        assign(paste0("file.names.",true_type[type,2],".",file_type,sep=""),
               list.files(path="RidewithGPS Files",
                          pattern=paste0("*.",file_type,sep=""),
                          full.names=as.logical(true_type[type,1]),
                          recursive=FALSE))
      }
    }
    rm(file_type,type)
  
  # 2.b. Load and clean RidewithGPS files
    if (assertthat::not_empty(file.names.long.gpx)==TRUE) {
      file_lookup_table<-read.csv("RidewithGPS Files/RidewithGPS File Lookups.csv",stringsAsFactors = FALSE)
      file_lookup_table$File.name.full<-paste0(file_lookup_table$File.name,".",file_lookup_table$File.Type,sep="")
      source ("Code/3b_Import_GPX_Data.R")
      ridewithgpsdata<-data.gpx.temp
      rm(data.gpx.temp,file_lookup_table)
    }
    
    if (assertthat::not_empty(file.names.long.gpx)==FALSE) {
      ridewithgpsdata<-data.frame()
    }
    
    rm(true_type,list=ls(pattern="file.names"))
    
  # 2.c. Order merged dataset and generate distances beteen points
    ridewithgpsdata <- ridewithgpsdata[order(
      ridewithgpsdata$date,
      ridewithgpsdata$order_relative_to_cycle,
      ridewithgpsdata$daily_order,
      ridewithgpsdata$datetime,
      ridewithgpsdata$id.file),
    ]
    ridewithgpsdata$id.total <- 1:nrow(ridewithgpsdata)
    ridewithgpsdata$distdiff <- haversine_path(ridewithgpsdata$lat,ridewithgpsdata$lon)
