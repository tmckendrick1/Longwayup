extragpxdata <- data.frame()

for(i in 1:length(file.names.gpx.extra)){
  pfile <- htmlTreeParse(file.names.gpx.extra[i],
                         error = function(...) {}, 
                         useInternalNodes = T)
  
  # Get all elevations, times and coordinates via the respective xpath
  elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
  times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
  coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
  atemp <- xpathSApply(pfile, path = "//trkpt/extensions", xmlValue)
  
  # Extract latitude and longitude from the coordinates
  lats <- as.numeric(coords["lat",])
  lons <- as.numeric(coords["lon",])
  
  id<-1:length(lats)
  
  # Assume we have lats and lons. If not need another line of code to stop the file!
  
  if (assertthat::not_empty(times)) print("ok") else times<-as.numeric(rep(NA,length(lats)))
  if (assertthat::not_empty(elevations)) print("ok") else elevations<-as.numeric(rep(NA,length(lats)))
  if (assertthat::not_empty(atemp)) print("ok") else atemp<-as.numeric(rep(NA,length(lats)))
  filename <- rep(file.names.gpx.extra.short[i],length(lats))
  
  assign(file.names.gpx.extra.short[i],data.frame(lat = lats, lon = lons, ele = elevations, time = times,temp=atemp,filename=filename,id=id))
  rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords","atemp","filename","id"))
  
  data.frames<-list("extragpxdata",file.names.gpx.extra.short[i])
  extragpxdata<-do.call(rbind,lapply(data.frames,get))
  
  remove.list<-as.character(file.names.gpx.extra.short[1])
  rm(list=remove.list)
}

extragpxdata$datetemp <- as.POSIXlt(ymd(substring(extragpxdata$filename,1,8)))
extragpxdata$time<-NULL

extragpxdata$speed.m.sfit<-as.numeric(NA)
extragpxdata$distdifffit<-as.numeric(NA)

extragpxdata$activity<-substring(extragpxdata$filename,nchar(as.character(extragpxdata$filename[1]))-4,nchar(as.character(extragpxdata$filename[1]))-4)
extragpxdata$order<-as.numeric(substring(extragpxdata$filename,10,11))

extragpxdata$filetype<- "Extra GPX Data"
extragpxdata$filename<-NULL