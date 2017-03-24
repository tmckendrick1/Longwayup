data.extragpx <- data.frame()

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
  filename <- rep(file.names.gpx.extra.short[i],ncol(coords))
  
  assign(file.names.gpx.extra.short[i],data.frame(lat = lats, lon = lons, ele = elevations, time = times,temp=atemp,filename=filename,id=id))
  rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords","atemp","filename","id"))
  
  data.frames<-list("data.extragpx",file.names.gpx.extra.short[i])
  data.extragpx<-do.call(rbind,lapply(data.frames,get))
  
  remove.list<-as.character(file.names.gpx.extra.short[i])
  rm(list=remove.list)
}

data.extragpx$datetemp <- as.POSIXlt(ymd(substring(data.extragpx$filename,1,8)))
data.extragpx$time<-NULL

data.extragpx$speed.m.sfit<-as.numeric(NA)
data.extragpx$distdifffit<-as.numeric(NA)

data.extragpx$activity<-substring(data.extragpx$filename,nchar(as.character(data.extragpx$filename[1]))-4,nchar(as.character(data.extragpx$filename[1]))-4)
data.extragpx$order<-as.numeric(substring(data.extragpx$filename,10,11))

data.extragpx$filetype<- "Extra GPX Data"
data.extragpx$filename<-NULL