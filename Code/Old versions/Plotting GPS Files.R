setwd("C:/Users/pss3c/Desktop/Route/R Files/")

rm(list=ls())

library(XML)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gdata)

file.names <- list.files(path="Sample Garmin Files",
                    pattern="*.gpx", 
                    full.names=T, 
                    recursive=FALSE)
file.names.short <- list.files(path="Sample Garmin Files",
                               pattern="*.gpx", 
                               full.names=F, 
                               recursive=FALSE)

geodf <- data.frame()

for(i in 1:length(file.names)){
  pfile <- htmlTreeParse(file.names[i],
                         error = function(...) {}, 
                         useInternalNodes = T)

                         
# Parse the GPX file
#pfile <- htmlTreeParse("C:/Users/pss3c/Desktop/Route/R Files/Sample Garmin Files/activity_1425875451.gpx",
                       #error = function(...) {}, 
                       #useInternalNodes = T)

# Get all elevations, times and coordinates via the respective xpath
elevations <- as.numeric(xpathSApply(pfile, path = "//trkpt/ele", xmlValue))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
atemp <- xpathSApply(pfile, path = "//trkpt/extensions", xmlValue)

# Extract latitude and longitude from the coordinates
lats <- as.numeric(coords["lat",])
lons <- as.numeric(coords["lon",])

# Put everything in a dataframe and get rid of old variables
assign(file.names.short[i],data.frame(lat = lats, lon = lons, ele = elevations, time = times,temp=atemp))
rm(list=c("elevations", "lats", "lons", "pfile", "times", "coords","atemp"))


data.frames<-list("geodf",file.names.short[i])
geodf<-do.call(rbind,lapply(data.frames,get))

keep (geodf,file.names,file.names.short,i,sure=TRUE)
#geodf<-do.call(rbind,lapply(file.names.short,get))

}


# Time
geodf$time <- strptime(geodf$time,
                       format = "%Y-%m-%dT%H:%M:%OS")
geodf$date <- paste(day(geodf$time),
                    "/",
                    month(geodf$time),
                    "/",
                    year(geodf$time),
                    sep="")
geodf$time<-sprintf("%02d:%02d:%02d",
                     hour(geodf$time),
                     minute(geodf$time),
                     second(geodf$time))
geodf$datetime<-dmy_hms(paste(geodf$date,geodf$time))


# Create indicator variables - will need to do this with merge later
geodf$cycle<-(day(geodf$datetime)==27)*1
geodfcycle <- geodf[geodf$cycle == 1, ]
geodfnoncycle<- geodf[geodf$cycle == 0, ]

# Load in Map e.g. Google Map
mapImageData <- get_map(location = c(lon = mean(geodf$lon),
                                     lat = median(geodf$lat)),
                        color = "color", 
                        source = "google",
                        maptype = "hybrid",
                        zoom = 10)

# Set track colour
cyclepathcolor <- "#F8971F"
noncyclepathcolor <- "#FF0000"

# Map
ggmap(mapImageData,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") + 
  geom_path(aes(x = lon, y = lat),
            data = geodf,
            colour = "black",
            size = 2) + #Bigger path in black for outline
  geom_path(aes(x = lon, y = lat),
            colour = cyclepathcolor,
            data = geodfcycle,
            size = 1.4) +
  geom_path(aes(x = lon, y = lat),
          colour = noncyclepathcolor,
          data = geodfnoncycle,
          size = 1.4) 



