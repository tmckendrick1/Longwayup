# Next steps: extract from this file and other GPS extraction file what is specific to
# FIT and GPS extraction. Make sure end result is dataset with same name and same variable
# names. Then put elevation profile and mapping into another file. Would be nice to be able
# to combine FIT and GPX files into a single data frame. Main problem will be with date
# where it isn't recorded in e.g. Strava file


setwd("C:/Users/pss3c/Desktop/Route/R Files/")

rm(list=ls())

library(XML)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gdata)
library(fitdc)

file.names <- list.files(path="Sample FIT Files",
                         pattern="*.fit", 
                         full.names=T, 
                         recursive=FALSE)
file.names.short <- list.files(path="Sample FIT Files",
                               pattern="*.fit", 
                               full.names=F, 
                               recursive=FALSE)

geodf <- data.frame()

#for(i in 1:length(file.names)){
for(i in 1:40){ 
 
data_mesgs <- read_fit(file.names[i])
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

assign("records",data.frame(sapply(records,unlist)))

# Change cumulative distance to distance diff
records$distdiff <- append(tail(records$distance.m,-1)
                   -head(records$distance.m,-1),
                   0,
                   after=0)
records$distance.m<-NULL
assign(file.names.short[i],records)


rm(records,colnames_full,empty,format_record,is_record,merge_lists)

data.frames<-list("geodf",file.names.short[i])
geodf<-do.call(rbind,lapply(data.frames,get))

keep (geodf,file.names,file.names.short,i,sure=TRUE)
}

geodf$lat <- geodf$position_lat.semicircles*(180/(2^31))
geodf$lon <- geodf$position_long.semicircles*(180/(2^31))

geodf$datetemp <- as.POSIXct(geodf$timestamp.s, origin = "1989-12-31")


geodf$date <- sprintf("%02d/%02d/%04d",
                    day(geodf$datetemp),
                    month(geodf$datetemp),
                    year(geodf$datetemp))
geodf$time<-sprintf("%02d:%02d:%02d",
                    hour(geodf$datetemp),
                    minute(geodf$datetemp),
                    second(geodf$datetemp))
geodf$datetime<-dmy_hms(paste(geodf$date,geodf$time))

geodf$ele <- geodf$altitude.m
geodf$temp <- geodf$temperature.C

geodf$timestamp.s<-NULL
geodf$position_lat.semicircles<-NULL
geodf$position_long.semicircles<-NULL
geodf$datetemp<-NULL
geodf$altitude.m<-NULL
geodf$temperature.C<-NULL

geodf <- geodf[order(geodf$datetime),]
geodfcomp<-geodf[complete.cases(geodf),]
rm(geodf)

geodfcomp$dist=cumsum(geodfcomp$distdiff)


# Elevation profile
geodfcomp$distkm <- geodfcomp$dist/1000
qplot(data=geodfcomp,x=distkm,y=ele,geom="line")

#### MAPPING

# Load in Map e.g. Google Map
mapImageData <- get_map(location = c(lon = median(geodfcomp$lon),
                                     lat = median(geodfcomp$lat)),
                        color = "color", 
                        source = "google",
                        maptype = "hybrid",
                        zoom = 4)

# Set track colour
pathcolor <- "#F8971F"

# Map
ggmap(mapImageData,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") + 
  geom_path(aes(x = lon, y = lat),
            data = geodfcomp,
            colour = "black",
            size = 2) + #Bigger path in black for outline
  geom_path(aes(x = lon, y = lat),
            colour = pathcolor,
            data = geodfcomp,
            size = 1.4)





