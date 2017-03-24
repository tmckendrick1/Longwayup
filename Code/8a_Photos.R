
##### DON'T RUN THIS FILE AGAIN: source("Code/8x_Getting_Photo_Timestamps.R") #####

load("R Saved Data/Photo Data.RData")

# Will we have a date match in the file?
photo_data$datematch <-
  sapply(photo_data$date, 
         function(x) max(geodfcomprwg$date==x))

# Will we have a datetime match in the file?
photo_data$datetimematch <-
  sapply(photo_data$date,
         function(x) max(geodfcomprwg$date==x & geodfcomprwg$filetype=="FIT"))


photo_data$info_lookup<-
  photo_data$lat<-
  photo_data$lon<-NA

# Where we have a datetime match
condition<-photo_data$datetimematch==1 & !is.na(photo_data$datetime)

photo_data$info_lookup[condition] <-
  sapply(photo_data$datetime[condition], 
         function(x) which.min(abs(geodfcomprwg$datetime - x)))

# Where we have a date only match
condition<-photo_data$datetimematch==0 &
  photo_data$datematch==1 &
  !is.na(photo_data$datetime)

for (i in 1:sum(condition)) {
  datecond<-geodfcomprwg$date==photo_data$date[condition][i]
  fraction<-as.numeric((photo_data$datetime[condition][i]-as.POSIXlt(photo_data$date[condition][i]))/24)
  cumdist<-cumsum(geodfcomprwg$distdiff[datecond])
  lookup_id<-geodfcomprwg$id.total[datecond][which.min(abs(cumdist-max(cumdist)*fraction))]
  photo_data$info_lookup[condition][i]<-which(geodfcomprwg$id.total==lookup_id)
}
rm(datecond,fraction,cumdist,lookup_id,condition)
                       
                       
photo_data$lat<- # IS THIS A PROBLEM IF LOOKUP IS NA?
  sapply(photo_data$info_lookup, function(x) geodfcomprwg$lat[x])
photo_data$lon<-
  sapply(photo_data$info_lookup, function(x) geodfcomprwg$lon[x])
photo_data$country<-
  sapply(photo_data$info_lookup, function(x) geodfcomprwg$country[x])
photo_data$blog<-
  sapply(photo_data$info_lookup, function(x) geodfcomprwg$blog[x])


# One way around unmatched photos is to assume a day begins at 9am and finishes at 6pm.
# If a file that day, take the fraction of the way through the day we are 
# and apply the photo to the file from that day.

# If no file that day, must be (close to) the end point of the previous file we have.
# However this will mean lots of photos for one particular point. So may want to be more specific.
# For now keep it as NA

