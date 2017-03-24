
# Function 1: Lag
lag <- function(var,first=NA) {
  append(head(var,-1),
         first,
         after=0)
}

# Function 2: First Diff
firstdiff <- function(var,first=NA) {
  append(tail(var,-1)
         -head(var,-1),
         first,
         after=0)
}  

# Function 3: Haversine
haversine <- function(v1lat,v1lon,v2lat,v2lon) {
  v1_lat_radians <- v1lat*pi/180
  v1_lon_radians <- v1lon*pi/180
  v2_lat_radians <- v2lat*pi/180
  v2_lon_radians <- v2lon*pi/180
  
  half_lat_radians_diff <- (v2_lat_radians-v1_lat_radians)/2
  half_lon_radians_diff <- (v2_lon_radians-v1_lon_radians)/2  
  
  dist_diff <- 2*6378137*asin(((sin(half_lat_radians_diff))^2 
                                           + (cos(v2_lat_radians)*
                                                cos(v1_lat_radians)*
                                                ((sin(half_lon_radians_diff))^2)))^0.5)
  dist_diff
}

# Function 4: Haversine path
haversine_path <- function(lat,lon) {
  haversine(lat,lon,lag(lat,lat[1]),lag(lon,lon[1]))
}

# Function 5: Obs min distance
min_distance_obs <- function(v1lat,v1lon,s1lat,s1lon,return_var,condition=T) {
  distances <- haversine(v1lat[condition],v1lon[condition],rep(s1lat,length(v1lat))[condition],rep(s1lon,length(v1lon))[condition])
  output <- c(return_var[condition][which.min(distances)],distances[which.min(distances)])
  output
}

# Function 5: Rest Day
rest_day <- function(dataset) {
  as.numeric((dataset$activity=="Biking" &
      (dataset$distance<20 &
         dataset$elegain<500)) |
     (dataset$activity=="Hiking" &
        (dataset$distance<5 &
           dataset$elegain<400)) |
     (dataset$activity=="Public Transport"))
}
