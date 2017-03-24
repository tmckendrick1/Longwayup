
lookup_table_date<-read.csv("Files/Daily Lookups.csv",stringsAsFactors = FALSE)
lookup_table_date$Date<-as.POSIXlt(dmy(lookup_table_date$Date))

# 1. Date lookup - contains country information (only, at present) MERGE WITH BELOW

geodfcomprwg$country<- NA
geodfcomprwg$blog<- NA
id.total<-as.vector(NA)


for (k in 1:nrow(lookup_table_date)) {
  
  condition <- geodfcomprwg$date==lookup_table_date$Date[k]
  
  if (lookup_table_date$Intraday.Country.or.Blog.Change[k]==0) {
    geodfcomprwg$country[condition] <- 
      lookup_table_date$Country1[k]
    geodfcomprwg$blog[condition] <-
      lookup_table_date$Blog1[k]
  } else {
  
    if (is.na(lookup_table_date$Latitude[k]) | 
        is.na(lookup_table_date$Longitude[k])) {
      id.total[k] <- max(geodfcomprwg$id.total[
        condition
        ]) 
    } else {
      id.total[k] <- min_distance_obs(geodfcomprwg$lat,
                                          geodfcomprwg$lon,
                                          lookup_table_date$Latitude[k],
                                          lookup_table_date$Longitude[k],
                                          geodfcomprwg$id.total,
                                          condition
      )[1]
    }
    

    geodfcomprwg$country[geodfcomprwg$id.total<id.total[k] &
                         condition] <-
      lookup_table_date$Country1[k]
    
    geodfcomprwg$country[geodfcomprwg$id.total>=id.total[k] &
                         condition] <-
      lookup_table_date$Country2[k]
    
    geodfcomprwg$blog[geodfcomprwg$id.total<id.total[k] &
                           condition] <-
      lookup_table_date$Blog1[k]
    
    geodfcomprwg$blog[geodfcomprwg$id.total>=id.total[k] &
                           condition] <-
      lookup_table_date$Blog2[k]
    
  }
}
rm(k)

# 2. Accommodation lookup table

  lookup_table_accommodation <- lookup_table_date[,c("Date",
                                                     "Destination",
                                                     "Accommodation.type",
                                                     "Accommodation.name",
                                                     "Country1",
                                                     "Country2",
                                                     "Blog1",
                                                     "Blog2")]
  
  lookup_table_accommodation$Country<-lookup_table_accommodation$Country1
  lookup_table_accommodation$Country[lookup_table_accommodation$Country2!=""]<-
    lookup_table_accommodation$Country2[lookup_table_accommodation$Country2!=""]
  lookup_table_accommodation$Blog<-lookup_table_accommodation$Blog1
  lookup_table_accommodation$Blog[lookup_table_accommodation$Blog2!=""]<-
    lookup_table_accommodation$Blog2[lookup_table_accommodation$Blog2!=""]
  
  lookup_table_accommodation <- lookup_table_accommodation[,c("Date",
                                                     "Destination",
                                                     "Accommodation.type",
                                                     "Accommodation.name",
                                                     "Country",
                                                     "Blog")]
  
  lookup_table_accommodation$lat<-
  lookup_table_accommodation$lon<-NA
  
  for (i in 1:nrow(lookup_table_accommodation)) {
    lookup_table_accommodation$lat[i]<-
      last(geodfcomprwg$lat[geodfcomprwg$date<=lookup_table_accommodation$Date[i]])
    lookup_table_accommodation$lon[i]<-
      last(geodfcomprwg$lon[geodfcomprwg$date<=lookup_table_accommodation$Date[i]]) 
  }
  
  lookup_table_accommodation$icon <- NA
  
  lookup_table_accommodation$icon[lookup_table_accommodation$Accommodation.type=="Hotel" |
                                    lookup_table_accommodation$Accommodation.type=="Hostel" |
                                    lookup_table_accommodation$Accommodation.type=="Hospedaje" |
                                    lookup_table_accommodation$Accommodation.type=="Basic Accommodation" |
                                    lookup_table_accommodation$Accommodation.type=="Cabana" |
                                    lookup_table_accommodation$Accommodation.type=="Refugio"] <- "https://pixabay.com/get/ea35b60729f31c22d2524518a33219c8b66ae3d111b7144991f5c07a/bed-307817_1280.png"
  
  lookup_table_accommodation$icon[lookup_table_accommodation$Accommodation.type=="Warmshowers"] <- "https://pbs.twimg.com/profile_images/2707735661/6acfde74054ca7fd083031a072da3908_400x400.png"
  
  lookup_table_accommodation$icon[lookup_table_accommodation$Accommodation.type=="Campsite" |
                                    lookup_table_accommodation$Accommodation.type=="Wild Camping"] <- "http://crec.unl.edu/images/Icons/OA_Tent_red.png"
  
  lookup_table_accommodation$icon[lookup_table_accommodation$Accommodation.type=="Overnight bus"] <- "https://image.flaticon.com/icons/png/128/0/308.png"
  lookup_table_accommodation$icon[lookup_table_accommodation$Accommodation.type=="Other"] <- "https://d30y9cdsu7xlg0.cloudfront.net/png/6393-200.png"
  
  lookup_table_accommodation$Accommodation.name[
    lookup_table_accommodation$Accommodation.name=="[UNKNOWN]" |
      lookup_table_accommodation$Accommodation.name=="[NA]"] <- ""
    
  rm(lookup_table_date)
