
# 1. Create accommodation lookup table

  lookup_table_accommodation <- lookup_table_date

# 2. Populate with country and blog data. If there is an entry for country2, call it this,
  # otherwise call it country1. Likewise for blogs
  lookup_table_accommodation$Country<-lookup_table_accommodation$Country1
  lookup_table_accommodation$Country[lookup_table_accommodation$Country2!=""]<-
    lookup_table_accommodation$Country2[lookup_table_accommodation$Country2!=""]
  
  lookup_table_accommodation$Blog<-lookup_table_accommodation$Blog1
  lookup_table_accommodation$Blog[lookup_table_accommodation$Blog2!=""]<-
    lookup_table_accommodation$Blog2[lookup_table_accommodation$Blog2!=""]
 
# 3. Generate latitude and longitude from last observation of that day 
  
  lookup_table_accommodation$lat<-
  lookup_table_accommodation$lon<-NA
  
  for (i in 1:nrow(lookup_table_accommodation)) {
    lookup_table_accommodation$lat[i]<-
      last(geodfcomprwg$lat[geodfcomprwg$date<=lookup_table_accommodation$Date[i]])
    lookup_table_accommodation$lon[i]<-
      last(geodfcomprwg$lon[geodfcomprwg$date<=lookup_table_accommodation$Date[i]]) 
  }
  
# 4. Generate 'group' variable, for icons later
  lookup_table_accommodation$group <- NA
  
  
  lookup_table_accommodation$group[lookup_table_accommodation$Accommodation.type=="Hotel" |
                                    lookup_table_accommodation$Accommodation.type=="Hostel" |
                                    lookup_table_accommodation$Accommodation.type=="Hospedaje" |
                                    lookup_table_accommodation$Accommodation.type=="Basic Accommodation" |
                                    lookup_table_accommodation$Accommodation.type=="Cabana" |
                                    lookup_table_accommodation$Accommodation.type=="Refugio"] <-
    "Hotel"
  
  lookup_table_accommodation$group[lookup_table_accommodation$Accommodation.type=="Warmshowers"] <- 
    "Warmshowers"
  
  lookup_table_accommodation$group[lookup_table_accommodation$Accommodation.type=="Campsite" |
                                    lookup_table_accommodation$Accommodation.type=="Wild Camping"] <-
    "Camping"
  
  lookup_table_accommodation$group[lookup_table_accommodation$Accommodation.type=="Overnight bus"] <- 
    "Bus"
  
  lookup_table_accommodation$group[lookup_table_accommodation$Accommodation.type=="Other"] <- 
    "Other"
  
