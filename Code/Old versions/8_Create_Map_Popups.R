
### Add every 1000 km of cycling?

# 1. Accommodation
    
  # Keep relevant data
    lookup_table_accommodation <- lookup_table_accommodation[,c("Date",
                                                                "Destination",
                                                                "Accommodation.type",
                                                                "Accommodation.name",
                                                                "Country",
                                                                "Blog")]
  # Select icons
    lookup_table_accommodation$icon <- NA
    
    lookup_table_accommodation$icon[lookup_table_accommodation$group=="Hotel"] <- 
      "https://pixabay.com/get/ea35b60729f31c22d2524518a33219c8b66ae3d111b7144991f5c07a/bed-307817_1280.png"
    
    lookup_table_accommodation$icon[lookup_table_accommodation$group=="Warmshowers"] <- 
      "https://pbs.twimg.com/profile_images/2707735661/6acfde74054ca7fd083031a072da3908_400x400.png"
    
    lookup_table_accommodation$icon[lookup_table_accommodation$group=="Camping"] <- 
      "http://crec.unl.edu/images/Icons/OA_Tent_red.png"
    
    lookup_table_accommodation$icon[lookup_table_accommodation$group=="Bus"] <- 
      "https://image.flaticon.com/icons/png/128/0/308.png"
    
    lookup_table_accommodation$icon[lookup_table_accommodation$group=="Other"] <- 
      "https://d30y9cdsu7xlg0.cloudfront.net/png/6393-200.png"
    
  # Make unknown names blank
    lookup_table_accommodation$Accommodation.name[
      lookup_table_accommodation$Accommodation.name=="[UNKNOWN]" |
        lookup_table_accommodation$Accommodation.name=="[NA]"] <- ""
    

# 2. Records
    
    # Select icons
    lookup_table_records$icon <- NA
    
    lookup_table_records$icon[lookup_table_records$type=="Compass"] <-
      "http://www.freeiconspng.com/uploads/compass-icon-24.png"
    lookup_table_records$icon[lookup_table_records$type=="Altitude"] <-
      "http://crec.unl.edu/images/Icons/OA%20Mountain%201%20Red.png"
    lookup_table_records$icon[lookup_table_records$type=="Temperature"] <-
      "http://www.iconsdb.com/icons/preview/red/thermometer-2-xxl.png"
    lookup_table_records$icon[lookup_table_records$type=="Speed"] <-
      "http://365psd.com/images/istock/thumbs/2559/25594879-simple-speedometer-icon.jpg"
    