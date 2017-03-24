
# Create empty columns
  geodfcomprwg$country<- NA
  geodfcomprwg$blog<- NA
  geodfcomprwg$destination<- NA
  geodfcomprwg$accommodation_type<- NA
  geodfcomprwg$accommodation_name<- NA

# Populate columns
  for (k in 1:nrow(lookup_table_date)) {
    
    condition <- geodfcomprwg$date==lookup_table_date$Date[k]
    
    # 1. Accommodation
      geodfcomprwg$destination[condition] <- 
        lookup_table_date$Destination[k]
      geodfcomprwg$accommodation_type[condition] <- 
        lookup_table_date$Accommodation.type[k]
      geodfcomprwg$accommodation_name[condition] <- 
        lookup_table_date$Accommodation.type[k]
    
    # 2. Country and Blog
      # If no intra-day change, or if latitude and longitude are NA:
      if (lookup_table_date$Intraday.Country.or.Blog.Change[k]!=1 |
          (lookup_table_date$Intraday.Country.or.Blog.Change[k]==1 &
          (is.na(lookup_table_date$Latitude[k]) | 
           is.na(lookup_table_date$Longitude[k])))) {
        
            # Country and blog are 'country 1' and 'blog 1'
            geodfcomprwg$country[condition] <- 
              lookup_table_date$Country1[k]
            geodfcomprwg$blog[condition] <-
              lookup_table_date$Blog1[k]
    
      # If there is an intra-day change and latitude aend longitude are not NA:
      } else {
        
        # Identify id.total associated with minimum distance to latitude and longitude provided
        condition_2 <- min_distance_obs(geodfcomprwg$lat,
                                        geodfcomprwg$lon,
                                        lookup_table_date$Latitude[k],
                                        lookup_table_date$Longitude[k],
                                        geodfcomprwg$id.total,
                                        condition)[1]
        
        # Where id is less than this value, country and blog are 'country 1' and 'blog 1'
        geodfcomprwg$country[geodfcomprwg$id.total<condition_2 &
                               condition] <-
          lookup_table_date$Country1[k]
       
        geodfcomprwg$blog[geodfcomprwg$id.total<condition_2 &
                            condition] <-
          lookup_table_date$Blog1[k]
        
        # Where id is greater than this value, country and blog are 'country 2' and 'blog 2'
        geodfcomprwg$country[geodfcomprwg$id.total>=condition_2 &
                               condition] <-
          lookup_table_date$Country2[k]
  
        geodfcomprwg$blog[geodfcomprwg$id.total>=condition_2 &
                            condition] <-
          lookup_table_date$Blog2[k]
        
      rm(condition_2)
      }
  
  }
  
  rm(k, condition)
