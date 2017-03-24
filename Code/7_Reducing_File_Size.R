
# Need to start with this so rule below works
  obsToDelete<-1

# Keep running code until nothing left to delete
  while (obsToDelete>0) {

    # Assign a new ID each time due to the collapse command below
    geodfcomprwg$id.total <- 1:nrow(geodfcomprwg)

    # Mark observations eligible for deletion
    geodfcomprwg$toDeletePre <- 
      as.numeric(geodfcomprwg$distdiff<dist_cutoff &
                   !((geodfcomprwg$activity!=lag(geodfcomprwg$activity,0))
                 | (geodfcomprwg$main_route!=lag(geodfcomprwg$main_route,0))
                 | (geodfcomprwg$country!=lag(geodfcomprwg$country,0))
                 | (geodfcomprwg$date!=lag(geodfcomprwg$date,0))))

    # String functions:
      # Turn eligible for deletion vector into a string with a separator to allow a later split
      toDeletePreString <- paste(geodfcomprwg$toDeletePre,collapse=",")
      # Ensure no consecutive observations can be deleted
      toDeleteString <- gsub("1,1","1,0",toDeletePreString)
      # Split back into the dataframe
      geodfcomprwg$toDelete <- as.numeric(unlist(strsplit(toDeleteString,",")))
      # Remove temporary vectors and strings
      rm(toDeletePreString,toDeleteString)
      geodfcomprwg$toDeletePre<-NULL

    # Mark whether we have anything for deletion
      obsToDelete<-sum(geodfcomprwg$toDelete)
    
    # Assign to the data to be deleted the previous observation's id, for collapsing
      geodfcomprwg$id.total[geodfcomprwg$toDelete==1]<-
        lag(geodfcomprwg$id.total,0)[geodfcomprwg$toDelete==1]

    # Collapse by id.total
      geodfcomprwg<-
        geodfcomprwg %>%
        group_by(id.total) %>%
        summarise(lat=last(lat),
                  lon=last(lon),
                  ele=last(ele),
                  distdiff=sum(distdiff),
                  date=last(date),
                  datetime=last(datetime),
                  filetype=last(filetype),
                  country=last(country),
                  blog=last(blog),
                  activity=last(activity),
                  main_route=last(main_route),
                  body_text=last(body_text)
        )
  }

# Remove remaining unneeded objects
  rm(dist_cutoff,obsToDelete)