# 1. Keep only relevant variables

  geodfcomprwg<-geodfcomprwg[,
                             c("lat",
                               "lon",
                               "ele",
                               "distdiff",
                               "date",
                               "datetime",
                               "country",
                               "blog",
                               "filetype",
                               "activity",
                               "main_route",
                               "body_text")
                             ]
  keep(
       # Data for App
         geodfcomprwg,
         lookup_table_stats_and_records,
       # Object used in this file
         max_to_delete_allowed,
         dist_cutoff,
       # Functions
         list=lsf.str(),
       sure=TRUE)

# 2. Remove datapoints with zero distance
  geodfcomprwg<-geodfcomprwg[geodfcomprwg$distdiff!=0,]

# 3. Algorithm for deleting groups of least important observations sequentially
  obs.to.delete <- max_to_delete_allowed+1
  
  while (obs.to.delete>max_to_delete_allowed) { # Doing this to cut down on the time taken to remove a long tail
    
    # NEW ID
    geodfcomprwg$id.total <- 1:nrow(geodfcomprwg)
    
    # 3. Sort observations into groups of where dist diff is less than 5 metres
      # The essence of this code is to not delete consecutive points with small differences as they may
      # accumulate to a lot
    
    geodfcomprwg$distance.groups <- as.numeric(geodfcomprwg$distdiff<dist_cutoff &
                                                  lag(geodfcomprwg$distdiff,0)>=dist_cutoff)
                                               
    geodfcomprwg$distance.groups[1]<-0
    geodfcomprwg$distance.groups<-cumsum(geodfcomprwg$distance.groups)+1
    geodfcomprwg$distance.groups[geodfcomprwg$distdiff>=dist_cutoff] <- NA
    
    
      # Create a separate matrix with id and group
      delete.me.table<-geodfcomprwg[!is.na(geodfcomprwg$distance.groups),
                                    c("id.total","distdiff","distance.groups")]
      # Mark for deletion smallest distdiff in each
  
      delete.me.table<-delete.me.table[order(
        delete.me.table$distance.groups,
        delete.me.table$distdiff,
        delete.me.table$id.total),
      ]
      
      delete.me.table$delete <- as.numeric(delete.me.table$distance.groups!=
                                             lag(delete.me.table$distance.groups,0))
      
      delete.ids<-delete.me.table$id.total[delete.me.table$delete==1]
      
      geodfcomprwg$to.delete<-0
      geodfcomprwg$to.delete[delete.ids]<-1

      geodfcomprwg$to.delete[(geodfcomprwg$activity!=lag(geodfcomprwg$activity,0))
                             | (geodfcomprwg$main_route!=lag(geodfcomprwg$main_route,0))
                             | (geodfcomprwg$country!=lag(geodfcomprwg$country,0))
                             | (geodfcomprwg$date!=lag(geodfcomprwg$date,0))]<-0
      
    # Remove if distance is less than threshold
  
      # Assign to the data to be deleted the previous observation's id, for collapsing
      geodfcomprwg$id.total[geodfcomprwg$to.delete==1]<-
        lag(geodfcomprwg$id.total,0)[geodfcomprwg$to.delete==1]
      
      
      # Mark whether we have anything for deletion
      obs.to.delete=sum(geodfcomprwg$to.delete)
      
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

# 4, Keep what we need
  ### This line may not be needed?
  geodfcomprwg<-geodfcomprwg[!is.na(geodfcomprwg$activity),]
  rm(dist_cutoff,obs.to.delete,max_to_delete_allowed,delete.me.table,delete.ids)