
activity_change_lookup_table<-read.csv("Files/Activity Change Lookups.csv",stringsAsFactors = FALSE, colClasses = c("character"))

# Clean up variables
numeric_variables <- c("Start.Lats","Start.Lons","End.Lats","End.Lons")
for (variable in 1:length(numeric_variables)) {
  activity_change_lookup_table[[numeric_variables[variable]]] <- 
    as.numeric(activity_change_lookup_table[[numeric_variables[variable]]])
}
rm(variable, numeric_variables)
activity_change_lookup_table$Date<-as.POSIXlt(dmy(activity_change_lookup_table$Date))

# Start the code
end.id.total<-as.vector(NA)
start.id.total<-as.vector(NA)

for (k in 1:nrow(activity_change_lookup_table)) {
  
  if (sum(geodfcomp$date==activity_change_lookup_table$Date[k])>0) {
    condition <- geodfcomp$date==activity_change_lookup_table$Date[k]
  } else {
    condition <- T
  }
  
  if (is.na(activity_change_lookup_table$End.Lats[k]) | 
      is.na(activity_change_lookup_table$End.Lons[k])) {
        end.id.total[k] <- max(geodfcomp$id.total[
          condition
        ]) 
  } else {
          end.id.total[k] <- min_distance_obs(geodfcomp$lat,
                   geodfcomp$lon,
                   activity_change_lookup_table$End.Lats[k],
                   activity_change_lookup_table$End.Lons[k],
                   geodfcomp$id.total,
                   condition
                   )[1]
  }
  
  if (is.na(activity_change_lookup_table$Start.Lats[k]) & 
      is.na(activity_change_lookup_table$Start.Lons[k])) {
        start.id.total[k] <- min(geodfcomp$id.total[
          condition
          ]) 
  } else {
        start.id.total[k] <- min_distance_obs(geodfcomp$lat,
                                      geodfcomp$lon,
                                      activity_change_lookup_table$Start.Lats[k],
                                      activity_change_lookup_table$Start.Lons[k],
                                      geodfcomp$id.total,
                                      condition
                                      )[1]
  }
  
  geodfcomp$activity[geodfcomp$id.total>=start.id.total[k] &
                       geodfcomp$id.total<=end.id.total[k]] <-
    activity_change_lookup_table$Activity.New[k]
}

rm(start.id.total, end.id.total, activity_change_lookup_table)