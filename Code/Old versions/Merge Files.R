
### KEEP RIDEWITHGPS SEPARATE

geodfcomp <- geodfcomp[order(geodfcomp$date,geodfcomp$order,geodfcomp$datetime,geodfcomp$id),]
geodfcomp$id.file <- geodfcomp$id
geodfcomp$id <- NULL
geodfcomp$id.total <- 1:nrow(geodfcomp)

ridewithgpsdata <- ridewithgpsdata[order(ridewithgpsdata$date,ridewithgpsdata$order,ridewithgpsdata$datetime,ridewithgpsdata$id),]
ridewithgpsdata$id.file <- ridewithgpsdata$id
ridewithgpsdata$id <- NULL
ridewithgpsdata$id.total <- 1:nrow(ridewithgpsdata)

