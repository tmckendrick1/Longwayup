
### Separate File - Don't Keep Running  

  # Define function to return time photo was taken
    exif_datetime <- function(path) {
      exif_datecmd <- 'exiftool -T -r -DateTimeOriginal '  
      datecmd <- paste(exif_datecmd, '"', path, '"', sep='')
      exif_timestamp <- system(datecmd, intern = TRUE)
      exif_timestamp
    }

  # Data frame for storing photo data
    photo_data<-data.frame(
      filenames = 
        list.files(path="Photos",
                   full.names=T,
                   recursive=FALSE),
      filenames_short = 
        list.files(path="Photos",
                   full.names=F,
                   recursive=FALSE),
      timestamp = NA
    )

    
    photo_data$timestamp<-
      sapply(photo_data$filenames, function(x) exif_datetime(x))
    photo_data$datetime<-ymd_hms(photo_data$timestamp)
    photo_data$date <- 
      dmy(sprintf("%02d/%02d/%04d",
                  day(ymd_hms(photo_data$timestamp)),
                  month(ymd_hms(photo_data$timestamp)),
                  year(ymd_hms(photo_data$timestamp))))
    
    save(photo_data,"R Saved Data/Photo Data.RData")