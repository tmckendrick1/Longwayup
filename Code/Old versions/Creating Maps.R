# SPLITTING FILE

# NEED TO ONLY KEEP WHAT IS NECESSARY FOR MAP

# Mark cells where there is a change in activity
geodfcomprwg$activity <- paste(geodfcomprwg$activity)
geodfcomprwg$activity.lag <- append(paste(head(geodfcomprwg$activity,-1)),
                                    paste(geodfcomprwg$activity[1]),
                                    after=0)

geodfcomprwg$activity.change<-geodfcomprwg$activity!=geodfcomprwg$activity.lag

geodfcomprwg$activity.change[1]<-TRUE

# Create new id and get id's where activity changes
geodfcomprwg$id.total <- 1:nrow(geodfcomprwg)
change.id.vector<-geodfcomprwg$id.total[geodfcomprwg$activity.change==TRUE] 

# Number of datasets to be created, one for each block of data with the same activity
datasets.number <- length(change.id.vector)

# Vectors to be used in mapping
activity.code.vector<-as.vector(rep(
  NA,datasets.number))
activity.vector<-as.vector(rep(
  NA,datasets.number))
colours.vector<-as.vector(rep(
  NA,datasets.number))

# Create the split datasets
for (i in 1:datasets.number) {
  if (i<datasets.number) {
    assign(paste0("geodfcomprwg",i,sep=""),
           geodfcomprwg[(geodfcomprwg$id.total>=change.id.vector[i]&geodfcomprwg$id.total<change.id.vector[i+1]),])
  }
  if (i==datasets.number) {
    assign(paste0("geodfcomprwg",i,sep=""),
           geodfcomprwg[(geodfcomprwg$id.total>=change.id.vector[i]),])
  }
  
  # Generate activity code vector cells one at a time
  activity.code.vector[i]<-paste(get(paste0("geodfcomprwg",i,sep=""))$activity[1])
}

# Can do vectors en masse. Future idea = do this using a merge file
activity.vector[activity.code.vector=="C"]<-"Biking"
activity.vector[activity.code.vector=="T"]<-"Public Transport - main route" # Transport that replaced cycling
activity.vector[activity.code.vector=="N"]<-"Public transport - other" # Other transport e.g. to treks and back
activity.vector[activity.code.vector=="W"]<-"Hiking"

colours.vector[activity.code.vector=="C"]<-"Red"
colours.vector[activity.code.vector=="T"]<-"Blue" 
colours.vector[activity.code.vector=="N"]<-"Purple"
colours.vector[activity.code.vector=="W"]<-"Green"

