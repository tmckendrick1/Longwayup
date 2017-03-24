
data.toclean$datestr <- sprintf("%02d/%02d/%04d",
                         day(data.toclean$datetemp),
                         month(data.toclean$datetemp),
                         year(data.toclean$datetemp))
data.toclean$timestr<-sprintf("%02d:%02d:%02d",
                       hour(data.toclean$datetemp),
                       minute(data.toclean$datetemp),
                       second(data.toclean$datetemp))
data.toclean$date<-dmy(paste(data.toclean$datestr))
data.toclean$datetime<-dmy_hms(paste(data.toclean$datestr,data.toclean$timestr))

data.toclean$datetemp<-NULL

# Haversine
data.toclean <- data.toclean[order(data.toclean$date,data.toclean$order,data.toclean$datetime,data.toclean$id),]

data.toclean$latradians <- data.toclean$lat*pi/180
data.toclean$lonradians <- data.toclean$lon*pi/180

data.toclean$latradiansseq <- append(tail(data.toclean$latradians,-1),
                                  0,
                                  after=length(tail(data.toclean$latradians,-1)))

data.toclean$lonradiansseq <- append(tail(data.toclean$lonradians,-1),
                                  0,
                                  after=length(tail(data.toclean$lonradians,-1)))

data.toclean$halflatradiansdiff <- (data.toclean$latradiansseq-data.toclean$latradians)/2
data.toclean$halflonradiansdiff <- (data.toclean$lonradiansseq-data.toclean$lonradians)/2

data.toclean$distdiff <- 2*6378137*asin(((sin(data.toclean$halflatradiansdiff))^2 
                                      + (cos(data.toclean$latradiansseq)*
                                           cos(data.toclean$latradians)*
                                           ((sin(data.toclean$halflonradiansdiff))^2)))^0.5)

data.toclean$distdiff<-append(head(data.toclean$distdiff,-1),
                           0,
                           after=0)

data.toclean$distdiff[length(data.toclean$distdiff)]<-0

data.toclean$latradians<-NULL
data.toclean$latradiansseq<-NULL
data.toclean$halflatradiansdiff<-NULL
data.toclean$lonradians<-NULL
data.toclean$lonradiansseq<-NULL
data.toclean$halflonradiansdiff<-NULL

data.toclean$secondsdiff <- append(tail(seconds(data.toclean$datetime),-1)
                                -head(seconds(data.toclean$datetime),-1),
                                0,
                                after=0)
data.toclean$speed.m.s <- data.toclean$distdiff/data.toclean$secondsdiff
data.toclean$secondsdiff<-NULL