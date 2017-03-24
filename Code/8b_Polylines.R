# 1. Remaining data for polylines

# 2.a. Create categories/groupings for each time relevant measure changes

# Activity and country/blog (For colouring by activity and grouping by activity)
geodfcomprwg$activity_country.change <- 0  
geodfcomprwg$activity_country.change[(geodfcomprwg$activity!=
                                        lag(geodfcomprwg$activity,geodfcomprwg$activity[1]))
                                     |
                                       (geodfcomprwg$country!=
                                          lag(geodfcomprwg$country,geodfcomprwg$country[1]))
                                     |
                                       (geodfcomprwg$blog!=
                                          lag(geodfcomprwg$blog,geodfcomprwg$blog[1]))] <- 1
geodfcomprwg$activity_country.cat<-cumsum(geodfcomprwg$activity_country.change)+1

# Activity mainroute and country/blog (For colouring by activity and grouping by mainroute)
geodfcomprwg$activity_mainroute_country.change <- 0  
geodfcomprwg$activity_mainroute_country.change[(geodfcomprwg$activity!=
                                                  lag(geodfcomprwg$activity,geodfcomprwg$activity[1]))
                                               |
                                                 (geodfcomprwg$main_route!=
                                                    lag(geodfcomprwg$main_route,geodfcomprwg$main_route[1]))
                                               |
                                                 (geodfcomprwg$country!=
                                                    lag(geodfcomprwg$country,geodfcomprwg$country[1]))
                                               |
                                                 (geodfcomprwg$blog!=
                                                    lag(geodfcomprwg$blog,geodfcomprwg$blog[1]))] <- 1
geodfcomprwg$activity_mainroute_country.cat<-
  cumsum(geodfcomprwg$activity_mainroute_country.change)+1 

# 2.b. Vectors saying what each category represents. Length is number of categories

# Activity and country only
activity_country.activity<-
  activity_country.country<-
  activity_country.blog<-
  as.vector(rep(NA,max(geodfcomprwg$activity_country.cat)))
for (i in 1:max(geodfcomprwg$activity_country.cat)) {
  activity_country.activity[i]<-
    geodfcomprwg$activity[geodfcomprwg$activity_country.cat==i][1]
  activity_country.country[i]<-
    geodfcomprwg$country[geodfcomprwg$activity_country.cat==i][1]
  activity_country.blog[i]<-
    geodfcomprwg$blog[geodfcomprwg$activity_country.cat==i][1]
}
rm(i)

# Activity mainroute and country
activity_mainroute_country.activity<-
  activity_mainroute_country.mainroute<-
  activity_mainroute_country.country<-
  activity_mainroute_country.blog<-
  as.vector(rep(NA,max(geodfcomprwg$activity_mainroute_country.cat)))

for (i in 1:max(geodfcomprwg$activity_mainroute_country.cat)) {
  activity_mainroute_country.activity[i]<-
    geodfcomprwg$activity[geodfcomprwg$activity_mainroute_country.cat==i][1]
  activity_mainroute_country.mainroute[i]<-
    geodfcomprwg$main_route[geodfcomprwg$activity_mainroute_country.cat==i][1]
  activity_mainroute_country.country[i]<-
    geodfcomprwg$country[geodfcomprwg$activity_mainroute_country.cat==i][1]
  activity_mainroute_country.blog[i]<-
    geodfcomprwg$blog[geodfcomprwg$activity_mainroute_country.cat==i][1]
}
rm(i)

# 2.c. Vectors saying what grouping to apply in map (the bit with the tickboxes)

# Activity and country
group.activity_country<-activity_country.activity

# Activity mainroute and country
group.activity_mainroute_country <- as.vector(rep(NA,max(geodfcomprwg$activity_mainroute_country.cat)))
group.activity_mainroute_country[activity_mainroute_country.mainroute==0]<-"Side Trips"
group.activity_mainroute_country[activity_mainroute_country.mainroute==1]<-"Main Route"


# 2.d. Vectors saying what colour to apply in map

# Activity and country   
colours.activity_country<-as.vector(rep(NA,max(geodfcomprwg$activity_country.cat)))
colours.activity_country[activity_country.activity=="Biking"]<-"Green"
colours.activity_country[activity_country.activity=="Public Transport"]<-"Blue" 
colours.activity_country[activity_country.activity=="Hiking"]<-"Red"

# Activity mainroute and country
colours.activity_mainroute_country<-as.vector(rep(NA,max(geodfcomprwg$activity_mainroute_country.cat)))
colours.activity_mainroute_country[activity_mainroute_country.activity=="Biking"]<-"Green"
colours.activity_mainroute_country[activity_mainroute_country.activity=="Public Transport"]<-"Blue"
colours.activity_mainroute_country[activity_mainroute_country.activity=="Hiking"]<-"Red"

# 2.e. Remove unnecessary objects
rm(activity_mainroute_country.activity,
   activity_mainroute_country.mainroute,
   activity_mainroute_country.country,
   activity_mainroute_country.blog,
   activity_country.activity,
   activity_country.country,
   activity_country.blog)  
geodfcomprwg$activity_country.change <-
  geodfcomprwg$activity_mainroute_country.change <- NULL

