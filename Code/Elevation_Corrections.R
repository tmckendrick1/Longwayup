
# Define variables needed and create a blank dataframe
  variables_needed <- c("lat","lon","ele","activity","date")
  ele_test_data = data.frame(matrix(vector(),
                         0,
                         length(variables_needed),
                         dimnames=list(c(), variables_needed)
                         )
                  )
  geodfcomprwg$cumdist_temp<-0
  
# Sample size per strata
  sample_size <- 1
# Every X kimoleters
  kilometer_increment <- 10
  
  ### Should do a stratified sample, ensure we have X observations from each day
  ### Need to have a filetype variable in here so we know only to look at FIT files
  
  # Strata:
    condition_permanent <- geodfcomprwg$activity=="Biking" & geodfcomprwg$filetype=="FIT"
    
    for (i in 1:length(unique(geodfcomprwg$date[condition_permanent]))) {
      
      # Condition to return only FIT biking files on that date
      condition_temporary_date <- 
        condition_permanent &
        geodfcomprwg$date==unique(geodfcomprwg$date[condition_permanent])[i]
      
      # Create temporary cumdist variable to report cumulative distance
      geodfcomprwg$cumdist_temp[condition_temporary_date] <- 
        cumsum(geodfcomprwg$distdiff[condition_temporary_date])
      
      # Return total distance on that date in km - rounded up to nearest 10k
      meter_increments<-
        1000*seq(kilometer_increment,
                 max(kilometer_increment,
                     (sum(geodfcomprwg$distdiff[condition_temporary_date])/1000/kilometer_increment)*
                       kilometer_increment),
                 by=kilometer_increment)
      
      for (j in meter_increments) {
        condition_total <- 
          condition_temporary_date & 
          geodfcomprwg$cumdist_temp<j &
          geodfcomprwg$cumdist_temp>=j-kilometer_increment*1000
        
        # This is just the sample of this stratum
        ### Need to create a blank ele test data dataframe up front
        ele_test_data<-
          rbind(ele_test_data,
                geodfcomprwg[sample(geodfcomprwg$id.total[condition_total],sample_size),
                             variables_needed]
          )
      }
    }
  

  ele_test_data$elelookup <- NA
  
# Return elevation for each observation 
  for (i in 1:nrow(ele_test_data)) {
    ele_test_data$elelookup[i] <- GNsrtm3(ele_test_data$lat[i],ele_test_data$lon[i])[[1]]
  }
  
# Check the percentage difference and absolute difference in elevation
  ele_test_data$elediffpc = abs((ele_test_data$elelookup/ele_test_data$ele-1))*100
  ele_test_data$elediffabs = abs(ele_test_data$elelookup-ele_test_data$ele)
  
# Set a rule - e.g. report if difference is above 10% and 50m, or 100m
  ele_test_data$inspect <- as.numeric((ele_test_data$elediffpc>10 & ele_test_data$elediffabs>100) |
                                         ele_test_data$elediffabs>100)

# Return unique dates to inspect further
  dates_to_inspect <- unique(ele_test_data$date[ele_test_data$inspect==1])