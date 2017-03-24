

### To do:
  ### Make map a bit bigger in app
  ### Make chart with nights in different accommodations
  ### Put numbers on country and withtin-country blog charts
  ### Highest distance and elegain days in map - as separate polylines with different colours and markers on both sides
  ### Tables with main stats
  ### Elevation corrections - start with visual inspection!
  ### Write route notes for all blogs
  ### Rio blog: hiking only has us to the top of corcovado, app thinks we slept there
  ### Bring comments within each file out to the master so I can see what's left

# A. Prepare environment
  
  rm(list=ls())
  setwd("C:/Users/pss3c/Desktop/Longwayup/R Files/")
  
  options(scipen = 999)
  options(geonamesUsername="tmckendrick1") 
  
    source("Code/1_Load_Packages.R")
    source("Code/2_Load_Functions.R")

# B. Load in, clean and merge data
  
    source("Code/3_Import_Data.R")
      
      save.image("R Saved Data/Raw Data Latest.RData")

  # B.1. Fill in RidewithGPS data, where applicable
  
  max.break.distance<-1000 # Maximum distance allowed between points before possible RidewithGPS data insertion
  max.rwg.match.distance<-500 # Maximum distance RidewithGPS data is allowed to be before it can be inserted
  
    source("Code/4_Fill_In_RWG_Data.R")
    
      save.image("R Saved Data/Data Post RWG Merge Latest.RData")
  
  # B.2. Other information, including country, blog and accommodation
      
    source("Code/5_Import_Other_Data.R")
      
      save.image("R Saved Data/Final Dataset Latest.RData")
      
# C. Analysis

    source("Code/6_Further_Calculations.R")
    
      save.image("R Saved Data/Final Dataset With Lookup Tables Latest.RData")
    
# D. Preparing data for App
  
  # Reducing file size
      
  ### STILL COULD DO WITH BEING CUT DOWN - TRY TO DO THE FOLLOWING:
    # Make todelete as normal based on distance
    # Bring in earlier rule that activity changes etc. can't be deleted
    # Collapse todelete down to a vector separated by commas
    # Find and replace "1,1," with "1,0,"
    # String split it back to a vector
    # Aggregate as usual
  dist_cutoff <- 0.04   # Distance difference threshold (in km) 
                        # above which a dataset cannot be deleted in the following code
  # max_to_delete_allowed <- 10 # Time-saving tool allowing a small tail of observations to remain
  
    source("Code/7_Reducing_File_Size.R")
  
      save.image("R Saved Data/Reduced Size Dataset Latest.RData")
  
  # Finalising markers, popups, charts and blogs
    source("Code/8_Preparation_For_App.R")
      
      save.image("Longwayup/Map App Dataset Latest.RData")
      
# E. Save files and App
  
  # Saving files locally and to Github repository
      source("Code/9_Save_Map_And_Elevation_Profile.R")
      
  # Uploading app to Shiny
    # library(rsconnect)
    # rsconnect::deployApp("C:/Users/pss3c/Desktop/Longwayup/R Files/App", appName="Longwayup")