# Would be nice to be able
# to combine FIT and GPX files into a single data frame. Main problem will be with date
# where it isn't recorded in e.g. Strava file

setwd("C:/Users/pss3c/Desktop/Route/R Files/")

rm(list=ls())

library(XML)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gdata)
library(fitdc)

# FIT data cleaning
source ("Reading FIT Files in R.R")

# Common data cleaning
source("Code/Common data cleaning.R")

# Mapping
source("Code/Mapping.R")

# Elevation profile
source("Code/Elevation Profiles.R")