# 1. Create remaining necessary objects
geodfcomprwg$id.total <- 1:nrow(geodfcomprwg)

country_id_start<-
  geodfcomprwg %>%
  group_by(country) %>%
  summarise(id_start=first(id.total))

countrylevels <- sort(country_id_start$country)[order(country_id_start$id_start)]

rm(country_id_start)

source("Code/8a_Photos.R")
source("Code/8b_Polylines.R")
source("Code/8c_Popups.R")
source("Code/8d_Blogs.R")
source("Code/8e_Charts.R")

rm(datacondition_across,
   datacondition_activity,
   datacondition_blog_cross,
   datacondition_comparison,
   datacondition_countries,
   datacondition_countries_blogs,
   datacondition_blogset_cross,
   variablescondition,
   variables_chart,
   blog, country, variable, across, activity, comparison,
   plot, plot_highlighted, plot_unhighlighted)

