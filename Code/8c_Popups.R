# 3. Remaining data for Popups

# 3.a. Accommodation

# Group
lookup_table_stats_and_records$accommodation_group <- NA


lookup_table_stats_and_records$accommodation_group[
  lookup_table_stats_and_records$accommodation_type=="Hotel" |
    lookup_table_stats_and_records$accommodation_type=="Hostel" |
    lookup_table_stats_and_records$accommodation_type=="Hospedaje" |
    lookup_table_stats_and_records$accommodation_type=="Basic Accommodation" |
    lookup_table_stats_and_records$accommodation_type=="Cabana" |
    lookup_table_stats_and_records$accommodation_type=="Refugio"] <-
  "Hotel"

lookup_table_stats_and_records$accommodation_group[
  lookup_table_stats_and_records$accommodation_type=="Warmshowers"] <- 
  "Warmshowers"

lookup_table_stats_and_records$accommodation_group[
  lookup_table_stats_and_records$accommodation_type=="Campsite" |
    lookup_table_stats_and_records$accommodation_type=="Wild Camping"] <-
  "Camping"

lookup_table_stats_and_records$accommodation_group[
  lookup_table_stats_and_records$accommodation_type=="Overnight bus"] <- 
  "Bus"

lookup_table_stats_and_records$accommodation_group[
  lookup_table_stats_and_records$accommodation_type=="Apartment"] <- 
  "Apartment"

lookup_table_stats_and_records$accommodation_group[
  lookup_table_stats_and_records$accommodation_type=="Other"] <- 
  "Other"

# Select icons
lookup_table_stats_and_records$accommodation_icon <- NA

lookup_table_stats_and_records$accommodation_icon[
  lookup_table_stats_and_records$accommodation_group=="Hotel"] <- 
  "https://cdn3.iconfinder.com/data/icons/glypho-travel/64/bed-accomodation-hotel-motel-512.png"

lookup_table_stats_and_records$accommodation_icon[
  lookup_table_stats_and_records$accommodation_group=="Warmshowers"] <- 
  "https://pbs.twimg.com/profile_images/2707735661/6acfde74054ca7fd083031a072da3908_400x400.png"

lookup_table_stats_and_records$accommodation_icon[
  lookup_table_stats_and_records$accommodation_group=="Camping"] <- 
  "http://crec.unl.edu/images/Icons/OA_Tent_red.png"

lookup_table_stats_and_records$accommodation_icon[
  lookup_table_stats_and_records$accommodation_group=="Bus"] <- 
  "https://image.flaticon.com/icons/png/128/0/308.png"

lookup_table_stats_and_records$accommodation_icon[
  lookup_table_stats_and_records$accommodation_group=="Other"] <- 
  "https://d30y9cdsu7xlg0.cloudfront.net/png/6393-200.png"

lookup_table_stats_and_records$accommodation_icon[
  lookup_table_stats_and_records$accommodation_group=="Apartment"] <- 
  "http://haseebq.com/wordpress/wp-content/uploads/2016/04/airbnb.png"

# Make unknown names blank
lookup_table_stats_and_records$accommodation_name[
  lookup_table_stats_and_records$accommodation_name=="[UNKNOWN]" |
    lookup_table_stats_and_records$accommodation_name=="[NA]"] <- ""


# 3.b. Records

# Select icons
lookup_table_stats_and_records$record_icon <- NA

lookup_table_stats_and_records$record_icon[lookup_table_stats_and_records$record_type=="Compass"] <-
  "http://www.freeiconspng.com/uploads/compass-icon-24.png"
lookup_table_stats_and_records$record_icon[lookup_table_stats_and_records$record_type=="Altitude"] <-
  "http://crec.unl.edu/images/Icons/OA%20Mountain%201%20Red.png"
lookup_table_stats_and_records$record_icon[lookup_table_stats_and_records$record_type=="Temperature"] <-
  "https://cdn2.iconfinder.com/data/icons/lovely-weather-icons/32/Thermometer-100-512.png"
lookup_table_stats_and_records$record_icon[lookup_table_stats_and_records$record_type=="Speed"] <-
  "https://cdn1.iconfinder.com/data/icons/miscellaneous-4/32/dashboard-high-512.png"

