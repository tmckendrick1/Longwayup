
# Assumes data is already loaded in

# Set main track colour
pathcolor <- "#F8971F"

# Load in Map e.g. Google Map
mapImageData <- get_map(location = c(lon = mean(geodfcomp$lon),
                                     lat = median(geodfcomp$lat)),
                        color = "color", 
                        source = "google",
                        maptype = "hybrid",
                        zoom = 10)

# Map
ggmap(mapImageData,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") + 
  geom_path(aes(x = lon, y = lat),
            data = geodf,
            colour = "black",
            size = 2) + #Bigger path in black for outline
  geom_path(aes(x = lon, y = lat),
            colour = pathcolor,
            data = geodfcomp,
            size = 1.4)
