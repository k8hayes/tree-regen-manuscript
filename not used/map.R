
library(ggmap)
library(ggplot2)
library(maps)
library(mapdata)

points <- read.csv("data/site_topography.csv")

sbbox <- make_bbox(lat = LAT,lon = LONG, data = points)

sbbox[4] <- 66
sbbox[2] <- 65
sbbox[1] <- -149.4
sbbox[3] <- -144.5
sbbox_total <- sbbox
# First get the map. By default it gets it from Google.  I want it to be a satellite map
sq_map <- get_map(location = sbbox, maptype = "satellite", source = "google")

# converting bounding box to center/zoom specification. (experimental)
#> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.75309,-119.751995&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false
ggmap(sq_map) + geom_point(data = points, mapping = aes(x = points$LONG, y = points$LAT), color = "red")


dalton <- points[points$SITE == "DALTON",]
steese <- points[points$SITE == "STEESE",]
  
sq_map3 <- get_map(location = sbbox,  maptype = "satellite", source = "google", zoom = 13)

ggmap(sq_map) + 
  geom_point(data = dalton, 
             mapping = aes(x = dalton$LONG, y = dalton$LAT),color = "#d95f0e", size = 2) +
  geom_point(data = steese, 
             mapping = aes(x = steese$LONG, y = steese$LAT), color = "#fec44f", size = 2) +
  labs(x = "Longitude", y = "Latitude")


##############
# dalton
dalton <- points[points$SITE == "DALTON",]
sbbox <- make_bbox(lat = LAT,lon = LONG, data = dalton)
max(dalton$LAT)
min(dalton$LAT)
max(dalton$LONG)
min(dalton$LONG)
sbbox[4] <- 65.8
sbbox[2] <- 65.5
sbbox[1] <- -149.25
sbbox[3] <- -149.94
# First get the map. By default it gets it from Google.  I want it to be a satellite map
sq_map_Dalton <- get_map(location = sbbox, maptype = "satellite", source = "google")

# converting bounding box to center/zoom specification. (experimental)
#> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.75309,-119.751995&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false
ggmap(sq_map_Dalton) + geom_point(data = dalton, mapping = aes(x = dalton$LONG, y = dalton$LAT), color = "red")

# splitting up treatment
unburned <- steese[steese$TREAT == 0,]
burn1 <- steese[steese$TREAT == 1,]
burn2 <- steese[steese$TREAT == 2,]
burn3 <- steese[steese$TREAT == 3,]

ggmap(sq_map_steese) + 
  geom_point(data = unburned, mapping = aes(x = unburned$LONG, y = unburned$LAT), color = "white") + 
  geom_point(data = burn1, mapping = aes(x = burn1$LONG, y = burn1$LAT), color = "yellow") + 
  geom_point(data = burn2, mapping = aes(x = burn2$LONG, y = burn2$LAT), color = "orange") +
  geom_point(data = burn3, mapping = aes(x = burn3$LONG, y = burn3$LAT), color = "orange")


############
# trying this with steese
steese <- points[points$SITE == "STEESE",]
sbbox <- make_bbox(lat = LAT,lon = LONG, data = steese)

sbbox[4] <- 65.6
sbbox[2] <- 65.53
sbbox[1] <- -145.1
sbbox[3] <- -144.9
# First get the map. By default it gets it from Google.  I want it to be a satellite map
sq_map_steese <- get_map(location = sbbox, maptype = "satellite", source = "google",zoom = 18)

# converting bounding box to center/zoom specification. (experimental)
#> Map from URL : http://maps.googleapis.com/maps/api/staticmap?center=34.75309,-119.751995&zoom=16&size=640x640&scale=2&maptype=satellite&language=en-EN&sensor=false
ggmap(sq_map_steese) + geom_point(data = steese, mapping = aes(x = steese$LONG, y = steese$LAT), color = "red")

# splitting up treatment
unburned <- steese[steese$TREAT == 0,]
burn1 <- steese[steese$TREAT == 1,]
burn2 <- steese[steese$TREAT == 2,]
burn3 <- steese[steese$TREAT == 3,]

stees_plot <- ggmap(sq_map_steese) + 
  geom_point(data = unburned, mapping = aes(x = unburned$LONG, y = unburned$LAT), color = "white") + 
  geom_point(data = burn1, mapping = aes(x = burn1$LONG, y = burn1$LAT), color = "yellow") + 
  geom_point(data = burn2, mapping = aes(x = burn2$LONG, y = burn2$LAT), color = "orange") +
  geom_point(data = burn3, mapping = aes(x = burn3$LONG, y = burn3$LAT), color = "orange")

# or, we can do this a different way
test_means <- sapply(steese[10:9], mean)
sq_plot_steese2 <- get_map(location = sbbox, maptype = "satellite", source = "google", zoom = 15)


install.packages("usmap")
library(usmap)
states_df <- usmap::us_map()
ak_df <- subset(states_df, full == "Alaska")
ak_base <- ggplot(data = ak_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "white") 
ak_base + theme_nothing() 
