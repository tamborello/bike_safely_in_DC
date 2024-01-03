# Author: Frank Tamborello
# CC-BY-SA 2015 Frank Tamborello
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of Creative Commons Attribute-ShareAlike 4.0 International License: http://creativecommons.org/licenses/by-sa/4.0/
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
#
# Description: This is an exploration of District of Columbia's Vision Zero traffic safety data, especially as it pertains to bicycling.


library(jsonlite)
library(httr)

# DC's "dataset attributes" are the variables of $features$properties dataframes

signed_bike_routes <- fromJSON("http://opendata.dc.gov/datasets/0a0ff97967fb413b91d5cf444ed82fce_6.geojson")

# Vision Zero
# http://opendata.dc.gov/datasets/3f28bc3ad77f49079efee0ac05d8464c_0
vision_zero <- fromJSON("http://opendata.dc.gov/datasets/3f28bc3ad77f49079efee0ac05d8464c_0.geojson")
save(vision_zero, file="~/Bike\ Safely/vision_zero.raw.dat")
load(file="~/Bike\ Safely/vision_zero.raw.dat", verbose=T)

# Bike Lanes, a subset of signed bike routes?
# http://opendata.dc.gov/datasets/5b0d63150e2c4c47b5b21db73a4fd928_3
bike_lanes <- fromJSON("http://opendata.dc.gov/datasets/5b0d63150e2c4c47b5b21db73a4fd928_3.geojson")


geometry <- signed_bike_routes$features$geometry
coordinates <- geometry[[2]]
locations <- data.frame()
locations <- rbind(locations, cbind(coordinates[[1]], rep(1, dim(coordinates[[1]])[1])))
locations <- rbind(locations, cbind(coordinates[[2]], rep(2, dim(coordinates[[2]])[1])))


for (i in 1:length(coordinates)) {
	locations <- rbind(locations, cbind(coordinates[[i]], rep(i, dim(coordinates[[i]])[1])))
	}
names(locations) <- c("longitude", "lattitude", "location.id")
write.table(locations, file="~/Bike\ Safely/signed_bike_routes.locations.txt", sep="\t")


g.lengths <- length(geometry$coordinates[[1:1024]])
properties <- signed_bike_routes$features$properties
write.table(properties, file="~/Bike\ Safely/signed_bike_routes.properties.txt", sep="\t")

bike.routes <- read.table(file="~/Bike\ Safely/signed_bike_routes.properties.txt", sep="\t", header=T)


filteredfeatures <- subset(signed_bike_routes$features, signed_bike_routes$features$properties$REGISTERED == "L")
str(filteredfeatures)


filteredJSON <- toJSON(filteredfeatures, pretty = TRUE)
filteredJSON
sort(unique(signed_bike_routes$features$properties$REGISTERED))
unique(signed_bike_routes$features$properties$FACILITYID)


# What factors might I use to build a multiple regression model?
# lattitude, longitude, location.id (or should that be OBJECTID?), STREET, 
# …bike_crashes, any crashes, fatalities, etc. 




# How can I cbind vision_zero & signed_bike_routes matching cases?
# ObjectID? STREETSEGID?
vzero <- vision_zero$features$properties

geometry <- vision_zero$features$geometry
coordinates <- geometry[[2]]
locations <- data.frame()

# surely there's a vectorized way to do this!
for (i in 1:length(coordinates)) {
	locations <- rbind(locations, cbind(coordinates[[i]][1], coordinates[[i]][2], i))
	}
names(locations) <- c("longitude", "lattitude", "location.id")
write.table(locations, file="~/Bike\ Safely/vision_zero.locations.txt", sep="\t")









# First plot all data!
plot(lattitude ~ longitude, data=locations)


# How can I norm the vision zero reports by bike traffic density, take them as an incident per cyclist rate? That seems sensible, but where do I get cyclist density?




# Bicycle Count Locations
# http://opendata.dc.gov/datasets/13ced1ecaa9c4354bf774e27fe3c00ab_66
bicycle_count_locations <- fromJSON("http://opendata.dc.gov/datasets/13ced1ecaa9c4354bf774e27fe3c00ab_66.geojson")
save(bicycle_count_locations, file="~/Bike\ Safely/bicycle_count_locations.raw.dat")
load(file="~/Bike\ Safely/bicycle_count_locations.raw.dat", verbose=T)
# No! No density and no counts. What is this allegedly counting? Did I even download all of it? The server seemed to hiccup. Is this only the locations in which bicycles were counted? Where are the bicycle counts?


# Traffic Monitoring Stations
tfc.mon.sta <- fromJSON("http://opendata.dc.gov/datasets/a87c1b9a71e143a4914e3c384bda2d3a_92.geojson")
# No counts? What is "properties.MEASURE?"


# Where can I get, not citizen complaints, but locations of officially-reported incidents like crashes? DC Open Data doesn't seem to provide that! It doesn't exist, according to WAMU (2015.08.10): D.C. Lacks The Crash Data It Needs To Pursue 'Vision Zero ...



bike_model <- glm(safety ~ lattitude * longitude * location.id, STREET, data=bike_data, family="binomial")
summary(bike_model)



# These adverse events are rare. What can other hazard identification & assessment methods tell us? Would it help to aggregate bike safety data from many places?


