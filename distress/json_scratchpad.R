library(stringr)
library(dplyr)
library(readr)
library(jsonlite)


setwd("C:/Users/Stephen/Desktop/leaflet_js/distress")

states <- fromJSON("states.json")
head(states)
str(states)
states2 <- states$features
str(states2)
states3 <- states2$properties
str(states3)
head(states3)

# census shapefiles: https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html
# manual conversion of shapefiles to json: http://converter.mygeodata.eu/
# pre-converted census shapefiles to json, but wary to use since not sure how up to date?
# http://eric.clst.org/Stuff/USGeoJSON

setwd("C:/Users/Stephen/Desktop/R/EDA/distress/county_maps/mygeodata (2)")
options(encoding="UTF-8")
counties <- fromJSON("counties_2014.json")
list.files()
str(counties)
counties2 <- counties$features
str(counties2)
counties3 <- counties2$properties
str(counties3)


