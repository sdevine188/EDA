library(ggplot2)
library(rgdal)
library(scales)
library(ggmap)
library(dplyr)
library(RCurl)
library(stringr)
library(datasets)
library(maps)
library(maptools)
library(dplyr)
library(RColorBrewer)
library(readr)


# http://www.r-bloggers.com/things-i-forget-reading-a-shapefile-in-r-with-readogr/

# tutorial at http://www.kevjohnson.org/making-maps-in-r/

# download shapefile for census tract in georgia in 2010
# go to the generalized "GENZ" folder for cartographic boundary files via the FTP 
# http://www2.census.gov/geo/tiger/
# or download via browser https://www.census.gov/geo/maps-data/data/tiger-cart-boundary.html

# other tiger products 
# https://www.census.gov/geo/maps-data/data/tiger.html

# note that census describes generalized cartographic boundary files as better for small areas, since they render faster than ungeneralized TIGER shapefiles

# might need gpclibPermit() to use fortify if you get error
# http://stackoverflow.com/questions/30790036/error-istruegpclibpermitstatus-is-not-true

#####################################


# load census tiger line shapefiles for states

# setwd to tiger line file
setwd("C:/Users/Stephen/Desktop/R/EDA/shapefile_distress_map/state_maps/tl_2016_us_state")

# read in tiger line file for 2016 state boundaries
# https://www.census.gov/geo/maps-data/data/tiger-line.html
# note that layer = file name for shapefiles
states_shape <- readOGR(dsn = ".", layer = "tl_2016_us_state")
names(states_shape)
gpclibPermit() # may need to install.packages("gpclib") first
states_shape <- fortify(states_shape, region="GEOID")
head(states_shape)

setwd("C:/Users/Stephen/Desktop/R/EDA/distress")
write_csv(states_shape, "states_shapefiles_dataframe.csv")


########################################################


# load county data

# setwd to tiger line file
setwd("C:/Users/Stephen/Desktop/R/EDA/shapefile_distress_map/county_maps/tl_2016_us_county")

# read in tiger line file for 2016 counties
# https://www.census.gov/geo/maps-data/data/tiger-line.html
# note that layer = file name for shapefiles
counties_shape <- readOGR(dsn = ".", layer = "tl_2016_us_county")
names(counties_shape)
gpclibPermit() # may need to install.packages("gpclib") first
counties_shape <- fortify(counties_shape, region="GEOID")
head(counties_shape)


#####################################


# merge data with counties_shape
setwd("C:/Users/Stephen/Desktop/R/EDA/distress")
counties <- read_csv("counties_20170201.csv")

# check for overlap
counties %>% filter(!(counties$fips_state_county %in% counties_shape$id)) %>% select(county_state, fips_state_county)

# merge
choropleth <- left_join(counties_shape, counties, by = c("id" = "fips_state_county"))
choropleth <- arrange(choropleth, order)

# inspect counties
length(unique(counties_shape$id))
length(unique(choropleth$id))
length(unique(counties$fips_state_county))

choropleth %>% filter(!(id %in% counties$fips_state_county)) %>% distinct(id) 


# filter choropleth shapefiles to just 3142 in counties/pc_inc/bls file
choropleth <- choropleth %>% filter(id %in% unique(counties$fips_state_county))

# check for overlap
counties %>% filter(!(unique(counties$fips_state_county) %in% unique(choropleth$id))) %>% 
        select(county_state, fips_state_county)
length(unique(choropleth$id))

write_csv(choropleth, "us_counties_shapefiles_dataframe.csv")

