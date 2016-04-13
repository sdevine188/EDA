library(ggmap)
library(stringr)
library(dplyr)
library(lubridate)
library(taRifx.geo)
library(readr)

# set working directory where master_data file is saved
setwd("G:/PNP/Performance Measurement/master_data")

# read in latest master_data
master_data_filename <- list.files()[str_detect(list.files(), "master_data_20")]
master_data <- read.csv(master_data_filename, stringsAsFactors = FALSE, colClasses = c("Control." = "character",
                                                                                       "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                                                                                       "Initiatives" = "character"))
md <- master_data

# read in archived shiny data with lat/long info
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")

filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny_app <- read_csv(filename, col_types = list("Control." = col_character()))
shiny_app2 <- select(shiny_app, Control., app_address, app_lat, app_lon)

## merge existing shiny data with latest master_data 

# run this if you only want to include approved grants 
# records <- filter(md, Status == "Approved")

records <- md
records <- left_join(records, shiny_app2, by = "Control.")

# only needed on 20160209 when changing variable name from address to app_address
# names(records)[which(names(records) == "address")] <- "app_address"

need_app_address <- filter(records, is.na(app_address))

# only needed when cleaning records previously mapped with just bing, but not google
# need_app_address_google <- filter(records, !is.na(app_address) & is.na(app_lat))

# compile address field to use in geocoding
need_app_address$app_address <- str_c(need_app_address$Appl.Street.Addr.1, need_app_address$Appl.City.Name, need_app_address$Appl.State.Abbr, need_app_address$Appl..Zip, sep = ", ")
need_app_address$app_address <- as.character(need_app_address$app_address)

# geocode addresses
options(BingMapsKey = "QFG8iBCiInAj6ER1ubuD~I5piVwPPZghOvhCJzBP-1g~AicfV1u7mkoKlY53KfatxR67u-NHXCfu1iEA8dBryA8vlUJy3yu3y0u2cZLWf-D4")

for(i in 1:nrow(need_app_address)){
        coordinates <- tryCatch(geocode(need_app_address$app_address[i], service = "bing"), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- need_app_address$app_address[i]
                                                print(address)
                                                zip <- need_app_address$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- need_app_address$app_address[i]
                                                print(address)
                                                zip <- need_app_address$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                address <- need_app_address$app_address[i]
                print(address)
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name, ", ", 
                                        need_app_address_google$Appl.State.Abbr[i], ", ", 
                                        need_app_address_google$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_address$app_lon[i] <- coordinates[2]
                need_app_address$app_lat[i] <- coordinates[1]
        }
        if(is.na(coordinates)){
                print("error: coordinates are NA")
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name, ", ", 
                                        need_app_address_google$Appl.State.Abbr[i], ", ", 
                                        need_app_address_google$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_address$app_lon[i] <- coordinates[2]
                need_app_address$app_lat[i] <- coordinates[1]
        }
        need_app_address$app_lon[i] <- coordinates[2]
        need_app_address$app_lat[i] <- coordinates[1]
}

# see how many apps bing could not map, even when trying just the zip code
# usually these records have too many or too few zip code digits, but some are OCONUS and bing just can't map them
# try running them through goolgle just in case
length(which(is.na(need_app_address$app_address)))
length(which(is.na(need_app_address$app_lat)))
length(which(is.na(need_app_address$app_lon)))
bing_failed <- need_app_address[which(is.na(need_app_address$app_lat)), ]
dim(bing_failed)
need_app_address_google <- bing_failed
# only need this if cleaning records previously mapped in just bing, not google
# need_app_address_google <- rbind(need_app_address_google, bing_failed)

for(i in 1:nrow(need_app_address_google)){
        coordinates <- tryCatch(geocode(need_app_address_google$app_address[i]), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- need_app_address_google$app_address[i]
                                                zip <- need_app_address_google$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- need_app_address_google$app_address[i]
                                                zip <- need_app_address_google$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name, ", ", 
                                        need_app_address_google$Appl.State.Abbr[i], ", ", 
                                        need_app_address_google$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip)
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_address_google$app_lon[i] <- coordinates[2]
                need_app_address_google$app_lat[i] <- coordinates[1]
        }
        if(is.na(coordinates)){
                print("error: coordinates are NA")
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name, ", ", 
                                need_app_address_google$Appl.State.Abbr[i], ", ", 
                                need_app_address_google$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip)
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_address_google$app_lon[i] <- coordinates[2]
                need_app_address_google$app_lat[i] <- coordinates[1]
        }
        need_app_address_google$app_lon[i] <- coordinates[2]
        need_app_address_google$app_lat[i] <- coordinates[1]
}

# see how many of the NA's google was not able to map
length(which(is.na(need_app_address_google$app_lat)))

# re-combine the existing master_data with the newly mapped applications
# only need this code chunk below if need_app_address_google is > 1
# need_app_address_minus <- filter(need_app_address, !is.na(need_app_address$app_lat))
# need_app_address_combined <- rbind(need_app_address_minus, need_app_address_google)
# records_minus <- filter(records, !is.na(app_address)) 
# shiny_app_data <- rbind(records_minus, need_app_address_combined)

# re-combine the existing master_data with the newly mapped applications
# use this code instead of chunk above if need_app_address_google = 0
records_minus <- filter(records, !is.na(app_address)) 
shiny_app_data <- rbind(records_minus, need_app_address)

# check to make sure all shiny_app_data records have app_address, app_lat, app_lon
length(which(is.na(shiny_app_data$app_address)))
length(which(is.na(shiny_app_data$app_lat)))
length(which(is.na(shiny_app_data$app_lon)))
# inspect those still missing app_lat to ensure they are hopeless cases
filter(shiny_app_data, is.na(app_lat)) %>% select(app_address, app_lat, app_lon)

# jitter duplicate coordinates to avoid overlapping on map
app_coord <- sapply(1:nrow(shiny_app_data), function(x) str_c(shiny_app_data$app_lat[x], " ", 
                                                              shiny_app_data$app_lon[x]))
dup_logical <- duplicated(app_coord)
dup_index <- which(dup_logical == TRUE)
shiny_jitter <- shiny_app_data
shiny_jitter$app_coord <- app_coord
dups <- shiny_jitter[dup_index, ]
unique_dups <- unique(dups$app_coord)
for(i in 1:length(unique_dups)){
        print(str_c("unique_dups is ", i))
        dup_i <- unique_dups[i]
        row_i <- which(shiny_jitter$app_coord == dup_i)
        shiny_jitter_dup_i <- shiny_jitter[row_i, ]
        direction_counter <- 1
        for(x in 1:nrow(shiny_jitter_dup_i)){
                print(str_c("shiny_jitter_dup_i is ", x))
                print(str_c("direction_counter is ", direction_counter))
                loop_multiplier <- ceiling(x / 4)
                print(str_c("loop_multiplier is ", loop_multiplier))
                if(direction_counter == 1){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$app_lat[shiny_jitter_row] <- shiny_jitter_dup_i$app_lat[x] + .01*loop_multiplier
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("app_lat + ", .01*loop_multiplier))
                        } 
                if(direction_counter == 2){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$app_lat[shiny_jitter_row] <- shiny_jitter_dup_i$app_lat[x] - .01*loop_multiplier
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("app_lat - ", .01*loop_multiplier))
                        } 
                if(direction_counter == 3){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$app_lon[shiny_jitter_row] <- shiny_jitter_dup_i$app_lon[x] + .01*loop_multiplier
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("app_lon + ", .01*loop_multiplier))
                        }
                if(direction_counter == 4){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$app_lon[shiny_jitter_row] <- shiny_jitter_dup_i$app_lon[x] - .01*loop_multiplier
                        direction_counter <- 1
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("app_lon - ", .01*loop_multiplier))
                        } else {direction_counter <- direction_counter + 1}
        }
}

# check jitter worked ok
dups2 <- shiny_jitter[dup_index, ]
head(dups$app_lat)
head(dups2$app_lat)

# save over shiny_app_data with jitter data
shiny_app_data <- shiny_jitter

# if posting publically, clean shiny_data_app to remove PII
# need to confirm the fields match eda press releases and usaspending.gov

# write shiny data to file
setwd("G:/PNP/Performance Measurement/rshinyapp")
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
shiny_filename <- str_c("shiny_app_data_", date2, ".csv")
write_csv(shiny_app_data, path = shiny_filename)

