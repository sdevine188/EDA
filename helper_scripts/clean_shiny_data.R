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
master_data <- read_csv(master_data_filename)
md <- master_data

# read in archived shiny data with lat/long info
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")

shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny_app_data <- read_csv(shiny_data_filename)
shiny_app_data <- select(shiny_app_data, Control., app_address, app_lat, app_lon)

## merge existing shiny data with latest master_data 

# run this if you only want to include approved grants 
# records <- filter(md, Status == "Approved")

records <- md
records <- left_join(records, shiny_app_data, by = "Control.")

# only needed on 20160209 when changing variable name from address to app_address
# names(records)[which(names(records) == "address")] <- "app_address"

need_app_address <- filter(records, is.na(app_address))

# only needed when cleaning records previously mapped with just bing, but not google
# need_app_address_google <- filter(records, !is.na(app_address) & is.na(app_lat))

# compile address field to use in geocoding
need_app_address$app_address <- str_c(need_app_address$Appl.Street.Addr.1, need_app_address$Appl.City.Name, need_app_address$Appl.State.Abbr, need_app_address$Appl..Zip, sep = ", ")

# remove Multi City
for(i in 1:nrow(need_app_address)) {
        if(need_app_address$Proj.City.Name[i] == "Multi City") {
                need_app_address$app_address[i] <- str_c(need_app_address$Proj.ST.Abbr[i], 
                                                           need_app_address$Proj.ZIP[i], sep = ", ")
        }     
}
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
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name[i], ", ", 
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
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name[i], ", ", 
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
# try running them through google just in case
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
                                                geocode(zip)
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- need_app_address_google$app_address[i]
                                                zip <- need_app_address_google$Appl..Zip[i]
                                                geocode(zip)
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name[i], ", ", 
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
                city_state_zip <- str_c(need_app_address_google$Appl.City.Name[i], ", ", 
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
app_coord <- sapply(1:nrow(shiny_jitter), function(x) str_c(shiny_jitter$app_lat[x], " ", 
                                                              shiny_jitter$app_lon[x]))
shiny_jitter$app_coord <- app_coord
dup_logical <- duplicated(app_coord)
sum(dup_logical)
dup_index <- which(dup_logical == TRUE)
dups <- shiny_jitter[dup_index, ]
select(dups, Appl.Short.Name, app_address, app_lat, app_lon) %>% arrange(app_address)

# save over shiny_app_data with jitter data
shiny_app_data <- shiny_jitter

# if posting publically, clean shiny_data_app to remove PII
# need to confirm the fields match eda press releases and usaspending.gov

# clean known special character issues that break shiny app map
espanola_row <- which(shiny_app_data$Control. == 76796)
shiny_app_data$Appl.Short.Name[espanola_row] <- "Espanola Valley FA Ctr"
shiny_app_data$Full.Applicant.Name[espanola_row] <- "Espanola Valley Fiber Arts Center"

# correct any known geocode errors - some are bing map errors, others are ugly addresses
setwd("G:/PNP/Performance Measurement/rshinyapp/clean_shiny_data")
recode <- read_csv("re-geocode_errors.csv")

# loop through list re-geocoding
for(i in 1:nrow(recode)) {
        error_address <- recode$error_address[i]
        error_row <- which(shiny_app_data$app_address == error_address)
        shiny_app_data$app_lat[error_row] <- recode$app_lat[i]
        shiny_app_data$app_lon[error_row] <- recode$app_lon[i]
}

# write shiny data to file
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
shiny_filename <- str_c("shiny_app_data_", date2, ".csv")
write_csv(shiny_app_data, path = shiny_filename)














# geo-code for proj_address
need_proj_address <- filter(records, is.na(proj_address))

# only needed when cleaning records previously mapped with just bing, but not google
# need_proj_address_google <- filter(records, !is.na(proj_address) & is.na(app_lat))

# compile address field to use in geocoding
need_proj_address$proj_address <- str_c(need_proj_address$Proj.City.Name, need_proj_address$Proj.ST.Abbr, 
                                        need_proj_address$Proj.ZIP, sep = ", ")
                                      
# remove Multi City
for(i in 1:nrow(need_proj_address)) {
        if(need_proj_address$Proj.City.Name[i] == "Multi City") {
                need_proj_address$proj_address[i] <- str_c(need_proj_address$Proj.ST.Abbr[i], need_proj_address$Proj.ZIP[i], 
                                              sep = ", ")
        }     
}

need_proj_address$proj_address <- as.character(need_proj_address$proj_address)

# initially map all projects
need_proj_address1 <- need_proj_address[1:2000, ]
sum(is.na(need_proj_address1$proj_address))
sum(is.na(need_proj_address1$proj_lat))
need_proj_address1 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address1, "need_proj_address1.csv")

# initially map all projects
need_proj_address2 <- need_proj_address[2001:6000, ]
sum(is.na(need_proj_address2$proj_address))
sum(is.na(need_proj_address2$proj_lat))
need_proj_address2 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address2, "need_proj_address2.csv")

# initially map all projects
need_proj_address3 <- need_proj_address[6001:10000, ]
sum(is.na(need_proj_address3$proj_address))
sum(is.na(need_proj_address3$proj_lat))
need_proj_address3 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address3, "need_proj_address3.csv")

# initially map all projects
need_proj_address4 <- need_proj_address[10001:14000, ]
sum(is.na(need_proj_address4$proj_address))
sum(is.na(need_proj_address4$proj_lat))
need_proj_address4 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address4, "need_proj_address4.csv")

# initially map all projects
need_proj_address5 <- need_proj_address[14001:18000, ]
sum(is.na(need_proj_address5$proj_address))
sum(is.na(need_proj_address5$proj_lat))
need_proj_address5 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address5, "need_proj_address5.csv")

# initially map all projects
need_proj_address6 <- need_proj_address[18001:22000, ]
sum(is.na(need_proj_address6$proj_address))
sum(is.na(need_proj_address6$proj_lat))
need_proj_address6 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address6, "need_proj_address6.csv")

# initially map all projects
need_proj_address7 <- need_proj_address[22001:25515, ]
sum(is.na(need_proj_address7$proj_address))
sum(is.na(need_proj_address7$proj_lat))
need_proj_address7 %>% select(proj_address, proj_lat, proj_lon) %>% head(.)
setwd("C:/Users/sdevine/Desktop/master_data")
write_csv(need_proj_address7, "need_proj_address7.csv")

need_proj_address %>% select(Proj.City.Name, Proj.ST.Abbr, Proj.ZIP, proj_address) %>% head(.)

# map 
options(BingMapsKey = "QFG8iBCiInAj6ER1ubuD~I5piVwPPZghOvhCJzBP-1g~AicfV1u7mkoKlY53KfatxR67u-NHXCfu1iEA8dBryA8vlUJy3yu3y0u2cZLWf-D4")

for(i in 1:nrow(need_proj_address7)){
        coordinates <- tryCatch(geocode(need_proj_address7$proj_address[i], service = "bing"), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- str_c(need_proj_address7$Proj.ST.Abbr[i], need_proj_address7$Proj.ZIP[i], 
                                                                 sep = ", ")
                                                geocode(address, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- str_c(need_proj_address7$Proj.ST.Abbr[i], need_proj_address7$Proj.ZIP[i], 
                                                                 sep = ", ")
                                                geocode(address, service = "bing")
                                        }
                                }
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                state_zip <- str_c(need_proj_address7$Proj.ST.Abbr[i], need_proj_address7$Proj.ZIP[i], 
                                              sep = ", ")
                print(state_zip)
                coordinates <- geocode(state_zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_proj_address7$proj_lon[i] <- coordinates[2]
                need_proj_address7$proj_lat[i] <- coordinates[1]
        }
        # if(is.na(coordinates)){
        #         print("error: coordinates are NA")
        #         city_state_zip <- str_c(need_proj_address7$Appl.City.Name, ", ", 
        #                                 need_proj_address7$Appl.State.Abbr[i], ", ", 
        #                                 need_proj_address7$Appl..Zip[i])
        #         print(city_state_zip)
        #         coordinates <- geocode(city_state_zip)
        #         coordinates <- unlist(coordinates)
        #         print(coordinates)
        #         need_proj_address7$proj_lon[i] <- coordinates[2]
        #         need_proj_address7$proj_lat[i] <- coordinates[1]
        # }
        need_proj_address7$proj_lon[i] <- coordinates[2]
        need_proj_address7$proj_lat[i] <- coordinates[1]
}


# jitter duplicate coordinates to avoid overlapping on map
proj_coord <- sapply(1:nrow(shiny_app_data), function(x) str_c(shiny_app_data$proj_lat[x], " ", 
                                                              shiny_app_data$proj_lon[x]))
dup_logical <- duplicated(proj_coord)
dup_index <- which(dup_logical == TRUE)
shiny_jitter <- shiny_app_data
shiny_jitter$proj_coord <- proj_coord
dups <- shiny_jitter[dup_index, ]
unique_dups <- unique(dups$proj_coord)
for(i in 1:length(unique_dups)){
        print(str_c("unique_dups is ", i))
        dup_i <- unique_dups[i]
        row_i <- which(shiny_jitter$proj_coord == dup_i)
        shiny_jitter_dup_i <- shiny_jitter[row_i, ]
        direction_counter <- 1
        for(x in 1:nrow(shiny_jitter_dup_i)){
                print(str_c("shiny_jitter_dup_i is ", x))
                print(str_c("direction_counter is ", direction_counter))
                loop_multiplier <- ceiling(x / 4)
                print(str_c("loop_multiplier is ", loop_multiplier))
                if(direction_counter == 1){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$proj_lat[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lat[x] + .01*loop_multiplier
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("proj_lat + ", .01*loop_multiplier))
                } 
                if(direction_counter == 2){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$proj_lat[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lat[x] - .01*loop_multiplier
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("proj_lat - ", .01*loop_multiplier))
                } 
                if(direction_counter == 3){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$proj_lon[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lon[x] + .01*loop_multiplier
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("proj_lon + ", .01*loop_multiplier))
                }
                if(direction_counter == 4){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        shiny_jitter$proj_lon[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lon[x] - .01*loop_multiplier
                        direction_counter <- 1
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        print(str_c("proj_lon - ", .01*loop_multiplier))
                } else {direction_counter <- direction_counter + 1}
        }
}

# check jitter worked ok
proj_coord <- sapply(1:nrow(shiny_jitter), function(x) str_c(shiny_jitter$proj_lat[x], " ", 
                                                            shiny_jitter$proj_lon[x]))
shiny_jitter$proj_coord <- proj_coord
dup_logical <- duplicated(proj_coord)
sum(dup_logical)
dup_index <- which(dup_logical == TRUE)
dups <- shiny_jitter[dup_index, ]
select(dups, Appl.Short.Name, proj_address, proj_lat, proj_lon) %>% arrange(proj_address)

# save over shiny_app_data with jitter data
shiny_app_data <- shiny_jitter




# helper script to test geocode errors
# new_geo <- geocode("MD, 24000")
# new_lat <- new_geo[2]
# new_lon <- new_geo[1]
# print(str_c(new_lat, new_lon, sep = " "))