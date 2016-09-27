library(ggmap)
library(stringr)
library(dplyr)
library(lubridate)
# library(taRifx.geo)
library(readr)

# set working directory where master_data file is saved
setwd("G:/PNP/Performance Measurement/master_data")

# turn-off scientific notation
options(scipen=999)

# read in latest master_data
master_data_filename <- list.files()[str_detect(list.files(), "master_data_20")]
md <- read_csv(master_data_filename, col_types = list(DUNS.. = col_character(), Local.Applicant.. = col_number(), Total.Project.. = col_number(), Best.EDA.. = col_number(),
                                                      Private.Investment = col_number(), Control. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                      Appl..Zip = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), 
                                                      Coapp.DUNS.. = col_character()))

# read in archived shiny data with lat/long info
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")

shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny_app_data <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                                 Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                                 Appl.ZIP = col_character(), Initiatives = col_character()))
shiny_app_data <- select(shiny_app_data, Control.No., app_address, app_lat, app_lon, proj_address, proj_lat, proj_lon)

## merge existing shiny data with latest master_data 

# run this if you only want to include approved grants 
# records <- filter(md, Status == "Approved")

records <- md
records <- left_join(records, shiny_app_data, by = c("Control." = "Control.No."))

# which records are new and don't yet have an app_address field created
need_app_address <- filter(records, is.na(app_address))
dim(need_app_address)

# compile address field to use in geocoding
need_app_address$app_address <- str_c(need_app_address$Appl.Street.Addr.1, need_app_address$Appl.City.Name, need_app_address$Appl.State.Abbr, need_app_address$Appl..Zip, sep = ", ")

# remove Multi City
for(i in 1:nrow(need_app_address)){
        if(!(is.na(need_app_address$Proj.City.Name[i]))){
                if(need_app_address$Proj.City.Name[i] == "Multi City"){
                        need_app_address$app_address[i] <- str_c(need_app_address$Proj.ST.Abbr[i], 
                                                                   need_app_address$Proj.ZIP[i], sep = ", ")
                }
        }
}

need_app_address$app_address <- as.character(need_app_address$app_address)
# replace any NAs with blanks to avoid breaking geocode
need_app_address$app_address[which(is.na(need_app_address$app_address))] <- ""

# tarifx.geo is deprecated, so bing maps no longer works, so just use google maps
for(i in 1:nrow(need_app_address)){
        coordinates <- tryCatch(geocode(need_app_address$app_address[i]), 
                                        warning = function(w) {
                                                if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                        print("this is a warning")
                                                        if(!(is.na(need_app_address$Appl.State.Abbr[i])) && !(is.na(need_app_address$Appl..Zip[i]))){
                                                                address <- str_c(need_app_address$Appl.State.Abbr[i], need_app_address$Appl..Zip[i], 
                                                                                 sep = ", ")
                                                                geocode(address)
                                                        } else {
                                                                print(i)
                                                                print("Appl.State.Abbr or Appl..Zip is NA, skipping to next record")
                                                                next
                                                        }
                                                }
                                        },
                                        error = function(e) {
                                                if(grepl("replacement has length zero", e)){
                                                        print("this is an error")
                                                        if(!(is.na(need_app_address$Appl.State.Abbr[i])) && !(is.na(need_app_address$Appl..Zip[i]))){
                                                                address <- str_c(need_app_address$Appl.State.Abbr[i], need_app_address$Appl..Zip[i], 
                                                                                 sep = ", ")
                                                                geocode(address)
                                                        } else {
                                                                print(i)
                                                                print("Appl.State.Abbr or Appl..Zip is NA, skipping to next record")
                                                                next
                                                        }
                                                }
                                        }
                )
                coordinates <- unlist(coordinates)
                print(i)
                print(coordinates)
                if(is.null(coordinates)){
                        print("error: coordinates are null")
                        if(!(is.na(need_app_address$Appl.State.Abbr[i])) && !(is.na(need_app_address$Appl..Zip[i]))){
                                
                                state_zip <- str_c(need_app_address$Appl.State.Abbr[i], need_app_address$Appl..Zip[i], 
                                                   sep = ", ")
                                print(state_zip)
                                coordinates <- geocode(state_zip)
                                coordinates <- unlist(coordinates)
                                print(coordinates)
                        } else {
                                print(i)
                                print("Appl.State.Abbr or Appl..Zip is NA, skipping to next record")
                                next
                        }
                }
                need_app_address$app_lon[i] <- coordinates[1]
                need_app_address$app_lat[i] <- coordinates[2]
}

# see how many of the NA's google was not able to map
sum(is.na(need_app_address$app_address))

# correct any known geocode errors - some are bing map errors, others are ugly addresses
setwd("G:/PNP/Performance Measurement/rshinyapp/clean_shiny_data")
recode <- data.frame(read_csv("re-geocode_errors.csv"))

# loop through list re-geocoding
for(i in 1:nrow(recode)) {
        error_address <- recode$error_address[i]
        error_row <- which(need_app_address$app_address == error_address)
        print(error_row)
        need_app_address$app_lat[error_row] <- recode$app_lat[i]
        need_app_address$app_lon[error_row] <- recode$app_lon[i]
}

# re-combine the existing master_data with the newly mapped and jittered applications
records_minus <- filter(records, !is.na(app_address))
shiny_app_data <- rbind(records_minus, need_app_address)

# check to make sure all shiny_app_data records have app_address, app_lat, app_lon
length(which(is.na(shiny_app_data$app_address)))
length(which(is.na(shiny_app_data$app_lat)))
na_length <- length(which(is.na(shiny_app_data$app_lon)))
# inspect those still missing app_lat to ensure they are hopeless cases
filter(shiny_app_data, is.na(app_lat)) %>% select(app_address, app_lat, app_lon) %>% data.frame(.) %>% head(., na_length)

# manual fix if rebuilding shiny_app_data from scratch
# clean known special character issues that break shiny app map
# espanola_row <- which(shiny_app_data$Control. == 76796)
# shiny_app_data$Appl.Short.Name[espanola_row] <- "Espanola Valley FA Ctr"
# shiny_app_data$Full.Applicant.Name[espanola_row] <- "Espanola Valley Fiber Arts Center"






# geo-code for proj_address
need_proj_address <- filter(shiny_app_data, is.na(proj_address))

# compile address field to use in geocoding
need_proj_address$proj_address <- str_c(need_proj_address$Proj.City.Name, need_proj_address$Proj.ST.Abbr, 
                                        need_proj_address$Proj.ZIP, sep = ", ")
                                      
# remove Multi City
for(i in 1:nrow(need_proj_address)) {
        if(!(is.na(need_proj_address$Proj.City.Name[i]))){
                if(need_proj_address$Proj.City.Name[i] == "Multi City") {
                        need_proj_address$proj_address[i] <- str_c(need_proj_address$Proj.ST.Abbr[i], need_proj_address$Proj.ZIP[i], 
                                                      sep = ", ")
                } 
        }
}

need_proj_address$proj_address <- as.character(need_proj_address$proj_address)
need_proj_address %>% select(Proj.City.Name, Proj.ST.Abbr, Proj.ZIP, proj_address, proj_lat, proj_lon) %>% head(.)
dim(need_proj_address)

# map 
for(i in 1:nrow(need_proj_address)){
        coordinates <- tryCatch(geocode(need_proj_address$proj_address[i]), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                if(!(is.na(need_proj_address$Proj.ST.Abbr[i])) && !(is.na(need_proj_address$Proj.ZIP[i]))){
                                                        address <- str_c(need_proj_address$Proj.ST.Abbr[i], need_proj_address$Proj.ZIP[i], 
                                                                         sep = ", ")
                                                        geocode(address)
                                                } else {
                                                        print(i)
                                                        print("Proj.ST.Abbr or Proj.ZIP is NA, skipping to next record")
                                                        next
                                                }
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                if(!(is.na(need_proj_address$Proj.ST.Abbr[i])) && !(is.na(need_proj_address$Proj.ZIP[i]))){
                                                        address <- str_c(need_proj_address$Proj.ST.Abbr[i], need_proj_address$Proj.ZIP[i], 
                                                                         sep = ", ")
                                                        geocode(address)
                                                } else {
                                                        print(i)
                                                        print("Proj.ST.Abbr or Proj.ZIP is NA, skipping to next record")
                                                        next
                                                }
                                        }
                                }
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                if(!(is.na(need_proj_address$Proj.ST.Abbr[i])) && !(is.na(need_proj_address$Proj.ZIP[i]))){
                        
                        state_zip <- str_c(need_proj_address$Proj.ST.Abbr[i], need_proj_address$Proj.ZIP[i], 
                                                      sep = ", ")
                        print(state_zip)
                        coordinates <- geocode(state_zip)
                        coordinates <- unlist(coordinates)
                        print(coordinates)
                } else {
                        print(i)
                        print("Proj.ST.Abbr or Proj.ZIP is NA, skipping to next record")
                        next
                }
        }
        need_proj_address$proj_lon[i] <- coordinates[1]
        need_proj_address$proj_lat[i] <- coordinates[2]
}

# see how many of the NA's google was not able to map
sum(is.na(need_proj_address$proj_lat))

# re-combine the existing master_data with the newly mapped and jittered applications
shiny_app_data_minus <- filter(shiny_app_data, !is.na(proj_lat))
shiny_app_data <- rbind(shiny_app_data_minus, need_proj_address)

# check to make sure all shiny_app_data records have app_address, app_lat, app_lon
length(which(is.na(shiny_app_data$proj_address)))
length(which(is.na(shiny_app_data$proj_lat)))
na_length <- length(which(is.na(shiny_app_data$proj_lon)))
# inspect those still missing proj_lat to ensure they are hopeless cases
filter(shiny_app_data, is.na(proj_lat)) %>% select(proj_address, proj_lat, proj_lon, Proj.ZIP, Proj.City.Name, Proj.ST.Abbr, database, FY) %>% data.frame(.) %>% head(., na_length)






# jitter duplicate app coordinates to avoid overlapping on map
app_coord <- sapply(1:nrow(shiny_app_data), function(x) str_c(shiny_app_data$app_lat[x], " ", 
                                                              shiny_app_data$app_lon[x]))
dup_logical <- duplicated(app_coord)
dup_index <- which(dup_logical == TRUE)
shiny_jitter <- shiny_app_data
shiny_jitter$app_coord <- app_coord
dups <- shiny_jitter[dup_index, ]
unique_dups <- unique(dups$app_coord)
length(unique_dups)
dups %>% select(app_coord, app_address) %>% data.frame(.) %>% head(., 50)

for(i in 1:length(unique_dups)){
        print(str_c("unique_dups number is ", i))
        dup_i <- unique_dups[i]
        print(str_c("unique_dups is ", dup_i))
        row_i <- which(shiny_jitter$app_coord == dup_i)
        print(str_c("shiny_jitter$app_coord match row is ", row_i))
        shiny_jitter_dup_i <- shiny_jitter[row_i, ]
        direction_counter <- 1
        for(x in 1:nrow(shiny_jitter_dup_i)){
                print(str_c("shiny_jitter_dup_i row is ", x))
                print(str_c("direction_counter is ", direction_counter))
                loop_multiplier <- ceiling(x / 4)
                print(str_c("loop_multiplier is ", loop_multiplier))
                if(direction_counter == 1){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        print(str_c("existing coord is ", shiny_jitter_dup_i$app_lat[x]))
                        proposed_new_app_lat <- shiny_jitter_dup_i$app_lat[x] + .01*loop_multiplier
                        # issue might be here, if it's overwriting proposed new app lat for all app_lat matches??
                        pre_existing_matches <- which(shiny_jitter$app_lat == proposed_new_app_lat)
                        while(length(pre_existing_matches) > 0) {
                                print("there was a pre-existing match, jittering further")
                                print(str_c("current proposed coord is ", proposed_new_app_lat))
                                proposed_new_app_lat <- proposed_new_app_lat + .01
                                pre_existing_matches <- which(shiny_jitter$app_lat == proposed_new_app_lat)
                                print(str_c("will try new proposed coord: ", proposed_new_app_lat))
                                print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$app_lat[shiny_jitter_row] <- proposed_new_app_lat
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("app_lat + ", .01*loop_multiplier))
                        print(str_c("app_lat is now ", proposed_new_app_lat))
                } 
                if(direction_counter == 2){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        print(str_c("existing coord is ", shiny_jitter_dup_i$app_lat[x]))
                        proposed_new_app_lat <- shiny_jitter_dup_i$app_lat[x] - .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$app_lat == proposed_new_app_lat)
                        while(length(pre_existing_matches) > 0) {
                                print("there was a pre-existing match, jittering further")
                                print(str_c("current proposed coord is ", proposed_new_app_lat))
                                # print(str_c("test of matching: ", which(shiny_jitter$app_lat == "15.157801")))
                                # print(str_c("test of matching length: ", length(which(shiny_jitter$app_lat == "15.157801"))))
                                proposed_new_app_lat <- proposed_new_app_lat - .01
                                # print(str_c("test proposed_new_app_lat equals: ", proposed_new_app_lat))
                                # print(str_c("test of proposed_new_app_lat class: ", class(proposed_new_app_lat)))
                                pre_existing_matches <- which(shiny_jitter$app_lat == proposed_new_app_lat)
                                # print(str_c("test print of pre_existing_matches: ", pre_existing_matches))
                                print(str_c("will try new proposed coord: ", proposed_new_app_lat))
                                print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$app_lat[shiny_jitter_row] <- proposed_new_app_lat
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("app_lat - ", .01*loop_multiplier))
                        print(str_c("app_lat is now ", proposed_new_app_lat))
                } 
                if(direction_counter == 3){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        print(str_c("existing coord is ", shiny_jitter_dup_i$app_lon[x]))
                        proposed_new_app_lon <- shiny_jitter_dup_i$app_lon[x] + .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$app_lon == proposed_new_app_lon)
                        while(length(pre_existing_matches) > 0) {
                                print("there was a pre-existing match, jittering further")
                                print(str_c("current proposed coord is ", proposed_new_app_lon))
                                proposed_new_app_lon <- proposed_new_app_lon + .01
                                pre_existing_matches <- which(shiny_jitter$app_lon == proposed_new_app_lon)
                                print(str_c("will try new proposed coord: ", proposed_new_app_lon))
                                print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$app_lon[shiny_jitter_row] <- proposed_new_app_lon
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("app_lon + ", .01*loop_multiplier))
                        print(str_c("app_lon is now ", proposed_new_app_lon))
                }
                if(direction_counter == 4){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        print(str_c("existing coord is ", shiny_jitter_dup_i$app_lon[x]))
                        proposed_new_app_lon <- shiny_jitter_dup_i$app_lon[x] - .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$app_lon == proposed_new_app_lon)
                        while(length(pre_existing_matches) > 0) {
                                print("there was a pre-existing match, jittering further")
                                print(str_c("current proposed coord is ", proposed_new_app_lon))
                                proposed_new_app_lon <- proposed_new_app_lon - .01
                                pre_existing_matches <- which(shiny_jitter$app_lon == proposed_new_app_lon)
                                print(str_c("will try new proposed coord: ", proposed_new_app_lon))
                                print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$app_lon[shiny_jitter_row] <- proposed_new_app_lon
                        direction_counter <- 1
                        print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("app_lon - ", .01*loop_multiplier))
                        print(str_c("app_lon is now ", proposed_new_app_lon))
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
dim(dups)
select(dups, Control., Appl.Short.Name, app_address, app_lat, app_lon, app_coord) %>% arrange(app_address) %>% data.frame(.) %>% head(., 50)

# if still duplicate app_coords that should have jitterd, run lines below and jitter loop again (need to debug further)
# dup_logical <- duplicated(app_coord)
# dup_index <- which(dup_logical == TRUE)
# dups <- shiny_jitter[dup_index, ]
# unique_dups <- unique(dups$app_coord)
# then run jitter loop, and redo diagnostic jitter check above to see if duplicates successfully jittered

# save over need_app_address with jitter data
shiny_app_data <- shiny_jitter
shiny_app_data <- shiny_app_data %>% select(-app_coord)

# jitter duplicate proj coordinates to avoid overlapping on map
proj_coord <- sapply(1:nrow(shiny_app_data), function(x) str_c(shiny_app_data$proj_lat[x], " ", 
                                                              shiny_app_data$proj_lon[x]))
dup_logical <- duplicated(proj_coord)
dup_index <- which(dup_logical == TRUE)
shiny_jitter <- shiny_app_data
shiny_jitter$proj_coord <- proj_coord
dups <- shiny_jitter[dup_index, ]
dups <- shiny_jitter[dup_logical, ]
unique_dups <- unique(dups$proj_coord)
length(unique_dups)
dups %>% select(app_coord, app_address) %>% data.frame(.) %>% head(., 50)

for(i in 1:length(unique_dups)){
        # print(str_c("unique_dups is ", i))
        dup_i <- unique_dups[i]
        # print(dup_i)
        row_i <- which(shiny_jitter$proj_coord == dup_i)
        shiny_jitter_dup_i <- shiny_jitter[row_i, ]
        direction_counter <- 1
        for(x in 1:nrow(shiny_jitter_dup_i)){
                # print(str_c("shiny_jitter_dup_i is ", x))
                # print(str_c("direction_counter is ", direction_counter))
                loop_multiplier <- ceiling(x / 4)
                # print(str_c("loop_multiplier is ", loop_multiplier))
                if(direction_counter == 1){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        # print(str_c("existing coord is ", shiny_jitter_dup_i$proj_lat[x]))
                        proposed_new_proj_lat <- shiny_jitter_dup_i$proj_lat[x] + .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$proj_lat == proposed_new_proj_lat)
                        while(length(pre_existing_matches) > 0) {
                                # print("there was a pre-existing match, jittering further")
                                # print(str_c("current proposed coord is ", proposed_new_proj_lat))
                                proposed_new_proj_lat <- proposed_new_proj_lat + .01
                                pre_existing_matches <- which(shiny_jitter$proj_lat == proposed_new_proj_lat)
                                # print(str_c("will try new proposed coord: ", proposed_new_proj_lat))
                                # print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$proj_lat[shiny_jitter_row] <- proposed_new_proj_lat
                        # shiny_jitter$proj_lat[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lat[x] + .01*loop_multiplier
                        # print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        # print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("proj_lat + ", .01*loop_multiplier))
                        # print(str_c("proj_lat is now ", proposed_new_proj_lat))
                } 
                if(direction_counter == 2){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        print(str_c("existing coord is ", shiny_jitter_dup_i$proj_lat[x]))
                        proposed_new_proj_lat <- shiny_jitter_dup_i$proj_lat[x] - .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$proj_lat == proposed_new_proj_lat)
                        while(length(pre_existing_matches) > 0) {
                                # print("there was a pre-existing match, jittering further")
                                # print(str_c("current proposed coord is ", proposed_new_proj_lat))
                                # print(str_c("test of matching: ", which(shiny_jitter$proj_lat == "15.157801")))
                                # print(str_c("test of matching length: ", length(which(shiny_jitter$proj_lat == "15.157801"))))
                                proposed_new_proj_lat <- proposed_new_proj_lat - .01
                                # print(str_c("test proposed_new_proj_lat equals: ", proposed_new_proj_lat))
                                # print(str_c("test of proposed_new_proj_lat class: ", class(proposed_new_proj_lat)))
                                pre_existing_matches <- which(shiny_jitter$proj_lat == proposed_new_proj_lat)
                                # print(str_c("test print of pre_existing_matches: ", pre_existing_matches))
                                # print(str_c("will try new proposed coord: ", proposed_new_proj_lat))
                                # print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$proj_lat[shiny_jitter_row] <- proposed_new_proj_lat
                        # shiny_jitter$proj_lat[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lat[x] - .01*loop_multiplier
                        # print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        # print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("proj_lat - ", .01*loop_multiplier))
                        # print(str_c("proj_lat is now ", proposed_new_proj_lat))
                } 
                if(direction_counter == 3){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        # print(str_c("existing coord is ", shiny_jitter_dup_i$proj_lon[x]))
                        proposed_new_proj_lon <- shiny_jitter_dup_i$proj_lon[x] + .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$proj_lon == proposed_new_proj_lon)
                        while(length(pre_existing_matches) > 0) {
                                # print("there was a pre-existing match, jittering further")
                                # print(str_c("current proposed coord is ", proposed_new_proj_lon))
                                proposed_new_proj_lon <- proposed_new_proj_lon + .01
                                pre_existing_matches <- which(shiny_jitter$proj_lon == proposed_new_proj_lon)
                                # print(str_c("will try new proposed coord: ", proposed_new_proj_lon))
                                # print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$proj_lon[shiny_jitter_row] <- proposed_new_proj_lon
                        # shiny_jitter$proj_lon[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lon[x] + .01*loop_multiplier
                        # print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        # print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("proj_lon + ", .01*loop_multiplier))
                        # print(str_c("proj_lon is now ", proposed_new_proj_lon))
                }
                if(direction_counter == 4){
                        shiny_jitter_row <- which(shiny_jitter$Control. == shiny_jitter_dup_i$Control.[x])
                        # print(str_c("existing coord is ", shiny_jitter_dup_i$proj_lon[x]))
                        proposed_new_proj_lon <- shiny_jitter_dup_i$proj_lon[x] - .01*loop_multiplier
                        pre_existing_matches <- which(shiny_jitter$proj_lon == proposed_new_proj_lon)
                        while(length(pre_existing_matches) > 0) {
                                # print("there was a pre-existing match, jittering further")
                                # print(str_c("current proposed coord is ", proposed_new_proj_lon))
                                proposed_new_proj_lon <- proposed_new_proj_lon - .01
                                pre_existing_matches <- which(shiny_jitter$proj_lon == proposed_new_proj_lon)
                                # print(str_c("will try new proposed coord: ", proposed_new_proj_lon))
                                # print(str_c("pre_existing_matches = ", length(pre_existing_matches)))
                        }
                        shiny_jitter$proj_lon[shiny_jitter_row] <- proposed_new_proj_lon
                        # shiny_jitter$proj_lon[shiny_jitter_row] <- shiny_jitter_dup_i$proj_lon[x] - .01*loop_multiplier
                        direction_counter <- 1
                        # print(str_c("Control. is ", shiny_jitter_dup_i$Control.[x]))
                        # print(str_c("shiny_jitter row is ", shiny_jitter_row))
                        # print(str_c("proj_lon - ", .01*loop_multiplier))
                        # print(str_c("proj_lon is now ", proposed_new_proj_lon))
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
dim(dups)
select(dups, Appl.Short.Name, proj_address, proj_lat, proj_lon) %>% arrange(proj_address) %>% data.frame(.) %>% head(., 50)

# save over shiny_app_data with jitter data
shiny_app_data <- shiny_jitter
shiny_app_data <- shiny_app_data %>% select(-proj_coord)

# rename variables
shiny_app_data <- rename(shiny_app_data, "Control.No." = Control., "EDA.Funding" = Best.EDA.., "Construction" = Cons.Non, 
       "Total.Proj.Cost" = Total.Project.., "CFDA" = CFDA.., "Appl.County.Name" = Appl.Cnty.Name, 
       "Appl.ZIP" = Appl..Zip, "Proj.State.Abbr" = Proj.ST.Abbr, "Region.Code" = RO.., "IRS" = IRS.., 
       "DUNS" = DUNS.., "Proj.Comp.Code" = Proj.Comp..Code,  
       # "PPR.Code" = PPR.Act, "DEC.Code" = DEC.Act, "GPE.Date" = X.GPE._Date, "GSD.Date" = X.GSD._Date,
       # "PRD.Code" = X.PRD._Action.Code, "PRD.Date" = X.PRD._Date, "PCL.Date" = X.PCL._Date, 
       "PPS.Date" = X.PPS._Date, "PPE.Date" = X.PPE._Date, "PX1.Date" = X.PX1._Date, 
       "PX2.Date" = X.PX2._Date, "Local.Applicant.Match" = Local.Applicant..)




# write shiny data to file
# setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
setwd("C:/Users/sdevine/Desktop/master_data")
# setwd("C:/Users/mlofthus/Desktop")
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
shiny_filename <- str_c("shiny_app_data_", date2, ".csv")
write_csv(shiny_app_data, shiny_filename)




# helper script to test geocode errors
# new_geo <- geocode("MD, 24000")
# new_lat <- new_geo[2]
# new_lon <- new_geo[1]
# print(str_c(new_lat, new_lon, sep = " "))