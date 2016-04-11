need_app_lat <- filter(records, is.na(app_lat), !is.na(app_address))

# geocode addresses
options(BingMapsKey = "QFG8iBCiInAj6ER1ubuD~I5piVwPPZghOvhCJzBP-1g~AicfV1u7mkoKlY53KfatxR67u-NHXCfu1iEA8dBryA8vlUJy3yu3y0u2cZLWf-D4")

for(i in 1:nrow(need_app_lat)){
        coordinates <- tryCatch(geocode(need_app_lat$app_address[i], service = "bing"), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- need_app_lat$app_address[i]
                                                print(address)
                                                zip <- need_app_lat$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- need_app_lat$app_address[i]
                                                print(address)
                                                zip <- need_app_lat$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                address <- need_app_lat$app_address[i]
                print(address)
                city_state_zip <- str_c(need_app_lat$Appl.City.Name[i], ", ", need_app_lat$Appl.State.Abbr[i], ", ", need_app_lat$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_lat$app_lon[i] <- coordinates[2]
                need_app_lat$app_lat[i] <- coordinates[1]
        }
        if(is.na(coordinates)){
                print("error: coordinates are NA")
                city_state_zip <- str_c(need_app_lat$Appl.City.Name[i], ", ", need_app_lat$Appl.State.Abbr[i], ", ", need_app_lat$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip, service = "bing")
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_lat$app_lon[i] <- coordinates[2]
                need_app_lat$app_lat[i] <- coordinates[1]
        }
        need_app_lat$app_lon[i] <- coordinates[2]
        need_app_lat$app_lat[i] <- coordinates[1]
}

unique(need_app_lat$app_lat)
# which addresses mapped
mapped_bing <- need_app_lat %>% filter(!is.na(app_lat)) %>% select(app_address, app_lat, app_lon)
# which did not map
not_mapped_bing <- need_app_lat %>% filter(is.na(app_lat)) %>% select(app_address, app_lat, app_lon)

# try using google
# google is definitely better at mapping
for(i in 1:nrow(need_app_lat)){
        coordinates <- tryCatch(geocode(need_app_lat$app_address[i]), 
                                warning = function(w) {
                                        if(grepl("geocode failed with status ZERO_RESULTS", w)){
                                                print("this is a warning")
                                                address <- need_app_lat$app_address[i]
                                                zip <- need_app_lat$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                },
                                error = function(e) {
                                        if(grepl("replacement has length zero", e)){
                                                print("this is an error")
                                                address <- need_app_lat$app_address[i]
                                                zip <- need_app_lat$Appl..Zip[i]
                                                geocode(zip, service = "bing")
                                        }
                                } 
        )
        coordinates <- unlist(coordinates)
        print(i)
        print(coordinates)
        if(is.null(coordinates)){
                print("error: coordinates are null")
                city_state_zip <- str_c(need_app_lat$Appl.City.Name[i], ", ", need_app_lat$Appl.State.Abbr[i], ", ", need_app_lat$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip)
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_lat$app_lon[i] <- coordinates[2]
                need_app_lat$app_lat[i] <- coordinates[1]
        }
        if(is.na(coordinates)){
                print("error: coordinates are NA")
                city_state_zip <- str_c(need_app_lat$Appl.City.Name[i], ", ", need_app_lat$Appl.State.Abbr[i], ", ", need_app_lat$Appl..Zip[i])
                print(city_state_zip)
                coordinates <- geocode(city_state_zip)
                coordinates <- unlist(coordinates)
                print(coordinates)
                need_app_lat$app_lon[i] <- coordinates[2]
                need_app_lat$app_lat[i] <- coordinates[1]
        }
        need_app_lat$app_lon[i] <- coordinates[2]
        need_app_lat$app_lat[i] <- coordinates[1]
}

unique(need_app_lat$app_lat)
# which addresses mapped
mapped_google <- need_app_lat %>% filter(!is.na(app_lat)) %>% select(app_address, app_lat, app_lon)
# which did not map
not_mapped_google <- need_app_lat %>% filter(is.na(app_lat)) %>% select(app_address, app_lat, app_lon)

# re-merge need_app_lat with shiny_app_data currently open in clean_shiny_data script
minus_missing_app_lat <- filter(shiny_app_data, !is.na(app_lat))
shiny_app_data <- rbind(minus_missing_app_lat, need_app_lat)
      