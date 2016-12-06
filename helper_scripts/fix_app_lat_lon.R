dim(shiny_app_data)

change <- shiny_app_data %>% filter(app_lat >= 106 | app_lat <= -50)
dim(change)

not_change <- shiny_app_data %>% filter((app_lat <= 106 & app_lat >= -50) | is.na(app_lat))
dim(not_change)

sum(is.na(shiny_app_data$app_lat))
1361 + 24829

change %>% select(app_lat, app_lon, Control.No.) %>% head(.)

for(i in 1:nrow(change)) {
        new_app_lon <- change$app_lat[i]
        new_app_lat <- change$app_lon[i]
        change$app_lat[i] <- new_app_lat
        change$app_lon[i] <- new_app_lon
}

shiny_app_data <- rbind(change, not_change)
dim(shiny_app_data2)
shiny_app_data <- shiny_app_data %>% arrange(desc(FY))
write_csv(shiny_app_data, "shiny_app_data_20160720.csv")


africa <- 110146, 110148, 74874, 110221, 100402
shiny_app_data %>% filter(Control.No. == "100402") %>% select(app_lat, app_lon, app_address, Full.Applicant.Name, Appl.Short.Name, Region.Name)
geocode("Frederiksted, VI, 00840")
geocode("UNIVERSITY OF PITTSBURGH")

for(i in 1:nrow(recode)) {
        error_address <- recode$error_address[i]
        error_row <- which(shiny_app_data$app_address == error_address)
        print(error_row)
        shiny_app_data$app_lat[error_row] <- recode$app_lat[i]
        shiny_app_data$app_lon[error_row] <- recode$app_lon[i]
}








