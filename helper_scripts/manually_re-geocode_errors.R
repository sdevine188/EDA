library(stringr)
library(dplyr)
library(taRifx.geo)
library(ggmap)
library(readr)

# manually re-geocode errors
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")

filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny_app_data <- read_csv(filename, col_types = list("Control." = col_character()))

# re-geocode errors
error_address <- "UOG Station, Mangilao, GU, 96923"
error_row <- which(shiny_app_data$app_address == error_address)
error_new_address <- "Mangilao, GU, 96923"
new_geo <- geocode(error_new_address)
new_lat <- new_geo[1]
new_lon <- new_geo[2]
shiny_app_data$app_lat[error_row] <- new_lat
shiny_app_data$app_lon[error_row] <- new_lon

error_address <- "1316 Anatahan Drive, Saipan, MP, 96950"
error_row <- which(shiny_app_data$app_address == error_address)
error_new_address <- "Saipan, MP, 96950"
new_geo <- geocode(error_new_address)
new_lat <- new_geo[1]
new_lon <- new_geo[2]
shiny_app_data$app_lat[error_row] <- new_lat
shiny_app_data$app_lon[error_row] <- new_lon

error_address <- "Box  1365, Cayey, PR, 88888"
error_row <- which(shiny_app_data$app_address == error_address)
error_new_address <- "Cayey, PR, 88888"
new_geo <- geocode(error_new_address)
new_lat <- new_geo[1]
new_lon <- new_geo[2]
shiny_app_data$app_lat[error_row] <- new_lat
shiny_app_data$app_lon[error_row] <- new_lon


# write updated shiny_app_data to file
setwd("G:/PNP/Performance Measurement/rshinyapp")
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
shiny_filename <- str_c("shiny_app_data_", date2, ".csv")
write_csv(shiny_app_data, path = shiny_filename)

