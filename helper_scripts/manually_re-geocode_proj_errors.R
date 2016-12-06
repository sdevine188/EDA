library(stringr)
library(dplyr)
library(taRifx.geo)
library(ggmap)
library(readr)


# disable scientific notation
options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny_app_data <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# map all projects to see any new mapping errors
popup_control <- sapply(shiny_app_data$Control.No., function(x) ifelse(is.na(x), "NA", x))
popup_control <- sapply(popup_control, function(x) str_c("Control #: ", x))
popup_app_name <- sapply(shiny_app_data$Appl.Short.Name, function(x) ifelse(is.na(x), "NA", x))
popup_app_name <- sapply(popup_app_name, function(x) str_c("Applicant name: ", x))
popup_app_address <- sapply(shiny_app_data$app_address, function(x) ifelse(is.na(x), "NA", x))
popup_app_address <- sapply(popup_app_address, function(x) str_c("Applicant Address: ", x))
popup_proj_address <- sapply(shiny_app_data$proj_address, function(x) ifelse(is.na(x), "NA", x))
popup_proj_address <- sapply(popup_proj_address, function(x) str_c("Project Address: ", x))
popup_fy <- sapply(shiny_app_data$FY, function(x) ifelse(is.na(x), "NA", x))
popup_fy <- sapply(popup_fy, function(x) str_c("Fiscal year: FY ", x))
popup_program <- sapply(shiny_app_data$Program, function(x) ifelse(is.na(x), "NA", x))
popup_program <- sapply(popup_program, function(x) str_c("Program: ", x))
popup_appropriation <- sapply(shiny_app_data$Appropriation, function(x) ifelse(is.na(x), "NA", x))
popup_appropriation <- sapply(popup_appropriation, function(x) str_c("Appropriation: ", x))
popup_funds <- sapply(shiny_app_data$EDA.Funding, function(x) ifelse(is.na(x), "NA", x))
popup_funds <- sapply(popup_funds, function(x) str_c("EDA funds: $", prettyNum(x, big.mark = ",",
                                                                               scientific = FALSE)))

default_popup <- str_c(popup_control, popup_app_name, popup_app_address, popup_proj_address, popup_fy, popup_program,
                       popup_appropriation, popup_funds, sep = "<br/>")

leaflet(shiny_app_data) %>% addTiles() %>% addCircleMarkers(data = shiny_app_data, lng = ~proj_lon, lat = ~proj_lat, popup = default_popup, opacity = 1, fillOpacity = 0)



# manually re-geocode errors
error_address <- "Orocovis, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Orocovis, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Juana Diaz, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Juana Diaz, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Hagatna, GU, 96932"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Hagatna, GU, 96932"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Piti, GU, 96925"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Piti, GU, 96925"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Tamuning, GU, 96931"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Tamuning, GU, 96931"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Dorado, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Dorado, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Cidra, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Cidra, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Agana, GU, 96932"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Agana, GU"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Agana, GU, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Agana, GU"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "GU, 66000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Agana, GU"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Naguabo, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Naguabo, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- ", AS, 96799"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "AS, 96799"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- ", VI, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Frederiksted, VI, 00840"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "VI, 78000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Frederiksted, VI, 00840"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Frederiksted, VI, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Frederiksted, VI, 00840"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- " VI, 78000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Frederiksted, VI, 00840"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Vega Baja, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Vega Baja, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Majuro, MH, 96960"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Majuro, MH, 96960"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Mangilao, GU, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Mangilao, GU"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Mangilao, GU, 96923"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Mangilao, GU"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Pago Pago, AS, 96799"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Koror, PW, 96940"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Pago Pago, AS, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Koror, PW, 96940"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "San Juan, PR, 00940"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "San Juan, PR, 00940"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- ", PW, 96940"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Koror, PW, 96940"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Lares, PR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Lares, PR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Invalid City, OR, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Warm Springs, OR"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Adak Naval Station/Mitchell Field, AK, 98791"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Anchorage, AK, 98791"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Charlotte Amalie, VI, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Charlotte Amalie, VI"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Invalid City, OK, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Drumright, OK, 74030"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Invalid City, VI, 00000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Charlotte Amalie, VI"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "SC, 45000"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Columbia, SC"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

error_address <- "Rota, MP, 96951"
error_row <- which(shiny_app_data$proj_address == error_address)
error_new_address <- "Rota, MP, 96951"
new_geo <- geocode(error_new_address)
new_lat <- as.numeric(new_geo[2])
new_lon <- as.numeric(new_geo[1])
shiny_app_data$proj_lat[error_row] <- new_lat
shiny_app_data$proj_lon[error_row] <- new_lon

# write updated shiny_app_data to file
setwd("C:/Users/sdevine/Desktop/master_data")
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
shiny_filename <- str_c("shiny_app_data_", date2, ".csv")
write_csv(shiny_app_data, path = shiny_filename)




# helper script to test geocode errors
# new_geo <- geocode("Orocovis, PR")
# new_lat <- new_geo[2]
# new_lon <- new_geo[1]
# print(str_c(new_lat, new_lon, sep = " "))
