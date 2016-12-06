library(readr)
library(stringr)
library(dplyr)
library(ggmap)
library(leaflet)


setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))
dim(shiny)

# find records with proj map in the artic or antarctic
change <- shiny %>% filter(proj_lat >=  70 | proj_lat <= -64)
dim(change)

not_change <- shiny %>% filter(!(proj_lat >=  70), !(proj_lat <= -64))
dim(not_change)

na_coord <- shiny %>% filter(is.na(proj_lat))
dim(na_coord)

# confirm not dropping any records
dim(change)[1] + dim(not_change)[1] + dim(na_coord)[1] == dim(shiny)[1]

# swap lat coord with lon coord to fix mapping issue        
change %>% select(proj_lat, proj_lon, Control.No.) %>% head(.)

for(i in 1:nrow(change)) {
        new_proj_lon <- change$proj_lat[i]
        new_proj_lat <- change$proj_lon[i]
        change$proj_lat[i] <- new_proj_lat
        change$proj_lon[i] <- new_proj_lon
}

# test coordinate mapping
popup_control <- sapply(change$Control.No., function(x) ifelse(is.na(x), "NA", x))
popup_control <- sapply(popup_control, function(x) str_c("Control #: ", x))
popup_app_name <- sapply(change$Appl.Short.Name, function(x) ifelse(is.na(x), "NA", x))
popup_app_name <- sapply(popup_app_name, function(x) str_c("Applicant name: ", x))
popup_app_address <- sapply(change$app_address, function(x) ifelse(is.na(x), "NA", x))
popup_app_address <- sapply(popup_app_address, function(x) str_c("Applicant Address: ", x))
popup_proj_address <- sapply(change$proj_address, function(x) ifelse(is.na(x), "NA", x))
popup_proj_address <- sapply(popup_proj_address, function(x) str_c("Project Address: ", x))
popup_fy <- sapply(change$FY, function(x) ifelse(is.na(x), "NA", x))
popup_fy <- sapply(popup_fy, function(x) str_c("Fiscal year: FY ", x))
popup_program <- sapply(change$Program, function(x) ifelse(is.na(x), "NA", x))
popup_program <- sapply(popup_program, function(x) str_c("Program: ", x))
popup_appropriation <- sapply(change$Appropriation, function(x) ifelse(is.na(x), "NA", x))
popup_appropriation <- sapply(popup_appropriation, function(x) str_c("Appropriation: ", x))
popup_funds <- sapply(change$EDA.Funding, function(x) ifelse(is.na(x), "NA", x))
popup_funds <- sapply(popup_funds, function(x) str_c("EDA funds: $", prettyNum(x, big.mark = ",",
                                                                               scientific = FALSE)))

default_popup <- str_c(popup_control, popup_app_name, popup_app_address, popup_proj_address, popup_fy, popup_program,
                       popup_appropriation, popup_funds, sep = "<br/>")

leaflet(change) %>% addTiles() %>% addCircleMarkers(data = change, lng = ~proj_lon, lat = ~proj_lat, popup = default_popup, opacity = 1, fillOpacity = 0)

# fix any remaining mapping issues
fix <- c("110516", "110142", "110141")

change$proj_lat[which(change$Control.No. == "110516")] <- as.numeric(geocode("Barrow, AK, 99723")[2])
change$proj_lon[which(change$Control.No. == "110516")] <- as.numeric(geocode("Barrow, AK, 99723")[1])
change$proj_lat[which(change$Control.No. == "110141")] <- as.numeric(geocode("TN, 47000")[2])
change$proj_lon[which(change$Control.No. == "110141")] <- as.numeric(geocode("TN, 47000")[1])

# based on manual inspection, noted that "GA, 13000" was plotting to washington dc georgia ave, so manually geocode it to atlanta, ga, which is app_city_name
# change$proj_lat[which(change$Control.No. == "110142")] <- as.numeric(geocode("GA, 13000")[2])
# change$proj_lon[which(change$Control.No. == "110142")] <- as.numeric(geocode("GA, 13000")[1])
shiny$Proj.County.Name[which(shiny$Control.No. == "110142")]
shiny$Proj.City.Name[which(shiny$Control.No. == "110142")]
shiny$Appl.City.Name[which(shiny$Control.No. == "110142")]
shiny$app_address[which(shiny$Control.No. == "110142")]
shiny$proj_address[which(shiny$Control.No. == "110142")]
change$app_address[which(change$Control.No. == "110142")]
change$proj_address[which(change$Control.No. == "110142")]

# manually geocode to atlanta ga and knoxville
change$proj_lat[which(change$Control.No. == "110142")] <- as.numeric(geocode("atlanta, ga")[2])
change$proj_lon[which(change$Control.No. == "110142")] <- as.numeric(geocode("atlanta, ga")[1])

# recombine changes with not_changed
final <- rbind(change, not_change, na_coord)
dim(final)
dim(shiny)

# confirm not dropping any records
dim(final)[1] == dim(shiny)[1]

# write file
setwd("C:/Users/sdevine/Desktop/master_data")
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
shiny_filename <- str_c("shiny_app_data_", date2, ".csv")
write_csv(final, shiny_filename)
