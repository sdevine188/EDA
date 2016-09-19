library(stringr)
library(readr)
library(dplyr)

# set wd
setwd("G:/PNP/Performance Measurement/rshinyapp/clean_shiny_data")

# load unedited initiative codes and clean for use in shiny app
initiatives_filename <- list.files()[str_detect(list.files(), "initiatives_unedited_20")]
init <- data.frame(read_csv(initiatives_filename))
init <- init[ , 1:2]
names(init) <- c("code", "description")
init$code_description <- str_c(init$code, init$description, sep = " - ")

# write file
write_csv(init, "initiatives.csv")

