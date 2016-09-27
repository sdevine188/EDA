library(dplyr)
library(stringr)
library(readr)

# disable scientific notation
options(scipen=999)

# load master_data
setwd("G:/PNP/Performance Measurement/master_data")
master_data_filename <- list.files()[str_detect(list.files(), "master_data_20")]
md <- read_csv(master_data_filename, col_types = list(DUNS.. = col_character(), Local.Applicant.. = col_number(), Total.Project.. = col_number(), Best.EDA.. = col_number(),
                                                      Private.Investment = col_number(), Control. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                      Appl..Zip = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), 
                                                      Coapp.DUNS.. = col_character()))

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), 
                                                        Coapp.DUNS.. = col_character()))