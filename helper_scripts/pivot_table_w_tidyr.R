library(dplyr)
library(stringr)
library(readr)
library(tidyr)

# disable scientific notation
options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# pivot table with tidyr
test <- shiny %>% select(FY, EDA.Funding, Appl.State.Abbr) %>% filter(FY > 2012) %>% group_by(FY, Appl.State.Abbr) %>%
        summarize(amount = sum(EDA.Funding), count = n()) %>% gather(variable, value, amount, count) %>%
        unite(fy_amount, FY, variable) %>% rename(fy_variable = fy_amount) %>% spread(fy_variable, value)