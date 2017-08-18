library(stringr)
library(readr)
library(dplyr)
library(scales)

options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(), 
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# find count/amount for fy 2012-2016 awards
shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

# find count/amount and jcr/pi for fy 2012-2016 construction projects
shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, Construction %in% c("C", "B")) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)),
                  js = sum(Est.Jobs.Saved, na.rm = TRUE), jc = sum(Est.Jobs.Created, na.rm = TRUE), 
                  jcr = comma(sum(js, jc)), 
                  pi = dollar(sum(Est.Private.Investment, na.rm = TRUE)))

# using new summary bullet structure - find count/amount and jcr/pi for fy 2012-2016 construction projects
shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>% 
        mutate_each(funs(ifelse(is.na(.), 0, .)), c(Est.Private.Investment, Est.Jobs.Created, Est.Jobs.Saved)) %>%
        mutate(jcr_pi_flag = case_when(.$Est.Jobs.Created > 0 | .$Est.Jobs.Saved > 0 | .$Est.Private.Investment > 0 ~ 1, TRUE ~ 0)) %>% 
        group_by(jcr_pi_flag) %>% summarize(count = n(), eda_funding = sum(EDA.Funding, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                                            jc = sum(Est.Jobs.Created, na.rm = TRUE),
                                            jcr = sum(js, jc, na.rm = TRUE), jcr_ratio = eda_funding / jcr, pi = sum(Est.Private.Investment, na.rm = TRUE),
                                            pi_ratio = pi / eda_funding) %>% mutate(eda_funding = dollar(eda_funding),
                                                                                    jcr = comma(jcr), jcr_ratio = dollar(jcr_ratio), 
                                                                                    pi = dollar(pi), pi_ratio = dollar(pi_ratio))

