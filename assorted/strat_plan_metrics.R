library(dplyr)
library(readr)
library(stringr)
library(scales)
library(ggplot2)

options(scipen=999)


# load shiny shiny locally
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]

shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(), 
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))
names(shiny)

# setwd
setwd("G:/PNP/Performance Measurement/DOC Strategic Plan/Strategic Plan Metrics/FY 2016")


# so 1.2
so_1.2 <- shiny %>% filter(Status == "Approved", FY == "2016", grepl("27", Initiatives, ignore.case = TRUE))%>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)), jc = sum(Est.Jobs.Created, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                  jcr = comma(sum(jc, js, na.rm = TRUE)), jcr_ratio = (sum(EDA.Funding, na.rm = TRUE) / sum(jc, js, na.rm = TRUE)), pi = dollar(sum(Est.Private.Investment, na.rm = TRUE)), 
                  pi_ratio = (sum(Est.Private.Investment, na.rm = TRUE) / sum(EDA.Funding, na.rm = TRUE)))

# so 1.3
so_1.3 <- shiny %>% filter(Status == "Approved", FY == "2016", grepl("28", Initiatives, ignore.case = TRUE)) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)), jc = sum(Est.Jobs.Created, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                  jcr = comma(sum(jc, js, na.rm = TRUE)), jcr_ratio = (sum(EDA.Funding, na.rm = TRUE) / sum(jc, js, na.rm = TRUE)), pi = dollar(sum(Est.Private.Investment, na.rm = TRUE)), 
                  pi_ratio = (sum(Est.Private.Investment, na.rm = TRUE) / sum(EDA.Funding, na.rm = TRUE)))

# outlier check - no issues
shiny %>% filter(Status == "Approved", FY == "2016", grepl("28", Initiatives, ignore.case = TRUE)) %>% ggplot(., aes(x = Est.Jobs.Created)) + geom_histogram()
shiny %>% filter(Status == "Approved", FY == "2016", grepl("28", Initiatives, ignore.case = TRUE)) %>% ggplot(., aes(x = Est.Jobs.Saved)) + geom_histogram()
shiny %>% filter(Status == "Approved", FY == "2016", grepl("28", Initiatives, ignore.case = TRUE)) %>% ggplot(., aes(x = Est.Private.Investment)) + geom_histogram() +
        scale_x_continuous(label = dollar)

# so 2.3 skills
so_2.3_skills <- shiny %>% filter(Status == "Approved", FY == "2016", grepl("WT", Initiatives, ignore.case = TRUE)) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)), jc = sum(Est.Jobs.Created, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                  jcr = comma(sum(jc, js, na.rm = TRUE)), jcr_ratio = (sum(EDA.Funding, na.rm = TRUE) / sum(jc, js, na.rm = TRUE)), pi = dollar(sum(Est.Private.Investment, na.rm = TRUE)), 
                  pi_ratio = (sum(Est.Private.Investment, na.rm = TRUE) / sum(EDA.Funding, na.rm = TRUE)))

# outlier check - no issues
shiny %>% filter(Status == "Approved", FY == "2016", grepl("WT", Initiatives, ignore.case = TRUE)) %>% ggplot(., aes(x = Est.Jobs.Created)) + geom_histogram()
shiny %>% filter(Status == "Approved", FY == "2016", grepl("WT", Initiatives, ignore.case = TRUE)) %>% ggplot(., aes(x = Est.Jobs.Saved)) + geom_histogram()
shiny %>% filter(Status == "Approved", FY == "2016", grepl("WT", Initiatives, ignore.case = TRUE)) %>% ggplot(., aes(x = Est.Private.Investment)) + geom_histogram() +
        scale_x_continuous(label = dollar)

# so 2.3 innovation
so_2.3_innovation <- shiny %>% filter(Status == "Approved", FY == "2016", grepl("03|24|26|29|32|33|34", Initiatives, ignore.case = TRUE)) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)), jc = sum(Est.Jobs.Created, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                  jcr = comma(sum(jc, js, na.rm = TRUE)), jcr_ratio = (sum(EDA.Funding, na.rm = TRUE) / sum(jc, js, na.rm = TRUE)), pi = dollar(sum(Est.Private.Investment, na.rm = TRUE)), 
                  pi_ratio = (sum(Est.Private.Investment, na.rm = TRUE) / sum(EDA.Funding, na.rm = TRUE)))

# so 3.3
so_3.3 <- shiny %>% filter(Status == "Approved", FY == "2016", grepl("PM", Initiatives, ignore.case = TRUE)) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)), jc = sum(Est.Jobs.Created, na.rm = TRUE), js = sum(Est.Jobs.Saved, na.rm = TRUE), 
                  jcr = comma(sum(jc, js, na.rm = TRUE)), jcr_ratio = (sum(EDA.Funding, na.rm = TRUE) / sum(jc, js, na.rm = TRUE)), pi = dollar(sum(Est.Private.Investment, na.rm = TRUE)), 
                  pi_ratio = (sum(Est.Private.Investment, na.rm = TRUE) / sum(EDA.Funding, na.rm = TRUE)))

# write output
write_csv(so_1.2, "so_1.2.csv")
write_csv(so_1.3, "so_1.3.csv")
write_csv(so_2.3_skills, "so_2.3_skills.csv")
write_csv(so_2.3_innovation, "so_2.3_innovation.csv")
write_csv(so_3.3, "so_3.3.csv")
