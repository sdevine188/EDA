library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(scales)

options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(), 
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# calculate unmet demand for fy 12-16, defined as projects that have PRD date from IRC (First_FALD_Date in GOL is mapped to PRD date), but were not Approved
# these are considered high-quality, since they passed merit and technical review to get to the IRC stage

# PRD Action Codes:
# 1	Carryover - Highly Competitive
# 2	Carryover - Competitive
# G	Continue Processing
# H	Hold
# W	Withdrawn
# X	Discontinue Processing
# T	Target Date

# all records that met tech and merit review and so were issued PRD dates

# summarize total high-quality projects
shiny %>% filter(!is.na(PRD.Date), FY > 2011, FY < 2017) %>% group_by(Status) %>% tally()
shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>% tally()

# total <- shiny %>% filter(!is.na(PRD.Date), FY > 2011, FY < 2017) %>%
#         summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))
# 
# met <- shiny %>% filter(!is.na(PRD.Date), Status == "Approved", FY > 2011, FY < 2017) %>%
#         summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))
# 
# unmet <- shiny %>% filter(!is.na(PRD.Date), Status != "Approved", FY > 2011, FY < 2017) %>%
#         summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))

# need to get count/amount of awards seperately, then add to count/amount of non-awards with PRD date
# because some awards are missing PRD date

# check awards missing PRD date
shiny %>% filter(Status == "Approved", is.na(PRD.Date), FY > 2011, FY < 2017) %>% select(Project.No., Status, FY, PRD.Date, database) %>% group_by(database) %>% tally()
shiny %>% filter(Status == "Approved", is.na(PRD.Date), FY > 2011, FY < 2017) %>% select(Project.No., Status, FY, PRD.Date, database) 
shiny %>% filter(Status == "Approved", is.na(PRD.Date), FY > 2011, FY < 2017) %>% tally()

total_awards <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>%
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))

total_prd_nonawards <- shiny %>% filter(Status != "Approved", !is.na(PRD.Date), FY > 2011, FY < 2017) %>%
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))

total <- data.frame(count = total_awards$count + total_prd_nonawards$count, amount = total_awards$amount + total_prd_nonawards$amount)

met <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>%
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))

unmet <- shiny %>% filter(!is.na(PRD.Date), Status != "Approved", FY > 2011, FY < 2017) %>%
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))


# combine into table and get pct of high quality applications funded vs not funded
table <- rbind(met, unmet) %>% mutate(pct_count = count / sum(count), pct_funding = amount / sum(amount))
table$name <- c("met", "unmet")
table
total

# check sum of met and unmet equals totals
table %>% select_if(is.numeric) %>% summarize_each(funs(sum))


##########################################################


# just as a check, re-calculate unmet demand without high-quality PRD.Date requirement

total_unrestricted <- shiny %>% filter(FY > 2011, FY < 2017) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

met_unrestricted <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>% 
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))

unmet_unrestricted <- shiny %>% filter(Status != "Approved", FY > 2011, FY < 2017) %>% 
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))

# combine into table and get pct of high quality applications funded vs not funded
table_unrestricted <- rbind(met_unrestricted, unmet_unrestricted) %>% 
        mutate(pct_count = count / sum(count), pct_funding = amount / sum(amount))

table_unrestricted$name <- c("met_unrestricted", "unmet_unrestricted")
table_unrestricted
total_unrestricted

# check sum of met and unmet equals totals
table_unrestricted %>% select_if(is.numeric) %>% summarize_each(funs(sum))
total_unrestricted
