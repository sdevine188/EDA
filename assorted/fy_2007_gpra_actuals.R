library(stringr)
library(readr)
library(dplyr)
library(stringr)
library(scales)
library(ggplot2)

options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(), 
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# convert jobs/pi actuals to numeric
shiny$Jobs.Created.at.6.years <- as.numeric(shiny$Jobs.Created.at.6.years)
shiny$Jobs.Retained.at.6.years <- as.numeric(shiny$Jobs.Retained.at.6.years)
shiny$Priv.Inv.at.6.years <- as.numeric(shiny$Priv.Inv.at.6.years)

shiny$Jobs.Created.at.9.years <- as.numeric(shiny$Jobs.Created.at.9.years)
shiny$Jobs.Retained.at.9.years <- as.numeric(shiny$Jobs.Retained.at.9.years)
shiny$Priv.Inv.at.9.years <- as.numeric(shiny$Priv.Inv.at.9.years)


################################################################


# reported fy 2007 9 year actuals

# with outliers
jc9_2007 <- shiny %>% filter(Status == "Approved", FY == "2007", Construction %in% c("C", "B")) %>% 
        select(Jobs.Created.at.9.years)
jc9_2007 %>% summarize(jc = sum(Jobs.Created.at.9.years, na.rm = TRUE))

js9_2007 <- shiny %>% filter(Status == "Approved", FY == "2007", Construction %in% c("C", "B")) %>% 
        select(Jobs.Retained.at.9.years)
js9_2007 %>% summarize(js = sum(Jobs.Retained.at.9.years, na.rm = TRUE))

pi9_2007 <- shiny %>% filter(Status == "Approved", FY == "2007", Construction %in% c("C", "B")) %>% 
        select(Priv.Inv.at.9.years)
pi9_2007 %>% summarize(pi = dollar(sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE)))

####################################################

# these were numbers used for final GPRA report
# 25% discount
# remove one outlier for PI (see below)
(24016 + 5945) * .75 # 22471
3501113467 * .75 # 2625835100

########################################################

# histograms
ggplot(jc9_2007, aes(x = Jobs.Created.at.9.years)) + geom_histogram(binwidth = 100) 

ggplot(js9_2007, aes(x = Jobs.Retained.at.9.years)) + geom_histogram(binwidth = 100) 

ggplot(pi9_2007, aes(x = Priv.Inv.at.9.years)) + geom_histogram() 


# without outliers

# 1 outlier around 12500
jc9_2007 %>% filter(Jobs.Created.at.9.years < 12000) %>% summarize(jc = sum(Jobs.Created.at.9.years, na.rm = TRUE))

js9_2007 %>% summarize(js = sum(Jobs.Retained.at.9.years, na.rm = TRUE))

# one outlier > 3 bil - this number used for final report
pi9_2007 %>% filter(Priv.Inv.at.9.years < 3000000000) %>%
        summarize(pi = dollar(sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE)))


# pi and jcr ratios
# eda construction funding in fy 2007
# consistent with 2007 annual report
shiny %>% filter(Status == "Approved", FY == "2007", Construction %in% c("C", "B")) %>% 
        summarize(amount = sum(EDA.Funding, na.rm = TRUE)) # 190426104

shiny %>% filter(Status == "Approved", FY == "2007", Construction %in% c("C", "B")) %>% group_by(Appr.Desc) %>%
        summarize(amount = sum(EDA.Funding, na.rm = TRUE))

shiny %>% filter(Status == "Approved", FY == "2007", Construction %in% c("C", "B")) %>% group_by(Appr.Desc) %>%
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE), jc9 = sum(Jobs.Created.at.9.years, na.rm = TRUE), 
                  js9 = sum(Jobs.Retained.at.9.years, na.rm = TRUE), pi9 = sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE))

# pi ratio
pi9_2007 %>% filter(Priv.Inv.at.9.years < 3000000000) %>%
        summarize(pi = dollar(sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE)))
3501113467 * .75 # 2625835100
2625835100 / 190426104 # 13.9

# jcr ratio
jc9_2007 %>% summarize(jc = sum(Jobs.Created.at.9.years, na.rm = TRUE))
js9_2007 %>% summarize(js = sum(Jobs.Retained.at.9.years, na.rm = TRUE))

(24016 + 5945) * .75 # 22471
190426104 / 22471 # 8474


######################################################################################




