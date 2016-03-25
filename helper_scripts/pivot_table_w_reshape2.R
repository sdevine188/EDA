library(dplyr)
library(stringr)
library(reshape2)

setwd("G:/PNP/Performance Measurement/master_data")
datafilename <- list.files()[str_detect(list.files(), "master_data_20")]

md <- read.csv(datafilename, stringsAsFactors = FALSE) 

# better way to make a pivot table is with reshape2 package using melt and dcast functions
# the tidyr function doesn't seem to be able to cleanly swing multiple columns


# create pivot table of post-2000, approved project count and dollar amount for each state by year
md2 <- md %>%
        filter(FY > 2000, Status == "Approved") %>%
        group_by(Appl.State.Abbr, FY) %>%
        summarize(
                project_count = n(),
                total_funding = sum(Best.EDA..)
                ) %>%
        melt(id.vars = c("Appl.State.Abbr", "FY")) %>%
        dcast(Appl.State.Abbr ~ FY + variable)

View(md2)
