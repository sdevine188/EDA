library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(readr)

# disable scientific notation
options(scipen=999)

setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")

setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))


##########################################################


# reference appropriation codes and special initiative codes save here:
# G:\Shared\EDA ALL INFORMATION\Grant Processing Information\Grant Information Management Training and Best Practices\OPCS User Manual and Data Dictionary

# check for PRB 1 - exports and fdi
shiny %>% filter(Status == "Approved", FY == "2017", grepl("20|70", Appr.Code, ignore.case = TRUE),
                 grepl("export|fdi|foreign", General_Descr, ignore.case = TRUE) |
                         grepl("export|fdi|foreign", Project.Short.Descrip, ignore.case = TRUE) |
                         grepl("export|fdi|foreign", GNS_Descr, ignore.case = TRUE) |
                         grepl("export|fdi|foreign", Scope_Of_Work, ignore.case = TRUE) |
                         grepl("export|fdi|foreign", oea_description, ignore.case = TRUE),
                 !grepl("27|28", Initiatives, ignore.case = TRUE)) %>% group_by(Region.Name) %>% 
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))


################################################################


# check for PRB 2 - Adv. manufacturing and innovation
shiny %>% filter(Status == "Approved", FY == "2017", grepl("20|70", Appr.Code, ignore.case = TRUE),
                        grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                        General_Descr, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               Project.Short.Descrip, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               GNS_Descr, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               Scope_Of_Work, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               oea_description, ignore.case = TRUE),
                 !grepl("AM|WT|03|24|26|29|32|33|34", Initiatives, ignore.case = TRUE)) %>% group_by(Region.Name) %>% 
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))


############################################


# check for PRB 3 - resiliency
shiny %>% filter(Status == "Approved", FY == "2017", grepl("20|70", Appr.Code, ignore.case = TRUE),
                 grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                       General_Descr, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               Project.Short.Descrip, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               GNS_Descr, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               Scope_Of_Work, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               oea_description, ignore.case = TRUE),
                 !grepl("PM", Initiatives, ignore.case = TRUE)) %>% group_by(Region.Name) %>% 
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))


###############################################


# check for PRB 6 - CEDS w/ Resiliency
shiny %>% filter(Status == "Approved", FY == "2017", grepl("40", Appr.Code, ignore.case = TRUE),
                 grepl("Partnership Planning", Appr.Desc, ignore.case = TRUE) | 
                         grepl("Partnership Planning", Prog.Tool.Name, ignore.case = TRUE),
                 grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                       General_Descr, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                               Project.Short.Descrip, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                               GNS_Descr, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                               Scope_Of_Work, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                               oea_description, ignore.case = TRUE),
                 !grepl("PM", Initiatives, ignore.case = TRUE)) %>% group_by(Region.Name) %>% 
        summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE))


################################################


# function to return only columns with keywords, to make manual review of keywords easier
# also saved as "find_keyword_matches.R" in R Helper Scripts folder here:
# G:\PNP\Performance Measurement\R Helper Scripts
find_keyword_matches <- function(dataframe, keyword_string, keep_columns = c("Control.No.")){
        keyword_matches <- data.frame(matrix(nrow = nrow(dataframe), ncol = ncol(dataframe)))
        names(keyword_matches) <- names(dataframe)
        drop_columns <- c()
        for(col in 1:ncol(dataframe)){
                column_values <- c()
                if(names(dataframe)[col] %in% keep_columns) {
                        column_values <- dataframe[ , col]
                        keyword_matches[ , col] <- column_values
                        next
                }
                if(!(names(dataframe)[col] %in% keep_columns)) {
                        for(row in 1:nrow(dataframe)){
                                if(grepl(keyword_string, dataframe[row, col], ignore.case = TRUE)){
                                        column_values <- c(column_values, as.character(dataframe[row, col]))
                                } else {
                                        column_values <- c(column_values, as.character(""))
                                }
                        }
                        if(length(which(column_values == "")) == nrow(dataframe)) {
                                drop_columns <- c(drop_columns, col)
                        }
                        keyword_matches[ , col] <- column_values
                }
        }
        keyword_matches <- keyword_matches[ , -drop_columns]
        return(keyword_matches)
}


######################################################


# write PRB2 keyword hits to file for manual review

# check for PRB 2 - Adv. manufacturing and innovation
prb2_awards <- shiny %>% filter(Status == "Approved", FY == "2017", grepl("20|70", Appr.Code, ignore.case = TRUE),
                 grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                       General_Descr, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               Project.Short.Descrip, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               GNS_Descr, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               Scope_Of_Work, ignore.case = TRUE) |
                         grepl("manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator", 
                               oea_description, ignore.case = TRUE),
                 !grepl("AM|WT|03|24|26|29|32|33|34", Initiatives, ignore.case = TRUE))

keyword_string <- "manufactur|training|workforce|labor force|innovation|commercialization|incubator|accelerator"

prb2_keywords <- find_keyword_matches(prb2_awards, keyword_string, 
                keep_columns = c("Control.No.", "Region.Name", "Status", "FY", "Initiatives", "Proj.Short.Descrip"))

write_csv(prb2_keywords, "prb2_keywords.csv")


#########################################################


# write PRB3 keyword hits to file for manual review

# check for PRB 3 - resiliency
prb3_awards <- shiny %>% filter(Status == "Approved", FY == "2017", grepl("20|70", Appr.Code, ignore.case = TRUE),
                 grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                       General_Descr, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               Project.Short.Descrip, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               GNS_Descr, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               Scope_Of_Work, ignore.case = TRUE) |
                         grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development", 
                               oea_description, ignore.case = TRUE),
                 !grepl("PM", Initiatives, ignore.case = TRUE))

keyword_string <- "resilienc|disaster|storm|flood|hurricane|tornado|diversif|CEDS|comprehensive economic development"

prb3_keywords <- find_keyword_matches(prb3_awards, keyword_string, 
                keep_columns = c("Control.No.", "Region.Name", "Status", "FY", "Initiatives", "Proj.Short.Descrip"))

write_csv(prb3_keywords, "prb3_keywords.csv")


######################################################3


# write PRB6 keyword hits to file for manual review

# check for PRB 6 - CEDS w/ Resiliency
prb6_awards <- shiny %>% filter(Status == "Approved", FY == "2017", grepl("40", Appr.Code, ignore.case = TRUE),
                                grepl("Partnership Planning", Appr.Desc, ignore.case = TRUE) | 
                                        grepl("Partnership Planning", Prog.Tool.Name, ignore.case = TRUE),
                                grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                                      General_Descr, ignore.case = TRUE) |
                                        grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                                              Project.Short.Descrip, ignore.case = TRUE) |
                                        grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                                              GNS_Descr, ignore.case = TRUE) |
                                        grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                                              Scope_Of_Work, ignore.case = TRUE) |
                                        grepl("resilienc|disaster|storm|flood|hurricane|tornado|diversif", 
                                              oea_description, ignore.case = TRUE),
                                !grepl("PM", Initiatives, ignore.case = TRUE))

keyword_string <- "resilienc|disaster|storm|flood|hurricane|tornado|diversif"

prb6_keywords <- find_keyword_matches(prb6_awards, keyword_string, 
                        keep_columns = c("Control.No.", "Region.Name", "Status", "FY", "Initiatives", "Proj.Short.Descrip"))

write_csv(prb6_keywords, "prb6_keywords.csv")
