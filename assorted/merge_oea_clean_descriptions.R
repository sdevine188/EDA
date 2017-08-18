# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/OEA Award Announcements")

# read in cleaned data with oea descriptions
# note read.csv must be used because read_csv chokes on multibyte string
oea <- read.csv("oea_clean_data_fy11-16.csv", colClasses = c("Project.No." = "character"))
glimpse(oea)

# need to re-pad project_no with leading zeros for opcs awards 
unique(nchar(oea$Project.No.))
oea %>% filter(nchar(Project.No.) == 8) %>% select(Project.No.) %>% head(.)
oea %>% filter(nchar(Project.No.) == 9) %>% select(Project.No.) %>% head(.)
oea %>% filter(nchar(Project.No.) == 10) %>% select(Project.No.) %>% head(.)
oea %>% filter(nchar(Project.No.) == 11) %>% select(Project.No.) %>% head(.)
oea %>% filter(nchar(Project.No.) == 14) %>% select(Project.No.) %>% head(.)

# rstudio flags code as error because it wants to add a closing parenthesis, but it's not needed
oea <- oea %>% 
        mutate(Project.No. = case_when(nchar(.$Project.No.) == 8 ~ str_pad(.$Project.No., width = 9, side = "left", pad = "0"),
                                       nchar(.$Project.No.) == 10 ~ str_pad(.$Project.No., width = 11, side = "left", pad = "0"),
                                       TRUE ~ .$Project.No.))
unique(nchar(oea$Project.No.))

# drop duplicate project_no so that left_join doesn't produce duplicates
sum(duplicated(oea$Project.No.))
dup_index <- which(duplicated(oea$Project.No.) == "TRUE")
oea2 <- oea[-dup_index, ]
sum(duplicated(oea2$Project.No.))
dim(oea)
dim(oea2)

head(oea2)

# select oea variables
oea2 <- oea2 %>% rename(award_num = Project.No.) %>% 
        select(award_num, Applicant, EDA.Program, Press.Release.Project.Description)

# combine oea data with other data of interest 
other_data <- shiny
new_data <- left_join(other_data, oea2, by = c("Project.No." = "award_num"))

# check how many opcs/gol awards could not be matched with oea info
dim(new_data)
sum(!is.na(new_data$Press.Release.Project.Description))

# note that oea data will likely match multiple rows where project.no. is duplicated
sum(duplicated(new_data$Project.No.))
sum(duplicated(new_data$Control.No.))

                      
