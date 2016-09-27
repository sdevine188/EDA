library(readr)
library(stringr)
library(dplyr)
library(ggplot2)


setwd("G:/PNP/Performance Measurement/master_data")

coapp <- read.csv("allapplications_with_coapplicants_20160922.csv", stringsAsFactors = FALSE, colClasses = c("Control." = "character",
                          "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                          "Initiatives" = "character", "Appl.Contact.Name" = "character", "Contact.Email" = "character", "DUNS.." = "character", 
                          "Local.Applicant.." = "character", "Total.Project.." = "character", "Best.EDA.." = "character", "Private.Investment" = "character"))

opcs <- read.csv("opcs_20160922.csv", stringsAsFactors = FALSE, colClasses = c("Control." = "character",
                         "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                         "Initiatives" = "character", "Appl.Contact.Name" = "character", "Contact.Email" = "character", "DUNS.." = "character", 
                         "Local.Applicant.." = "character", "Total.Project.." = "character", "Best.EDA.." = "character", "Private.Investment" = "character"))

dim(coapp)
dim(opcs)

# confirm coapp does contain coapplicants
opcs %>% group_by(Appl.Lead..Y.N.) %>% tally() %>% data.frame(.)
coapp %>% group_by(Appl.Lead..Y.N.) %>% tally() %>% data.frame(.)

# create id variable
opcs2 <- opcs %>% mutate(id = str_c(Control., Appl.Short.Name, sep = "_"))
coapp2 <- coapp %>% mutate(id = str_c(Control., Appl.Short.Name, sep = "_"))

head(opcs2$id)
head(coapp2$id)

# find distinct id to identify applicants and coapplicants
# same count of distinct opcs if use appl.short.name or duns as third match point; but coapp is 25760 with appl name, and 25667 with duns
# coapp is 25670 if just control and appl name
opcs_dist <- opcs2 %>% distinct(id, .keep_all = TRUE)
coapp_dist <- coapp2 %>% distinct(id, .keep_all = TRUE)

dim(opcs_dist)
dim(coapp_dist)
dim(shiny)

# inspect for missing or blank DUNS
sum(is.na(opcs_dist$DUNS..))
sum(is.na(coapp_dist$DUNS..))
length(which(opcs_dist$DUNS.. == ""))
length(which(coapp_dist$DUNS.. == ""))

opcs_duns <- opcs_dist[which(opcs_dist$DUNS.. == ""), ]
coapp_duns <- coapp_dist[which(coapp_dist$DUNS.. == ""), ]
opcs_duns %>% group_by(FY) %>% tally()
coapp_duns %>% group_by(FY) %>% tally()

# view comp.codes in id 
# if three point match
# unique(sapply(opcs_dist$id, function(x) { str_sub(x, start = str_locate_all(x, "_")[[1]][1], end = str_locate_all(x, "_")[[1]][2]) }))
# unique(sapply(coapp_dist$id, function(x) { str_sub(x, start = str_locate_all(x, "_")[[1]][1], end = str_locate_all(x, "_")[[1]][2]) }))

# find records in coapp_dist not in opcs_dist
new_index <- which(!(coapp_dist$id %in% opcs_dist$id))
length(new_index)

new_coapp <- coapp_dist[new_index, ]
dim(new_coapp)
unique(new_coapp$Comp.Code)
new_coapp %>% group_by(Appl.Lead..Y.N.) %>% tally() %>% data.frame(.)

# some in new_coapp are listed as lead applicant; confirm not in opcs_dist
new_coapp %>% filter(Appl.Lead..Y.N. == "Y") %>% select(id, FY, Control., Comp.Code, Appl.Lead..Y.N., Appl.Short.Name, DUNS.., IRS.., Minority.Status) %>% head(.)
check_id <- new_coapp %>% filter(Appl.Lead..Y.N. == "Y") %>% select(id)

which(check_id$id %in% new_coapp$id)
which(check_id$id %in% opcs2$id)

opcs2 %>% filter(Control. == "109298") %>% select(id, Control., Appl.Lead..Y.N., Comp.Code, Appl.Short.Name)
unique(opcs2$Comp.Code)
unique(opcs2$Appl.Lead..Y.N.)

# inspect distribution of fy
new_coapp_fy <- new_coapp %>% group_by(FY) %>% tally() %>% data.frame(.)
ggplot(new_coapp_fy, aes(x = FY, y = n)) + geom_bar(stat = "identity")

new_coapp_fy <- new_coapp %>% group_by(FY, Region.Name) %>% tally() %>% data.frame(.)
ggplot(new_coapp_fy, aes(x = FY, y = n, group = Region.Name, color = Region.Name)) + geom_line() + geom_point()
new_coapp_fy %>% filter(Region.Name %in% c("Seattle", "Philadelphia", "Denver")) %>% ggplot(., aes(x = FY, y = n, group = Region.Name, color = Region.Name)) + geom_line() + geom_point()

# inspect count of coapps per control.
new_coapp %>% group_by(Control.) %>% tally() %>% summarize(avg_count = mean(n), max = max(n), median = median(n))
new_coapp %>% group_by(Control.) %>% tally() %>% group_by(n) %>% summarize(count = n())
new_coapp %>% group_by(Control.) %>% tally() %>% filter(n == 8) 

# n = 12
opcs_dist %>% filter(Control. == "59399") %>% select(FY, Control., Comp.Code, Appl.Short.Name, DUNS.., IRS.., Appl.Street.Addr.1)
coapp_dist %>% filter(Control. == "59399") %>% select(FY, Control., Comp.Code, Appl.Short.Name, DUNS.., IRS.., Appl.Street.Addr.1)

# n = 8
opcs_dist %>% filter(Control. == "59398") %>% select(FY, Control., Comp.Code, Appl.Short.Name, DUNS.., IRS.., Appl.Street.Addr.1)
coapp_dist %>% filter(Control. == "59398") %>% select(FY, Control., Comp.Code, Appl.Short.Name, DUNS.., IRS.., Appl.Street.Addr.1)

# inspect for missing or blank DUNS
sum(is.na(new_coapp$DUNS..))
length(which(new_coapp$DUNS.. == ""))

coapp_no_duns <- new_coapp[which(new_coapp$DUNS.. == ""), ]
coapp_no_duns %>% group_by(FY) %>% tally()

length(which(new_coapp$DUNS.. != ""))
coapp_duns <- new_coapp[which(new_coapp$DUNS.. != ""), ]
coapp_duns %>% group_by(FY) %>% tally()


