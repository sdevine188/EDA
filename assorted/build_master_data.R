library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(lazyeval)
library(readxl)
library(scales)
library(noncensus)

# setwd
setwd("G:/PNP/Performance Measurement/master_data")

## read in and clean opcs data

# turn-off scientific notation
options(scipen=999)

# find current version of option1 file with oit unqueryable/truncated fields
# opcs file is saved as csv directly from impromptu
opcs_filename <- list.files()[str_detect(list.files(), "opcs_impromptu")]

# read-in opcs data from impromptu
opcs <- read.csv(opcs_filename, stringsAsFactors = FALSE, colClasses = c("Control." = "character",
                                                                         "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                                                                         "Initiatives" = "character", "Appl.Contact.Name" = "character", "Contact.Email" = "character", "DUNS.." = "character", "IRS.." = "character",
                                                                         "Local.Applicant.." = "numeric", "Total.Project.." = "character", "Best.EDA.." = "numeric", "Private.Investment" = "numeric"), na.strings = c("", "NA"))

# glimpse(opcs)

opcs2 <- opcs


##########################################


# pad variables with leading zeroes

# pad congressional districts
# sum(is.na(opcs2$Appl.Cong.Dist))
# unique(nchar(opcs2$Appl.Cong.Dist))
opcs2$Appl.Cong.Dist <- str_pad(opcs2$Appl.Cong.Dist, width = 2, side = "left", pad = "0")
# sum(is.na(opcs2$Appl.Cong.Dist))
# unique(nchar(opcs2$Appl.Cong.Dist))
opcs2$Proj.Cong.Dist <- str_pad(opcs2$Proj.Cong.Dist, width = 2, side = "left", pad = "0")

# pad appl.fips
opcs2$Appl.FIPS.ST <- str_pad(opcs2$Appl.FIPS.ST, width = 2, side = "left", pad = "0")
opcs2$Appl.FIPS.Cnty <- str_pad(opcs2$Appl.FIPS.ST, width = 3, side = "left", pad = "0")

# pad DUNS with zeroes
errors_index <- which(nchar(opcs2$DUNS..) < 9 & nchar(opcs2$DUNS..) > 0)
opcs2$DUNS..[errors_index] <- str_pad(opcs2$DUNS..[errors_index], width = 9, side = "right", pad = "0")

# pad leading zeroes back on zip codes
opcs2$Appl..Zip <- str_pad(opcs2$Appl..Zip, width = 5, side = "left", pad = "0")
opcs2$Proj.ZIP <- str_pad(opcs2$Proj.ZIP, width = 5, side = "left", pad = "0")
opcs2$Appl.ZIP.4 <- str_pad(opcs2$Appl.ZIP.4, width = 4, side = "left", pad = "0")


#################################################


# convert MSI.Indicator code into acronym
opcs2$MSI.Indicator <- as.character(opcs2$MSI.Indicator)
for(i in 1:nrow(opcs2)) {
        if(opcs2$MSI.Indicator[i] == "1" & !is.na(opcs2$MSI.Indicator[i])) {
                opcs2$MSI.Indicator[i] <- "HBCU"
        }
        if(opcs2$MSI.Indicator[i] == "2" & !is.na(opcs2$MSI.Indicator[i])) {
                opcs2$MSI.Indicator[i] <- "HSI"
        }
        if(opcs2$MSI.Indicator[i] == "3" & !is.na(opcs2$MSI.Indicator[i])) {
                opcs2$MSI.Indicator[i] <- "TCU"
        }
        if(opcs2$MSI.Indicator[i] == "4" & !is.na(opcs2$MSI.Indicator[i])) {
                opcs2$MSI.Indicator[i] <- "Other"
        }
        if(opcs2$MSI.Indicator[i] == "5" & !is.na(opcs2$MSI.Indicator[i])) {
                opcs2$MSI.Indicator[i] <- "AKHIPPI"
        }
}


######################################################


# create coapplicant variables

# create id variable and get distinct id - still will be some remaining duplicates due to coapplicants
opcs2 <- opcs2 %>% mutate(id = str_c(str_replace_na(opcs2$Control.), str_replace_na(opcs2$Appl.Short.Name), sep = "_")) %>%
        distinct(id, .keep_all = TRUE)
row_id <- data.frame(row_id = seq_along(1:nrow(opcs2)))
opcs2 <- bind_cols(row_id, opcs2)

# flag coapplicants
dup_control_index <- duplicated(opcs2$Control.)
dup_control_numbers <- opcs2$Control.[dup_control_index]
total_dup_records <- opcs2 %>% filter(Control. %in% dup_control_numbers)
# create value for test to confirm no control numbers being dropped with filter for comp.code = 1, appl.lead = y, and is.na(appl.short.name)
total_dup_count <- length(unique(total_dup_records$Control.))
# filter total_dup_records to find lead applicants
dup_lead_app_records <- total_dup_records %>% filter(Comp.Code == "1" & Appl.Lead..Y.N. == "Y") %>% filter(!is.na(Appl.Short.Name))
# manually add lead applicant records for any found via inspection that don't have both comp.code = 1 and appl.lead = Y due to data entry errors
manual_lead <- opcs2 %>% filter(Control. == 59400 & Appl.Short.Name == "Simi Valley, City of" |
                                        Control. == 59749 & Appl.Short.Name == "American Samoa Govt of")
dup_lead_app_records <- bind_rows(dup_lead_app_records, manual_lead)
# run test to confim no untracked duplicates requiring manual handling
lead_app_count <- length(unique(dup_lead_app_records$Control.))
if(total_dup_count == lead_app_count) {
        print("good: no untracked duplicates")
} else {
        stop("untracked duplicates detected: total_dup_count is greater than dup_control_count, so some records must be manually handled")
}

# if you get error above for untracked duplicates where comp.code !=1 and appl.lead != Y, need to inspect and manually add them 
# untracked_dup_index <- which(!(unique(total_dup_records$Control.) %in% unique(dup_lead_app_records$Control.)))
# untracked_dup_control_no <- unique(total_dup_records$Control.)[untracked_dup_index]
# untracked_dups <- total_dup_records %>% filter(Control. %in% untracked_dup_control_no)
# untracked_dups %>% select(Control., Appl.Short.Name, Full.Applicant.Name, Appl.Lead..Y.N., Comp.Code)
# total_dup_records %>% filter(Control. %in% untracked_dup_control_no) %>%
#         select(Control., Appl.Short.Name, Appl.Lead..Y.N., Comp.Code) %>% arrange(Control.)

# flag coapplicant records
opcs2$coapp <- NA
coapp_records <- opcs2 %>% filter(Control. %in% dup_lead_app_records$Control., !(row_id %in% dup_lead_app_records$row_id))
coapp_rows <- coapp_records$row_id
# opcs2[ , "coapp"][coapp_rows] <- 1
coapp_col <- opcs2$coapp
coapp_col[coapp_rows] <- 1
opcs2$coapp <- coapp_col
# test to confirm records flagged by row_id order are the originally identified dups
coapp_flag <- opcs2 %>% filter(coapp == 1)
if(sum(coapp_flag$Control. %in% dup_control_numbers) != nrow(coapp_flag)) {
        stop("Some flagged coapplicants have control numbers not in the originally identified duplicates")
} else {
        print("Good: coapplicants flagged correctly")
}

# create function to produce coapplicant variable and assign it as compiled string in lead applicant record
create_coapp_var <- function(var) {
        var_name <- str_c("Coapp.", var)
        opcs2[ , var_name] <- NA
        dup_control_index <- duplicated(opcs2$Control.)
        dup_control_numbers <- opcs2$Control.[dup_control_index]
        dup_lead_app_records <- opcs2 %>% filter(Control. %in% dup_control_numbers, is.na(coapp))
        
        for(i in 1:nrow(dup_lead_app_records)) {
                # compile coapplicant variable into string and assign to lead applicant record
                coapp_records <- opcs2 %>% filter(Control. == dup_lead_app_records$Control.[i], row_id != dup_lead_app_records$row_id[i])
                coapp_var <- coapp_records %>% select_(.dots = lazyeval::interp(var))
                coapp_var_list <- sapply(coapp_var[ , lazyeval::interp(var)], function(x) { x })
                coapp_var_str <- str_c(coapp_var_list, collapse = ";; ")
                lead_app_row <- which(opcs2$row_id == dup_lead_app_records$row_id[i])
                opcs2[ , var_name][lead_app_row] <- coapp_var_str
        }
        
        opcs2
}

# create coapp variables
opcs2 <- create_coapp_var("Appl.Short.Name")
opcs2 <- create_coapp_var("Full.Applicant.Name")
opcs2 <- create_coapp_var("Appl.Street.Addr.1")
opcs2 <- create_coapp_var("Appl.Street.Addr.2")
opcs2 <- create_coapp_var("Appl.City.Name")
opcs2 <- create_coapp_var("Appl.FIPS.City")
opcs2 <- create_coapp_var("Appl.Cnty.Name")
opcs2 <- create_coapp_var("Appl.FIPS.Cnty")
opcs2 <- create_coapp_var("Appl.State.Abbr")
opcs2 <- create_coapp_var("Appl.FIPS.ST")
opcs2 <- create_coapp_var("Appl..Zip")
opcs2 <- create_coapp_var("Appl.ZIP.4")
opcs2 <- create_coapp_var("Appl.Type.Name")
opcs2 <- create_coapp_var("Appl.Type.Code")
opcs2 <- create_coapp_var("Appl.Contact.Name")
opcs2 <- create_coapp_var("Appl.Contact.Title")
opcs2 <- create_coapp_var("Appl.Cont.Phone")
opcs2 <- create_coapp_var("Entity.Code")
opcs2 <- create_coapp_var("MSI.Indicator")
opcs2 <- create_coapp_var("Minority.Status")
opcs2 <- create_coapp_var("DUNS..")
opcs2 <- create_coapp_var("IRS..")


##########################################################


# test that duplicates all have one funding amount
opcs_unique_amounts <- opcs2 %>% group_by(Project.No.) %>% summarize(unique_amounts = length(unique(Best.EDA..)))
# head(opcs_unique_amounts)
na_count <- opcs_unique_amounts %>% filter(is.na(Project.No.)) %>% select(unique_amounts)
if(unique(opcs_unique_amounts$unique_amounts)[1] == 1 && unique(opcs_unique_amounts$unique_amounts)[2] == na_count) {
        print("Good: Each Project Number has only one unique EDA.Funding amount")
} else {
        stop("Error: Some Project Numbers have more than one unique EDA.Funding amount - cannot drop duplicates")
}

# find and delete coapplicant records
opcs3 <- opcs2 %>% filter(is.na(coapp))

# delete coapp, row_ID, and id variables
opcs3 <- opcs3 %>% select(-c(coapp, id, row_id))

# test for duplicate Control Numbers
if(sum(duplicated(opcs3$Control_.)) == 0)  {
        print("Good: No duplicate Control Numbers detected")
} else {
        stop("Error: Duplicate Control Numbers detected")
}


###################################################################


# test to confirm that mgmt report has same count/amount as master_data opcs script
historic_mgmt_rep_filename <- list.files()[str_detect(list.files(), "historic_opcs_")]
historic_mgmt <- read.csv(historic_mgmt_rep_filename, stringsAsFactors = FALSE, 
                          colClasses = c("Control_." = "character"), na.strings = c("", "NA"))

current_mgmt_rep_filename <- list.files()[str_detect(list.files(), "current_opcs_")]
current_mgmt <- read.csv(current_mgmt_rep_filename, stringsAsFactors = FALSE, 
                         colClasses = c("Control_." = "character"), na.strings = c("", "NA"))

mgmt <- rbind(historic_mgmt, current_mgmt)
# glimpse(mgmt)

# test for duplicate Control Numbers
if(sum(duplicated(mgmt$Control_.)) == 0)  {
        print("Good: No duplicate Control Numbers detected")
} else {
        stop("Error: Duplicate Control Numbers detected")
}

# there should not be duplicates - inspect any duplicates found
dup_control <- mgmt$Control_.[which(duplicated(mgmt$Control_.) == "TRUE")]
mgmt %>% filter(Control_. %in% dup_control) %>% 
        select(Control_., Project_No, FY, Status, Best_EDA_., Full_Applicant_Name)

# known duplicate issues
# control_. 110988 is FY 2016 pending on historical_mgmt file, but FY 2017 on current mgmt report - must have switched FY - drop FY 2016 pending app
control_110988_index <- which(mgmt$Control_. == "110988")
mgmt[control_110988_index[1], ] %>% select(Control_., Project_No, FY, Status, Best_EDA_., Full_Applicant_Name)
mgmt <- mgmt[-control_110988_index[1], ]

# re-test for duplicate Control Numbers
if(sum(duplicated(mgmt$Control_.)) == 0)  {
        print("Good: No duplicate Control Numbers detected")
} else {
        stop("Error: Duplicate Control Numbers detected")
}

# trim Status 
mgmt$Status <- str_trim(mgmt$Status, side = "both")


############################################################


# compare overall count of awards and control numbers match
# note that mgmt_sum is filtered to exclude "awards" with funding == 0
# note that amount likely won't match because of de-obligations showing in fresh opcs_impromptu data, but not historic mgmt rep
mgmt_sum <- mgmt %>% filter(Status == "A", Best_EDA_. > 0) %>% summarize(count = n()) %>% data.frame(.)
mgmt_sum

# summary of opcs3
opcs_sum <- opcs3 %>% filter(Status == "Approved") %>% summarize(count = n()) %>% data.frame(.)
opcs_sum

if(mgmt_sum == opcs_sum) {
        print("Good: opcs3 impromptu report and management report have same the count of awards")
} else {
        stop("Error: opcs3 impromptu report and management report do not have the same count of awards")
}

# inspect if count in opcs3 and mgmt does not match
mgmt_awards <- mgmt %>% filter(Status == "A", Best_EDA_. > 0)
opcs3_awards <- opcs3 %>% filter(Status == "Approved")

# These awards are consistently missing:
#Control_. Project_No   FY Status Best_EDA_. Full_Applicant_Name
#360     108912   58605784 2015      A      40300     Garfield County
#3775     89279   67905667 2011      A     841307   City of Cleveland



missing_from_opcs_index <- which(!(mgmt_awards$Control_. %in% opcs3_awards$Control.))
mgmt_awards[missing_from_opcs_index, ] %>% select(Control_., Project_No, FY, Status, Best_EDA_., Full_Applicant_Name)

missing_from_mgmt_index <- which(!(opcs3_awards$Control. %in% mgmt_awards$Control_.))
opcs3_awards[missing_from_mgmt_index, ] %>% select(Control., Project.No., FY, Status, Best.EDA.., Full.Applicant.Name)


dim(mgmt_awards)
dim(opcs3_awards)

# opcs still has 24 pendinng applications



# if error, need to manually inspect missing_from_opcs/mgmt above
if(length(missing_from_opcs_index) == 0 && length(missing_from_mgmt_index) == 0) {
        print("Good: opcs3 impromptu report and management report have same Control Numbers")
} else {
        stop("Error: opcs3 impromptu report and management report do not have same Control Numbers")
}

# create list of missing awards to inspect
#This was written in previously 08/15/2017
#missing_awards <- str_c(mgmt_awards$Control_.[missing_from_opcs_index], opcs3_awards[missing_from_mgmt_index])
missing_awards <- str_c(mgmt_awards$Control_.[missing_from_opcs_index], opcs3_awards$Control.[missing_from_mgmt_index])

# known issues with count/amount matching
# control 108912, 89279 is in historic mgmt rep as Status = Approved, but in recent opcs_impromptu pull its Status = Cancelled After Decision
# these would update correctly if we refreshed historic mgmt rep - can ignore
mgmt %>% filter(Control_. %in% missing_awards) %>% select(Control_., FY, Status, Best_EDA_., Full_Applicant_Name)
opcs3 %>% filter(Control. %in% missing_awards) %>% select(Control., Status, FY, Best.EDA.., Full.Applicant.Name)

# confirm that count amount matches after accounting for known issues
known_issue_count <- length(missing_awards)
if((mgmt_sum$count - opcs_sum$count) == known_issue_count ) {
        print("Good: after accounting for known_issues, opcs3 impromptu report and management report have same the count of awards")
} else {
        stop("Error: after accounting for known_issues, opcs3 impromptu report and management report do not have the same count of awards")
}


###################################


# summary of mgmt apps
mgmt_app_sum <- mgmt %>% filter(Status != "A", Best_EDA_. > 0) %>% summarize(count = n()) %>% data.frame(.)
mgmt_app_sum

# summary of opcs3 apps
opcs_app_sum <- opcs3 %>% filter(Status != "Approved") %>% summarize(count = n()) %>% data.frame(.)
opcs_app_sum

if(mgmt_app_sum == opcs_app_sum) {
        print("Good: opcs3 impromptu report and management report have same the count of apps")
} else {
        stop("Error: opcs3 impromptu report and management report do not have the same count of apps")
}

# inspect if count in opcs3 and mgmt does not match
mgmt_apps <- mgmt %>% filter(Status != "A", Best_EDA_. > 0)
opcs3_apps <- opcs3 %>% filter(Status != "Approved")

dim(mgmt_apps)
length(unique(mgmt_apps$Control_.))

dim(opcs3_apps)
length(unique(opcs3_apps$Control.))

# test that opc3 and mgmt applications have the same Control Numbers
missing_from_opcs_apps <- mgmt_apps$Control_.[!(mgmt_apps$Control_. %in% opcs3_apps$Control.)] 
missing_from_mgmt_apps <- opcs3_apps$Control.[!(opcs3_apps$Control. %in% mgmt_apps$Control_.)] 

if(length(missing_from_opcs_apps) == 0 && length(missing_from_mgmt_apps) == 0) {
        print("Good: opcs3_apps and mgmt_apps have the same control numbers")
} else {
        stop("Error: opcs3_apps and mgmt_apps do not have the same control numbers")
}

# inspect missing apps
# in 20170707, mgmt_apps is missing 89279, 108912 (both in mgmt as Approved, so not in mgmt_apps), and 97174 (in mgmt as $0 funding, so not in mgmt_apps)
mgmt_apps %>% filter(Control_. %in% missing_from_opcs_apps) %>% select(Control_., FY, Status, Best_EDA_.)
mgmt_apps %>% filter(Control_. %in% missing_from_mgmt_apps) %>% select(Control_., FY, Status, Best_EDA_.)
mgmt %>% filter(Control_. %in% missing_from_mgmt_apps) %>% select(Control_., FY, Status, Best_EDA_.)
opcs3_apps %>% filter(Control. %in% missing_from_mgmt_apps) %>% select(Control., FY, Status, Best.EDA..)
opcs3_apps %>% filter(Control. %in% missing_from_opcs_apps) %>% select(Control., FY, Status, Best.EDA..)


##################################################################


# combine mgmt report's text and proj.cong.dist fields with opcs3

# check for duplicates just in case
if(sum(duplicated(opcs3$Control.)) == 0 && sum(duplicated(mgmt$Control_.)) == 0) {
        print("Good: No duplicates in OPCS or Management Report")
} else {
        stop("Error: Duplicates detected")
}

unique(mgmt$Database)

mgmt_extract <- mgmt %>% select(`Control_.`, Geographic_Need_Descr, Pressing_Need_Descr, General_Descr, Scope_Of_Work, GNS_Descr, 
                                                               Economic_Impact_Or_Benefit, Comments, X_PPS_Date, X_PPE_Date, X_PX1_Date, X_PX2_Date, Proj_Cong_Dist, Appl_Cong_Dist)

merged <- left_join(opcs3, mgmt_extract, by = c("Control." = "Control_."))
merged$database <- "opcs"
# glimpse(merged)


###########################################################################


# overwrite opcs3 proj/appl cong dist with the concatenated versions from mgmt extract (Q for Steve: why did you comment this out?)

# inspect current cong dist format
# unique(opcs3$Proj.Cong.Dist)
# unique(merged$Proj_Cong_Dist)
# unique(opcs3$Appl.Cong.Dist)
# unique(merged$Appl_Cong_Dist)

for(i in 1:nrow(merged)) {
        # pad mgmt proj congressional districts with leading zero
        proj_cong_dist_split <- str_split(merged$Proj_Cong_Dist[i], "\\|")[[1]]
        proj_cong_dist_split_padded <- sapply(proj_cong_dist_split, function(x) { ifelse(nchar(x) == 1, str_pad(x, width = 2, side = "left", pad = "0"), x) } )
        merged$Proj.Cong.Dist2[i] <- str_c(proj_cong_dist_split_padded, collapse = "|")
        
        # pad mgmt appl congressional districts with leading zero
        appl_cong_dist_split <- str_split(merged$Appl_Cong_Dist[i], "\\|")[[1]]
        appl_cong_dist_split_padded <- sapply(appl_cong_dist_split, function(x) { ifelse(nchar(x) == 1, str_pad(x, width = 2, side = "left", pad = "0"), x) } )
        merged$Appl.Cong.Dist2[i] <- str_c(appl_cong_dist_split_padded, collapse = "|")
}

# inspect output
merged %>% select(Proj.Cong.Dist, Proj_Cong_Dist, Proj.Cong.Dist2, Appl.Cong.Dist, Appl_Cong_Dist, Appl.Cong.Dist2) %>% head(.)

# replace Proj/Appl.Cong.Dist with padded values and drop extra variables
merged <- merged %>% mutate(Proj.Cong.Dist = Proj.Cong.Dist2, Appl.Cong.Dist = Appl.Cong.Dist2) %>% select(-c(Proj_Cong_Dist, Proj.Cong.Dist2, Appl_Cong_Dist, Appl.Cong.Dist2))
merged %>% select(Proj.Cong.Dist, Appl.Cong.Dist) %>% head(.)


################################################################


# convert dates to usable date format
merged$PCL.Date <- ymd_hm(merged$PCL.Date, tz = "EST")
merged$PCL.Entry.Dt <- ymd_hm(merged$PCL.Entry.Dt, tz = "EST")
merged$DEC.Date <- ymd_hm(merged$DEC.Date, tz = "EST")
merged$DEC.Entry.Dt <- ymd_hm(merged$DEC.Entry.Dt, tz = "EST")
merged$GSD.Date <- ymd_hm(merged$GSD.Date, tz = "EST")
merged$GPE.Date <- ymd_hm(merged$GPE.Date, tz = "EST")
merged$GPX.Date <- ymd_hm(merged$GPX.Date, tz = "EST")
merged$PPR.Date <- ymd_hm(merged$PPR.Date, tz = "EST")
merged$PRD.Date <- ymd_hm(merged$PRD.Date, tz = "EST")
# PPS, PPE, PX1, and PX2 contain multiple date formats, so use parse_date_time
merged$X_PPS_Date <- parse_date_time(merged$X_PPS_Date, orders = c("mdy", "ymd"), tz = "EST")
merged$X_PPE_Date <- parse_date_time(merged$X_PPE_Date, orders = c("mdy", "ymd"), tz = "EST")
merged$X_PX1_Date <- parse_date_time(merged$X_PX1_Date, orders = c("mdy", "ymd"), tz = "EST")
merged$X_PX2_Date <- parse_date_time(merged$X_PX2_Date, orders = c("mdy", "ymd"), tz = "EST")
merged$Report.Date.3.years <- ymd_hm(merged$Report.Date.3.years, tz = "EST")
merged$Report.Date.6.years <- ymd_hm(merged$Report.Date.6.years, tz = "EST")
merged$Report.Date.9.years <- ymd_hm(merged$Report.Date.9.years, tz = "EST")
merged$First.Resrv.Dt <- ymd_hm(merged$First.Resrv.Dt, tz = "EST")
merged$First.Oblig.Dt <- ymd_hm(merged$First.Oblig.Dt, tz = "EST")
merged$Last.Disb.Dt <- ymd_hm(merged$Last.Disb.Dt, tz = "EST")

# convert GPX.Act class to character, since the placeholder GPX.Act variable in the GOL dataframe below will be stored as a character
# field is all NA for OPCS and GOL, so no issues
merged$GPX.Act <- as.character(merged$GPX.Act)


########################################################################


# check to confirm merged has same control#, count, amount as opcs_no_duplicates
opcs_no_dup_filename <- list.files()[str_detect(list.files(), "opcs_no_duplicates_20")]
opcs_no_dup <- read_csv(opcs_no_dup_filename)

missing_from_merged <- opcs_no_dup$`Control#`[!(opcs_no_dup$`Control#` %in% merged$Control.)]
head(missing_from_merged)
opcs_no_dup %>% filter(`Control#` %in% missing_from_merged) %>% select(`Control#`, Status, FY, `Best EDA $`)

missing_from_opcs_no_dup <- merged$Control.[!(merged$Control. %in% opcs_no_dup$`Control#`)]
head(missing_from_opcs_no_dup)
merged %>% filter(Control. %in% missing_from_opcs_no_dup) %>% select(Control., Status, FY, Best.EDA..)

# test 
# if error, need to manually inspect missing_from_opcs/mgmt above
if(length(missing_from_merged) == 0 && length(missing_from_opcs_no_dup) == 0) {
        print("Good: merged and opcs_no_dup have same Control Numbers")
} else {
        stop("Error: merged and opcs_no_dup do not have same Control Numbers")
}

# check count/amount
merged_sum <- merged %>% summarize(count = n(), amount = sum(as.numeric(Best.EDA..), na.rm = TRUE)) %>% data.frame(.)
opcs_no_dup_sum <- opcs_no_dup %>% summarize(count = n(), amount = sum(as.numeric(`Best EDA $`), na.rm = TRUE)) %>% data.frame(.)

# test if total merged and opcs_no_dup count/amount match
if(merged_sum$count == opcs_no_dup_sum$count && merged_sum$amount == opcs_no_dup_sum$amount) {
        print("Good: merged and opcs_no_dup total count/amount match")
} else {
        stop("Error: merged and opcs_no_dup total count/amount do not match")
}

# funding for fy 2017 is off by $2.2 mil, $2 mil of which is the 2 projects missing from merged identified above
# the other 200k is presumably changes to fy 2017 records made in the time 
# btw 20170707 and 20170710 when the merged and opcs_no_dup files were pulled
merged_sum_fy <- merged %>% group_by(FY) %>% summarize(count = n(), amount = sum(as.numeric(Best.EDA..), na.rm = TRUE)) %>% data.frame(.)
opcs_no_dup_sum_fy <- opcs_no_dup %>% group_by(FY) %>% summarize(count = n(), amount = sum(as.numeric(`Best EDA $`), na.rm = TRUE)) %>% data.frame(.)
for(i in 1:nrow(merged_sum_fy)) {
        count_diff <- (merged_sum_fy$count[i] - opcs_no_dup_sum_fy$count[i])
        amount_diff <- dollar((merged_sum_fy$amount[i] - opcs_no_dup_sum_fy$amount[i]))
        print(str_c("FY ", merged_sum_fy$FY[i], ": count_diff: ", count_diff, "; amount_diff: ", amount_diff))
}


##################################################################################
###################################################################################
###################################################################################


# add gol data to merged
setwd("G:/PNP/Performance Measurement/master_data")

gol_awards_filename <- list.files()[str_detect(list.files(), "gol_awards_report_20")]
gol_awards <- read.csv(gol_awards_filename, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# lillian's gol award script includes three additional variables that need to be removed

# note when lillian sends gol_awards, it includes variables below, 

# gol_awards <- gol_awards %>% select(-c(TO_NUMBER.AF.FISCAL_YEAR., TO_NUMBER.TO_CHAR.AWT.GO_SIGN_, 
#                                        TO_CHAR.AWT.GO_SIGN_DT..YYYY.., TO_CHAR.AWT.GO_SIGN_DT..MON..
#                                     ))

# but when lillian's deputy sends files it has slightly different variables
gol_awards <- gol_awards %>% select(-c(TO_NUMBER.AF.FISCAL_YEAR., TO_NUMBER.TO_CHAR.AWT.GO_SIGN_, 
                                       TO_CHAR.AWT.GO_SIGN_DT..YYYY.., TO_CHAR.AWT.GO_SIGN_DT..MON.., FINANCIAL_CLOSEOUT,
                                       DISB_AMOUNT, DEOB_AMOUNT, AWARD_BALANCE))
# Add back: FINANCIAL_CLOSEOUT, ADMIN_CLOSEOUT, DISB_AMOUNT, DEOB_AMOUNT,
#AWARD_BALANCE

gol_applications_filename <- list.files()[str_detect(list.files(), "gol_applications_report_20")]
gol_applications <- read.csv(gol_applications_filename, stringsAsFactors = FALSE, na.strings = c("", "NA"))

# add placeholder variable to gol_applications for ADMIN_CLOSEOUT, since applications are not awarded and report does not include ADMIN_CLOSEOUT
gol_applications$ADMIN_CLOSEOUT <- NA

# test to ensure same variables in both gol_awards and gol_applications
if(sum(names(gol_awards) == names(gol_applications)) == length(names(gol_awards))) {
        print("Good: gol_awards and gol_applications have the same variables - good to merge")
} else {
        stop("Error: gol_awards and gol_applications do not have the same variables - cannot merge")
}

# if names do not match, inspect
names(gol_awards)[which(!names(gol_awards) %in% names(gol_applications))]

# merge gol_awards with gol_applications
gol <- rbind(gol_awards, gol_applications)

gol2 <- select(gol, UNIQUE_ID, LINE_OFFICE, PROGRAM_OFFICE, AWARD_NUMBER, APPLICATION_ID, APPLICANT_NAME, PROJECT_TITLE, RECEIVED_DT, PROJECT_DESC,
               AWARD_FED_SHARE, AWARD_NONFED_SHARE, APP_FED_SHARE, APP_NONFED_SHARE, GO_SIGN_DT, CONSTRUCTION_AWARD, AWARD_STATUS, 
               APPLICATION_STATUS, RFA_NAME, AWARD_FILE_FISCAL_YEAR, FUNDING_FY,
               COMPETITION_NAME, SPEC_INIT_CODES, APPLICANT_STREET, APPLICANT_CITY, APPLICANT_COUNTY, APPLICANT_STATE, APPLICANT_ZIP, 
               ESTIMATED_JOB_CREATED, ESTIMATED_JOB_SAVED, ESTIMATED_PRIVATE_INVESTMENT, APPLICATION_CONTACT, CONTACT_PHONE, CONTACT_EMAIL,
               CFDA_NUMBER, APPLICATION_STATUS, DUNS_NUMBER, MSI_CODE, APPROPRIATION_CODE, ASSIGNED_FPO,
               EIN_NUMBER, APPLICANT_CD, APPLICANT_TYPE,
               FIPS_CITY_CD, FIPS_COUNTY_CD, FIPS_STATE_CD, PROJECT_START_DT, PROJECT_END_DT, AWARD_START_DT, AWARD_END_DT,
               FIRST_FALD_DT, PROJECT_BENEFIT_AREA, PROJECT_NEED, ANTICIP_ECO_BENEFIT, PRIMARY_NAICS, ADMIN_CLOSEOUT)

# test for duplicate UNIQUE_ID
if(sum(duplicated(gol2$UNIQUE_ID)) == 0)  {
        print("Good: No duplicate UNIQUE_ID detected")
} else {
        stop("Error: Duplicate UNIQUE_ID detected")
}


###########################################################


# test to ensure that all unique award/amendemnt/fy combinations are included in gol_awards extract
gol_comparison_unique_filename <- list.files()[str_detect(list.files(), "gol_awards_unique")]
gol_comparison_unique <- read_csv(gol_comparison_unique_filename)
dim(gol_comparison_unique)

# create function to build unique combination from UNIQUE_ID by removing APPLICATION_ID from concatenation
create_unique_combination <- function(unique_id) {        
        str_c(str_split(unique_id, "-")[[1]][c(1, 2, 4)], collapse = "-")
}
gol_awards_unique <- sapply(gol_awards$UNIQUE_ID, function(x) { create_unique_combination(x) })
gol_awards_unique <- unname(gol_awards_unique)
length(gol_awards_unique)

awards_match_comparison <- length(which(gol_awards_unique %in% gol_comparison_unique$UNIQUE_COMBINATION))
comparison_match_awards <- length(which(gol_comparison_unique$UNIQUE_COMBINATION %in% gol_awards_unique))

# test

if(comparison_match_awards == nrow(gol_comparison_unique) && 
   awards_match_comparison == nrow(gol_comparison_unique)) {
        print("Good: GOL awards match GOL unique combination comparison file - there are no missing award/amendments")
} else {
        stop("Error: GOL awards do not match GOL unique combination comparison file - there are missing award/amendments")
}


#############################################################


# DUNS should be 9 digits - correct DUNS that lost leading zeroes when initially opening excel doc, and those w four trailing zeroes
unique(nchar(gol2$DUNS_NUMBER))
gol2 %>% group_by(nchar(DUNS_NUMBER)) %>% tally()
duns_errors <- which(str_sub(gol2$DUNS_NUMBER, start = -4) == "0000")
# test that all duns with 13 digits have four trailing zeroes before removing them
for(i in duns_errors){
        gol2$DUNS_NUMBER[i] <- str_sub(gol2$DUNS_NUMBER[i], start = 1, end = -5)
}
gol2$DUNS_NUMBER <- str_pad(gol2$DUNS_NUMBER, 9, side = "left", pad = "0")

# inspect
# unique(gol2$DUNS_NUMBER)

# test that all DUNS have 9 digits, except NA values
duns_check <- gol2 %>% group_by(nchar(DUNS_NUMBER)) %>% tally()
if(duns_check %>% filter(!is.na(`nchar(DUNS_NUMBER)`)) %>% distinct(`nchar(DUNS_NUMBER)`) == 9) {
        print("Good: All DUNS numbers have 9 digits or are NA")
} else {
        stop("Error: Not all DUNS numbers have 9 digits")
}


##########################################


# if grant_status is "No Award File" because it's a non-award, interpolate with application_status
# unique(gol2$AWARD_STATUS)
for(i in 1:nrow(gol2)){
        if(gol2$AWARD_STATUS[i] == "No Award File" && !is.na(gol2$APPLICATION_STATUS[i])) {
                gol2$AWARD_STATUS[i] <- gol2$APPLICATION_STATUS[i]
        } else if(gol2$AWARD_STATUS[i] == "No Award File" && is.na(gol2$APPLICATION_STATUS[i])) {
                gol2$AWARD_STATUS[i] <- "Application Status is Blank" # there were two non-award records with APPLICATION_STATUS blank
        }
}

# inspect
# unique(gol2$AWARD_STATUS)

# test that award_status is populated for all records, and that application_status was substituted for records not yet in 
# awards process
if(!("No Award File" %in% unique(gol2$AWARD_STATUS)) && !is.na(gol2$AWARD_STATUS)) {
        print("Good: There are no AWARD_STATUS = NA records, and the APPLICATION_STATUS substitution was successful")
} else {
        stop("Error: There are either AWARD_STATUS = NA records, or the APPLICATION_STATUS substitution unsuccessful")
}


##########################################


# create FY variable
gol2$FY <- gol2$FUNDING_FY

# inspect
# unique(gol2$FY)

# test for no NA values for FY
if(sum(is.na(gol2$FY)) == 0) {
        print("Good: All records have an FY")
} else {
        stop("Error: All records do not have an FY")
}


############################################


# add state and county fips, and congressional district using gol Appl.Zip and HUD's quarterly crosswalk files
# gol applicant_CD is non-standardized
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html

# setwd
setwd("G:/PNP/Performance Measurement/master_data")

# add state and county fips
# note fips are imputed from appl.zip code for those records without a FIPS_STATE_CD or FIPS_COUNTY_CD listed already
zip_county_filename <- list.files()[str_detect(list.files(), "ZIP_COUNTY_")]
zip_county <- read_csv(zip_county_filename)

# inspect recrods with no applicant zip code
# gol2 %>% filter(is.na(APPLICANT_ZIP)) %>% select(UNIQUE_ID, APPLICANT_ZIP, APPLICANT_STREET, APPLICANT_NAME)

# create Appl.ZIP.4 and clean APPLICANT_ZIP
for(i in 1:nrow(gol2)) {
        if(!is.na(gol2$APPLICANT_ZIP[i])) {
                if(grepl("-|'|/|\\?", gol2$APPLICANT_ZIP[i])) {
                        # split zipcode plus 4 (there are some non-standardized split characters)
                        # there is at least one entry with three consecutive question marks??
                        # so the zip_split can either be of length 2 or length 4
                        zip_split <- str_split(gol2$APPLICANT_ZIP[i], "-|'|/|\\?")[[1]] 
                        gol2$Appl.ZIP.4[i] <- str_trim(zip_split[length(zip_split)], side = "both")
                        
                        # trim and pad the five digit zip
                        zip_trim <- str_trim(zip_split[1], side = "both")
                        gol2$APPLICANT_ZIP[i] <- str_pad(zip_trim, width = 5, side = "left", pad = "0")
                } else if(!grepl("-|'|/", gol2$APPLICANT_ZIP[i]) && nchar(gol2$APPLICANT_ZIP[i]) > 5) {
                        # handle entries with nine digits and no sepearation
                        gol2$Appl.ZIP.4[i] <- str_sub(gol2$APPLICANT_ZIP[i], start = -4)
                        zip_minus_last_four <- str_sub(gol2$APPLICANT_ZIP[i], start = 1, end = -5)
                        zip_minus_last_four <- str_trim(zip_minus_last_four, side = "both")
                        gol2$APPLICANT_ZIP[i] <- str_pad(zip_minus_last_four, width = 5, side = "left", pad = "0")
                } else if(nchar(gol2$APPLICANT_ZIP[i]) <= 5) {
                        # handle entries with less than 5 digits
                        gol2$APPLICANT_ZIP[i] <- str_pad(gol2$APPLICANT_ZIP[i], width = 5, side = "left", pad = "0")
                }
        }
}

# inspect 
# unique(gol2$APPLICANT_ZIP)
# unique(gol2$Appl.ZIP.4)

# inspect zip codes not in HUD list - not clear why they are missing from HUD list
unknown_zip_index <- which(!(gol2$APPLICANT_ZIP %in% zip_county$ZIP))
# gol2[unknown_zip_index, ] %>% select(UNIQUE_ID, APPLICANT_ZIP, APPLICANT_STATE, APPLICANT_CITY,
#                                      APPLICANT_COUNTY, FIPS_STATE_CD, FIPS_COUNTY_CD)
# gol2[unknown_zip_index, ] %>% select(UNIQUE_ID, APPLICANT_ZIP, APPLICANT_STATE, APPLICANT_CITY,
#                                      APPLICANT_COUNTY, FIPS_STATE_CD, FIPS_COUNTY_CD) %>% distinct(APPLICANT_ZIP)


# test to see if any additional zip codes not in HUD's list that are not already known exceptions
# not sure why some zips are missing from HUD data, but should be minor issue
non_hud_zip <- gol2[unknown_zip_index, ] %>% select(APPLICANT_ZIP)
zip_exceptions_count <- nrow(unique(non_hud_zip)) # currently 16 exceptions
zip_exceptions <- unique(non_hud_zip)
if(zip_exceptions_count <= 16) { 
        print("Good: All non-HUD zip codes in data are known exceptions")
} else {
        stop("Error: Some non-HUD zip codes in data are not known exceptions")
}


##############################################################


# create placeholder appl_fips_state_county
gol2$app_fips_state_county <- NA

# loop through assigning state/county fips based on APPLICANT ZIP
for(i in 1:nrow(gol2)){
        if(!(is.na(gol2$APPLICANT_ZIP[i]))){
                if(gol2$APPLICANT_ZIP[i] %in% zip_county$ZIP){
                        zip_match_index <- which(zip_county$ZIP == gol2$APPLICANT_ZIP[i])
                        # note some zip codes span multiple counties
                        # they will be assigned the first county in the list though, which isnt great, but better than all NAs
                        gol2$app_fips_state_county[i] <- zip_county$COUNTY[zip_match_index[1]]
                }
        }
}
gol2$app_fips_state_county <- str_pad(gol2$app_fips_state_county, width = 5, side = "left", pad = "0")
gol2$Appl.FIPS.State <- str_sub(gol2$app_fips_state_county, 1, 2)
gol2$Appl.FIPS.County <- str_sub(gol2$app_fips_state_county, 3, 5)

# inspect derived Appl.FIPS.State/County
# gol2 %>% select(app_fips_state_county, Appl.FIPS.State, Appl.FIPS.County) %>% head(.)

# test that all records have received derived app_fips_state_county, except known exceptions
na_app_fips_state_county <- gol2 %>% filter(is.na(app_fips_state_county)) %>% select(APPLICANT_ZIP)
if(sum(na_app_fips_state_county$APPLICANT_ZIP %in% zip_exceptions$APPLICANT_ZIP) == length(na_app_fips_state_county$APPLICANT_ZIP)) { 
        print("Good: All records with app_fips_state_county = NA in data are known exceptions")
} else {
        stop("Error: Some records with app_fips_state_county = NA in data are not known exceptions")
}

# overwrite derived Appl.FIPS.State/County with original FIPS_STATE_CD or FIPS_COUNTY_CD if available
# inspect how many original FIPS_STATE/COUNTY_CD are NA, which will therefore retained derived state/county
sum(is.na(gol2$FIPS_STATE_CD))
sum(is.na(gol2$FIPS_COUNTY_CD))
gol2 %>% group_by(AWARD_STATUS) %>% summarize(fips_state_na_count = sum(is.na(FIPS_STATE_CD)), fips_county_na_count = sum(is.na(FIPS_COUNTY_CD)))

for(i in 1:nrow(gol2)) {
        if(!(is.na(gol2$FIPS_STATE_CD[i]))) {
                gol2$Appl.FIPS.State[i] <- gol2$FIPS_STATE_CD[i]
        }
        if(!(is.na(gol2$FIPS_COUNTY_CD[i]))) {
                gol2$Appl.FIPS.County[i] <- gol2$FIPS_COUNTY_CD[i]
        }
}

gol2$Appl.FIPS.State <- str_pad(gol2$Appl.FIPS.State, width = 2, side = "left", pad = "0")
gol2$Appl.FIPS.County <- str_pad(gol2$Appl.FIPS.County, width = 3, side = "left", pad = "0")

# inspect
gol2$Appl.FIPS.County
gol2$Appl.FIPS.State
sum(is.na(gol2$Appl.FIPS.County))
sum(is.na(gol2$Appl.FIPS.State))

# test to make sure all Appl.FIPS.State/County have values, except known exceptions
na_appl_fips_state <- gol2 %>% filter(is.na(Appl.FIPS.State)) %>% select(APPLICANT_ZIP)
if(sum(na_appl_fips_state$APPLICANT_ZIP %in% zip_exceptions$APPLICANT_ZIP) == length(na_appl_fips_state$APPLICANT_ZIP)) { 
        print("Good: All records with Appl.FIPS.State = NA in data are known exceptions")
} else {
        stop("Error: Some records with Appl.FIPS.State = NA in data are not known exceptions")
}

na_appl_fips_county <- gol2 %>% filter(is.na(Appl.FIPS.County)) %>% select(APPLICANT_ZIP)
if(sum(na_appl_fips_county$APPLICANT_ZIP %in% zip_exceptions$APPLICANT_ZIP) == length(na_appl_fips_county$APPLICANT_ZIP)) { 
        print("Good: All records with Appl.FIPS.County = NA in data are known exceptions")
} else {
        stop("Error: Some records with Appl.FIPS.County = NA in data are not known exceptions")
}


#########################################################################


# add county names
# using 2010 counties & fips from census https://www.census.gov/geo/reference/codes/cou.html
counties <- read_csv("us_counties.txt", col_names = c("state", "fips_state", "fips_county", "county", "some_variable"))
counties$fips_state_county <- str_c(counties$fips_state, counties$fips_county)

# check - if Error due to non-Census app_fips_state_county in data that are not known exceptions
# gol2 %>% filter(APPLICANT_ZIP %in% non_census_app_fips_state_county_zip$APPLICANT_ZIP) %>% select(APPLICANT_STATE, APPLICANT_ZIP, app_fips_state_county)

# update list of known zip_exceptions
zip_exceptions <- rbind(zip_exceptions, "57770")

# test to see if any gol2 fips_state_county are not in census list of counties
unknown_app_fips_state_county_index <- which(!(gol2$app_fips_state_county %in% counties$fips_state_county))
non_census_app_fips_state_county_zip <- gol2[unknown_app_fips_state_county_index, ] %>% select(APPLICANT_ZIP)
if(sum(non_census_app_fips_state_county_zip$APPLICANT_ZIP %in% zip_exceptions$APPLICANT_ZIP) == length(non_census_app_fips_state_county_zip$APPLICANT_ZIP)) { 
        print("Good: All non-Census app_fips_state_county in data are known exceptions")
} else {
        stop("Error: Some non-Census app_fips_state_county in data are not known exceptions")
}

# create placeholder Appl.Cnty.Name and loop through data assigning values
gol2$Appl.Cnty.Name <- NA
for(i in 1:nrow(gol2)){
        if(!(is.na(gol2$app_fips_state_county[i]))){
                if(gol2$app_fips_state_county[i] %in% counties$fips_state_county) {
                        # print(i)
                        # print(gol2$app_fips_state_county[i])
                        county_match_index <- which(counties$fips_state_county == gol2$app_fips_state_county[i])
                        county_name <- str_replace(counties$county[county_match_index], " County", "")
                        county_name <- str_replace(county_name, " Parish", "")
                        county_name <- str_replace(county_name, " Municipality", "")
                        county_name <- str_replace(county_name, " Borough", "")
                        county_name <- str_replace(county_name, " Municipio", "")
                        gol2$Appl.Cnty.Name[i] <- county_name
                }
                # special handling for oglala lakota county in south dakota (46-102)
                # it isn't in census list, but is real
                # https://www.census.gov/geo/reference/county-changes.html
                if(gol2$app_fips_state_county[i] == "46102") {
                        gol2$Appl.Cnty.Namep[i] == "Oglala Lakota"
                }
        }
}

# overwrite derived Appl.Cnty.Name with original APPLICANT_COUNTY if available
# inspect how many original APPLICANT_COUNTY are NA, which will therefore retain derived county name
sum(is.na(gol2$APPLICANT_COUNTY))

for(i in 1:nrow(gol2)) {
        if(!(is.na(gol2$APPLICANT_COUNTY[i]))) {
                gol2$Appl.Cnty.Name[i] <- gol2$APPLICANT_COUNTY[i]
        }
}

# inspect
gol2$Appl.Cnty.Name
sum(is.na(gol2$Appl.Cnty.Name))

# test to make sure all Appl.Cnty.Name have values, except known exceptions
na_appl_cnty_name <- gol2 %>% filter(is.na(Appl.FIPS.State)) %>% select(APPLICANT_ZIP)
if(sum(na_appl_cnty_name$APPLICANT_ZIP %in% zip_exceptions$APPLICANT_ZIP) == length(na_appl_cnty_name$APPLICANT_ZIP)) { 
        print("Good: All records with Appl.Cnty.Name = NA in data are known exceptions")
} else {
        stop("Error: Some records with Appl.Cnty.Name = NA in data are not known exceptions")
}


################################################################


# add congressional districts using HUD CD-ZIP crosswalk
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html
zip_cd_filename <- list.files()[str_detect(list.files(), "ZIP_CD_")]
zip_cd <- read_csv(zip_cd_filename)
# note there will be a warning reading it in due to unexpected asterisks in the CD field, but these are just a placeholder for the statewide row of the crosswalk

# note gol awards have standardized APPLICANT_CD, but non-awards are unstandardized free-text
# so only need to derive appl.cong.dist for non-awards

# create placeholder variables and loop through data assigning values
gol2$Appl.Cong.Dist <- NA
for(i in 1:nrow(gol2)){
        if(!(is.na(gol2$APPLICANT_ZIP[i])) && gol2$AWARD_STATUS[i] != "Signed and Complete" && gol2$APPLICANT_ZIP[i] %in% zip_cd$ZIP) {
                zip_match_index <- which(zip_cd$ZIP == gol2$APPLICANT_ZIP[i])
                # note there will likely be some warnings, since some zip codes span multiple cong_districts
                # it will just assign the first county in the list though, which isnt great, but better than all NAs
                gol2$Appl.Cong.Dist[i] <- str_sub(zip_cd$CD[zip_match_index[1]], start = -2)
        }
        # if the award is complete and the state is in the US (not Guam, American Samoa, etc), assign GOL-generated/vetted APPLICANT_CD
        if(gol2$AWARD_STATUS[i] == "Signed and Complete" && gol2$APPLICANT_STATE[i] %in% counties$state) {
                gol2$Appl.Cong.Dist[i] <- str_sub(gol2$APPLICANT_CD[i], start = -2) 
        }
        # handle known issues with zip codes not listed in HUD crosswalk file
        if(!is.na(gol2$APPLICANT_ZIP[i]) && gol2$APPLICANT_ZIP[i] == "15282") {
                gol2$Appl.Cong.Dist[i] <- "14"
        }
        if(!is.na(gol2$APPLICANT_ZIP[i]) && gol2$APPLICANT_ZIP[i] == "40526") {
                gol2$Appl.Cong.Dist[i] <- "06"
        }
}

# test to make sure all Appl.Cong.Dist have values, except known exceptions
na_appl_cong_dist <- gol2 %>% filter(is.na(Appl.Cong.Dist)) %>% select(APPLICANT_ZIP)
if(sum(na_appl_cong_dist$APPLICANT_ZIP %in% zip_exceptions$APPLICANT_ZIP) == length(na_appl_cong_dist$APPLICANT_ZIP)) { 
        print("Good: All records with Appl.Cong.Dist = NA in data are known exceptions")
} else {
        stop("Error: Some records with Appl.Cong.Dist = NA in data are not known exceptions")
}


#################################################################


# add region
gol2$Region.Name <- NA
for(i in 1:nrow(gol2)){
        if(!(is.na(gol2$LINE_OFFICE[i]))){
                if(gol2$LINE_OFFICE[i] == "ATL"){
                        gol2$Region.Name[i] <- "Atlanta"
                }
                if(gol2$LINE_OFFICE[i] == "AUS"){
                        gol2$Region.Name[i] <- "Austin"
                }
                if(gol2$LINE_OFFICE[i] == "CHI"){
                        gol2$Region.Name[i] <- "Chicago"
                }
                if(gol2$LINE_OFFICE[i] == "DEN"){
                        gol2$Region.Name[i] <- "Denver"
                }
                if(gol2$LINE_OFFICE[i] == "PHI"){
                        gol2$Region.Name[i] <- "Philadelphia"
                }
                if(gol2$LINE_OFFICE[i] == "SEA"){
                        gol2$Region.Name[i] <- "Seattle"
                }
                if(gol2$LINE_OFFICE[i] == "HDQ"){
                        gol2$Region.Name[i] <- "Headquarters"
                }
        }
}

# check that all projects have a Region.Name
if(sum(is.na(gol2$Region.Name)) == 0) {
        print("Good: No projects are missing a Region.Name")
} else {
        stop("Error: Some projects are missing a Region.Name")
}


##########################################################


# clean gol variables by making them title-case (removes all upper case values, etc)
# note is also de-caps acronyms

# applicant name
head(gol2$APPLICANT_NAME)
for(z in 1:nrow(gol2)) {
        if(!(grepl("[a-z]", gol2$APPLICANT_NAME[z]) && grepl("[A-Z]", gol2$APPLICANT_NAME[z]))) {
                gol2$APPLICANT_NAME[z] <- str_to_title(gol2$APPLICANT_NAME[z])
        }
}
head(gol2$APPLICANT_NAME)

# applicant street
head(gol2$APPLICANT_STREET)
for(z in 1:nrow(gol2)) {
        if(!(grepl("[a-z]", gol2$APPLICANT_STREET[z]) && grepl("[A-Z]", gol2$APPLICANT_STREET[z]))) {
                gol2$APPLICANT_STREET[z] <- str_to_title(gol2$APPLICANT_STREET[z])
        }
}
head(gol2$APPLICANT_STREET)

# applicant city
head(gol2$APPLICANT_CITY)
for(z in 1:nrow(gol2)) {
        if(!(grepl("[a-z]", gol2$APPLICANT_CITY[z]) && grepl("[A-Z]", gol2$APPLICANT_CITY[z]))) {
                gol2$APPLICANT_CITY[z] <- str_to_title(gol2$APPLICANT_CITY[z])
        }
}
head(gol2$APPLICANT_CITY)


###########################################################################


# add to the merged opcs dataframe any variables that are present in gol but not in opcs
merged$Appl_NAICS <- NA
merged$Appl_NAICS[1] <- as.integer(123456)
merged$Appl_NAICS[1] <- NA

# then we'll create a placeholder dataframe with the same dimensions as the merged opcs data
# which we'll populate by crosswalking the gol variables to their opcs equivalents
gol3 <- as.data.frame(matrix(as.character(""), ncol = ncol(merged), nrow = nrow(gol2)))

# convert empty dataframe to class character
for(i in 1:ncol(gol3)){
        gol3[ , i] <- as.character(gol3[ , i])
}

# assign gol columns to empty dataframe to map with merged column numbers
gol3[ , which(names(merged) == "FY")] <- gol2$FY
gol3[ , which(names(merged) == "CFDA..")] <- gol2$CFDA_NUMBER
gol3[ , which(names(merged) == "Appropriation")] <- gol2$PROGRAM_OFFICE
gol3[ , which(names(merged) == "Prog.Abbr")] <- gol2$PROGRAM_OFFICE
gol3[ , which(names(merged) == "Status")] <- gol2$AWARD_STATUS
gol3[ , which(names(merged) == "Control.")] <- gol2$UNIQUE_ID
gol3[ , which(names(merged) == "Project.No.")] <- gol2$AWARD_NUMBER
gol3[ , which(names(merged) == "Appl.Short.Name")] <- gol2$APPLICANT_NAME
gol3[ , which(names(merged) == "Full.Applicant.Name")] <- gol2$APPLICANT_NAME
gol3[ , which(names(merged) == "Project.Short.Descrip")] <- gol2$PROJECT_TITLE
gol3[ , which(names(merged) == "General.Descr.")] <- gol2$PROJECT_DESC
gol3[ , which(names(merged) == "Scope.of.Work")] <- gol2$PROJECT_DESC
gol3[ , which(names(merged) == "GNS.Descr.")] <- gol2$PROJECT_NEED
gol3[ , which(names(merged) == "Economic.Impact.or.Benefit")] <- gol2$ANTICIP_ECO_BENEFIT
gol3[ , which(names(merged) == "PPR.Date")] <- gol2$RECEIVED_DT
gol3[ , which(names(merged) == "DEC.Date")] <- gol2$GO_SIGN_DT
gol3[ , which(names(merged) == "Cons.Non")] <- gol2$CONSTRUCTION_AWARD
gol3[ , which(names(merged) == "Appr.Desc")] <- gol2$RFA_NAME
gol3[ , which(names(merged) == "Prog.Tool.Name")] <- gol2$COMPETITION_NAME
gol3[ , which(names(merged) == "Initiatives")] <- gol2$SPEC_INIT_CODES
gol3[ , which(names(merged) == "Jobs.Created")] <- gol2$ESTIMATED_JOB_CREATED
gol3[ , which(names(merged) == "Jobs.Saved")] <- gol2$ESTIMATED_JOB_SAVED
gol3[ , which(names(merged) == "Private.Investment")] <- gol2$ESTIMATED_PRIVATE_INVESTMENT
gol3[ , which(names(merged) == "Appl.Street.Addr.1")] <- gol2$APPLICANT_STREET
gol3[ , which(names(merged) == "Appl.City.Name")] <- gol2$APPLICANT_CITY
gol3[ , which(names(merged) == "Appl.State.Abbr")] <- gol2$APPLICANT_STATE
gol3[ , which(names(merged) == "Appl..Zip")] <- gol2$APPLICANT_ZIP
gol3[ , which(names(merged) == "Appl.FIPS.ST")] <- gol2$Appl.FIPS.State
gol3[ , which(names(merged) == "Appl.FIPS.Cnty")] <- gol2$Appl.FIPS.County
gol3[ , which(names(merged) == "Appl.Cnty.Name")] <- gol2$Appl.Cnty.Name 
gol3[ , which(names(merged) == "Appl.ZIP.4")] <- gol2$Appl.ZIP.4
gol3[ , which(names(merged) == "Appl.Cong.Dist")] <- gol2$Appl.Cong.Dist
gol3[ , which(names(merged) == "Proj.City.Name")] <- gol2$APPLICANT_CITY
gol3[ , which(names(merged) == "Proj.ST.Abbr")] <- gol2$APPLICANT_STATE
gol3[ , which(names(merged) == "Proj.ZIP")] <- gol2$APPLICANT_ZIP
gol3[ , which(names(merged) == "Proj.FIPS.ST")] <- gol2$Appl.FIPS.State
gol3[ , which(names(merged) == "Proj.FIPS.Cnty")] <- gol2$Appl.FIPS.County
gol3[ , which(names(merged) == "Proj.County.Name")] <- gol2$Appl.Cnty.Name 
gol3[ , which(names(merged) == "Proj.Cong.Dist")] <- gol2$Appl.Cong.Dist
gol3[ , which(names(merged) == "Contact.Email")] <- gol2$CONTACT_EMAIL
gol3[ , which(names(merged) == "Appl.Cont.Phone")] <- gol2$CONTACT_PHONE
gol3[ , which(names(merged) == "Appl.Contact.Name")] <- gol2$APPLICATION_CONTACT
gol3[ , which(names(merged) == "Region.Name")] <- gol2$Region.Name
gol3[ , which(names(merged) == "DUNS..")] <- gol2$DUNS_NUMBER
gol3[ , which(names(merged) == "MSI.Indicator")] <- gol2$MSI_CODE
gol3[ , which(names(merged) == "Appr.Code")] <- gol2$APPROPRIATION_CODE
gol3[ , which(names(merged) == "IRS..")] <- gol2$EIN_NUMBER
gol3[ , which(names(merged) == "Appl.FIPS.City")] <- gol2$FIPS_CITY_CD
gol3[ , which(names(merged) == "X.PPS._Date")] <- gol2$PROJECT_START_DT
gol3[ , which(names(merged) == "X.PPE._Date")] <- gol2$PROJECT_END_DT
gol3[ , which(names(merged) == "GSD.Date")] <- gol2$AWARD_START_DT
gol3[ , which(names(merged) == "GPE.Date")] <- gol2$AWARD_END_DT
gol3[ , which(names(merged) == "PRD.Date")] <- gol2$FIRST_FALD_DT
gol3[ , which(names(merged) == "EDA.Official.Name")] <- gol2$ASSIGNED_FPO
gol3[ , which(names(merged) == "Entity.Code")] <- gol2$PROJECT_BENEFIT_AREA
gol3[ , which(names(merged) == "Appl_NAICS")] <- gol2$PRIMARY_NAICS
gol3[ , which(names(merged) == "Appl.Type.Name")] <- gol2$APPLICANT_TYPE
gol3[ , which(names(merged) == "PCL.Date")] <- gol2$ADMIN_CLOSEOUT


# compute Best.EDA.., Local.Applicant.., and Total.Project.. from award_fed_share if available, app_nonfed_share if not
# gol3[ , which(names(merged) == "Best.EDA..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_FED_SHARE[row])) {gol2$APP_FED_SHARE[row]} else 
# {gol2$AWARD_FED_SHARE[row]})
gol3[ , which(names(merged) == "Best.EDA..")] <- sapply(1:nrow(gol2), function(row) if(gol2$AWARD_STATUS[row] != "Signed and Complete") {gol2$APP_FED_SHARE[row]} else 
{gol2$AWARD_FED_SHARE[row]})
gol3[ , which(names(merged) == "Local.Applicant..")] <- sapply(1:nrow(gol2), function(row) if(gol2$AWARD_STATUS[row] != "Signed and Complete") {gol2$APP_NONFED_SHARE[row]} else 
{gol2$AWARD_NONFED_SHARE[row]})
gol3[ , which(names(merged) == "Total.Project..")] <- sapply(1:nrow(gol2), function(row) if(gol2$AWARD_STATUS[row] != "Signed and Complete") {sum(gol2$APP_NONFED_SHARE[row], gol2$APP_FED_SHARE[row])} else 
{sum(gol2$AWARD_NONFED_SHARE[row], gol2$AWARD_FED_SHARE[row])})

names(gol3) <- names(merged)


##########################################################################


# fill in database variable showing data origin
gol3$database <- "gol"

# convert gol Status "Accepted" to "Approved"
approved_index <- which(gol3$Status == "Signed and Complete")
gol3$Status[approved_index] <- "Approved"


#########################################################################


# need to convert gol data fields to date/time format in order to bind together without error

# for those date variables in opcs without a gol equivalent, we need to create a dummy date
# then convert that date to date format, then erase the dummy date
gol3$Report.Date.3.years[1] <- "2009-08-21 12:00 AM"
gol3$Report.Date.3.years <- ymd_hms(gol3$Report.Date.3.years[1])
gol3$Report.Date.3.years[1] <- NA

gol3$Report.Date.6.years[1] <- "2009-08-21 12:00 AM"
gol3$Report.Date.6.years <- ymd_hms(gol3$Report.Date.6.years[1])
gol3$Report.Date.6.years[1] <- NA

gol3$Report.Date.9.years[1] <- "2009-08-21 12:00 AM"
gol3$Report.Date.9.years <- ymd_hms(gol3$Report.Date.9.years[1])
gol3$Report.Date.9.years[1] <- NA

gol3$First.Resrv.Dt[1] <- "2009-08-21 12:00 AM"
gol3$First.Resrv.Dt <- ymd_hms(gol3$First.Resrv.Dt[1])
gol3$First.Resrv.Dt[1] <- NA

gol3$First.Oblig.Dt[1] <- "2009-08-21 12:00 AM"
gol3$First.Oblig.Dt <- ymd_hms(gol3$First.Oblig.Dt[1])
gol3$First.Oblig.Dt[1] <- NA

gol3$Last.Disb.Dt[1] <- "2009-08-21 12:00 AM"
gol3$Last.Disb.Dt <- ymd_hms(gol3$Last.Disb.Dt[1])
gol3$Last.Disb.Dt[1] <- NA

gol3$PCL.Date <- mdy_hm(gol3$PCL.Date)
gol3$PCL.Date <- ymd_hms(gol3$PCL.Date)

gol3$X_PPS_Date[1] <- "2009-08-21 12:00 AM"
gol3$X_PPS_Date <- ymd_hms(gol3$X_PPS_Date)
gol3$X_PPS_Date[1] <- NA

gol3$X_PPE_Date[1] <- "2009-08-21 12:00 AM"
gol3$X_PPE_Date <- ymd_hms(gol3$X_PPE_Date)
gol3$X_PPE_Date[1] <- NA

gol3$X_PX1_Date[1] <- "2009-08-21 12:00 AM"
gol3$X_PX1_Date <- ymd_hms(gol3$X_PX1_Date)
gol3$X_PX1_Date[1] <- NA

gol3$X_PX2_Date[1] <- "2009-08-21 12:00 AM"
gol3$X_PX2_Date <- ymd_hms(gol3$X_PX2_Date)
gol3$X_PX2_Date[1] <- NA


#Contact Lillian over standardizing dates, scripts she uses returns one set of dates format and the the script her subordinate uses uses returns another set of dates formats
# for opcs dates with a gol equivalent, just convert to date format without need for dummy dates
gol3$DEC.Date <- mdy_hm(gol3$DEC.Date)
# gol3$DEC.Date <- parse_date_time(gol3$DEC.Date, orders = c("%d-%m-%y", "%m/%d/%Y"))
gol3$PPR.Date <- mdy_hm(gol3$PPR.Date)
#gol3$PPR.Date <- parse_date_time(gol3$PPR.Date, orders = c("%d-%m-%y", "%m/%d/%Y"))
# note: there currently is one record that with a non-valid date that fails to parse - will just assign NA to that record, not an issue
gol3$GSD.Date <- mdy_hm(gol3$GSD.Date)
#gol3$GSD.Date <- parse_date_time(gol3$GSD.Date, orders = c("%d-%m-%y", "%m/%d/%Y"))

gol3$GPE.Date <- mdy_hm(gol3$GPE.Date)
#gol3$GPE.Date <- parse_date_time(gol3$GPE.Date, orders = c("%d-%m-%y", "%m/%d/%Y"))

gol3$PRD.Date <- mdy_hm(gol3$PRD.Date)
#gol3$PRD.Date <- parse_date_time(gol3$PRD.Date, orders = c("%d-%m-%y", "%m/%d/%Y"))

# rbind gol data to merged oit and opcs data
merged2 <- rbind(gol3, merged)


#################################################


# create Program variable spelling out the Prog.Abbr variable
merged2$Program <- merged2$Prog.Abbr

for(i in 1:nrow(merged2)){
        if(!(is.na(merged2$Program[i]))){
                if(merged2$Program[i] == "PW"){
                        merged2$Program[i] <- "Public Works"
                }
                if(merged2$Program[i] == "PL"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA"){
                        merged2$Program[i] <- "Technical Assistance"
                }
                if(merged2$Program[i] == "T9"){
                        merged2$Program[i] <- "Economic Adjustment Assistance"
                }
                if(merged2$Program[i] == "TJ"){
                        merged2$Program[i] <- "Trade Adjustment Assistance for Firms"
                }
                if(merged2$Program[i] == "RE"){
                        merged2$Program[i] <- "Research"
                }
                if(merged2$Program[i] == "EV"){
                        merged2$Program[i] <- "Evaluation"
                }
                if(merged2$Program[i] == "PL-ATRO"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA-ATRO"){
                        merged2$Program[i] <- "Technical Assistance"
                }
                if(merged2$Program[i] == "PL-AURO"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA-AURO"){
                        merged2$Program[i] <- "Technical Assistance"
                }
                if(merged2$Program[i] == "PL-CRO"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA-CRO"){
                        merged2$Program[i] <- "Technical Assistance"
                }
                if(merged2$Program[i] == "PL-DRO"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA-DRO"){
                        merged2$Program[i] <- "Technical Assistance"
                }
                if(merged2$Program[i] == "OIE"){
                        merged2$Program[i] <- "Regional Innovation Strategies"
                }
                if(merged2$Program[i] == "RNTA"){
                        merged2$Program[i] <- "Research and National Technical Assistance"
                }
                if(merged2$Program[i] == "TAAF"){
                        merged2$Program[i] <- "Trade Adjustment Assistance for Firms"
                }
                if(merged2$Program[i] == "PL-PRO"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA-PRO"){
                        merged2$Program[i] <- "Technical Assistance"
                }
                if(merged2$Program[i] == "PL-SRO"){
                        merged2$Program[i] <- "Planning"
                }
                if(merged2$Program[i] == "TA-SRO"){
                        merged2$Program[i] <- "Technical Assistance"
                }
        }
}

# check that all projects have a Program
# there are 10 known exceptions from 2012 opcs projects with NA for Program/Prog.Abbr for some reason, likely due to 2012 IT disruption
prog_exceptions <- c("100338", "100348", "100334", "100347", "100351", "100336", "100341", "100340", "100339", "100356")
missing_program <- merged2 %>% filter(is.na(Program))
if(sum(missing_program$Control. %in% prog_exceptions) == length(prog_exceptions)) {
        print("Good: No non-exception projects are missing a Program")
} else {
        stop("Error: Some non-exception projects are missing a Program")
}


################################################################


# standardize the Prog.Abbr variable across opcs and gol
for(i in 1:nrow(merged2)){
        if(!(is.na(merged2$Prog.Abbr[i]))){
                if(merged2$Prog.Abbr[i] == "T9"){
                        merged2$Prog.Abbr[i] <- "EAA"
                }
                if(merged2$Prog.Abbr[i] == "TJ"){
                        merged2$Prog.Abbr[i] <- "TAAF"
                }
                if(merged2$Prog.Abbr[i] == "PL-ATRO"){
                        merged2$Prog.Abbr[i] <- "PL"
                }
                if(merged2$Prog.Abbr[i] == "TA-ATRO"){
                        merged2$Prog.Abbr[i] <- "TA"
                }
                if(merged2$Prog.Abbr[i] == "PL-AURO"){
                        merged2$Prog.Abbr[i] <- "PL"
                }
                if(merged2$Prog.Abbr[i] == "TA-AURO"){
                        merged2$Prog.Abbr[i] <- "TA"
                }
                if(merged2$Prog.Abbr[i] == "PL-CRO"){
                        merged2$Prog.Abbr[i] <- "PL"
                }
                if(merged2$Prog.Abbr[i] == "TA-CRO"){
                        merged2$Prog.Abbr[i] <- "TA"
                }
                if(merged2$Prog.Abbr[i] == "PL-DRO"){
                        merged2$Prog.Abbr[i] <- "PL"
                }
                if(merged2$Prog.Abbr[i] == "TA-DRO"){
                        merged2$Prog.Abbr[i] <- "TA"
                }
                if(merged2$Prog.Abbr[i] == "PL-PRO"){
                        merged2$Prog.Abbr[i] <- "PL"
                }
                if(merged2$Prog.Abbr[i] == "TA-PRO"){
                        merged2$Prog.Abbr[i] <- "TA"
                }
                if(merged2$Prog.Abbr[i] == "PL-SRO"){
                        merged2$Prog.Abbr[i] <- "PL"
                }
                if(merged2$Prog.Abbr[i] == "TA-SRO"){
                        merged2$Prog.Abbr[i] <- "TA"
                }
                if(merged2$Prog.Abbr[i] == "OIE"){
                        merged2$Prog.Abbr[i] <- "RIS"
                }
        }
}


##########################################################


# add oea clean descriptions
# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/OEA Award Announcements")

# read in cleaned data with oea descriptions
# note read.csv must be used because read_csv chokes on multibyte string
oea <- read.csv("oea_clean_data_fy11-16.csv", colClasses = c("Project.No." = "character"))
glimpse(oea)

# need to re-pad project_no with leading zeros for opcs awards 
unique(nchar(oea$Project.No.))
# oea %>% filter(nchar(Project.No.) == 8) %>% select(Project.No.) %>% head(.)
# oea %>% filter(nchar(Project.No.) == 9) %>% select(Project.No.) %>% head(.)
# oea %>% filter(nchar(Project.No.) == 10) %>% select(Project.No.) %>% head(.)
# oea %>% filter(nchar(Project.No.) == 11) %>% select(Project.No.) %>% head(.)
# oea %>% filter(nchar(Project.No.) == 14) %>% select(Project.No.) %>% head(.)

# rstudio flags code as error because it wants to add a closing parenthesis, but it's not needed
# to avoid error flags, leave it commented out before/after running code chunk
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

# head(oea2)

# select oea variables and convert to character
oea2 <- oea2 %>% rename(award_num = Project.No.) %>% 
        select(award_num, Applicant, EDA.Program, Press.Release.Project.Description) %>%
        mutate(Applicant = as.character(Applicant), EDA.Program = as.character(EDA.Program),
               Press.Release.Project.Description = as.character(Press.Release.Project.Description))
glimpse(oea2)

# combine oea data with other data of interest 
merged3 <- merged2
merged3 <- left_join(merged3, oea2, by = c("Project.No." = "award_num"))

# check how many opcs/gol awards could not be matched with oea info
# the oea data only when back to 2012, so there will be several years of NA
dim(merged2)
dim(merged3)
sum(!is.na(merged3$Press.Release.Project.Description))

# change names of oea variables, and then interpolate database values for years prior to 2012 without OEA descriptions
merged3 <- merged3 %>% rename(oea_program = EDA.Program, oea_applicant = Applicant, 
                              oea_description = Press.Release.Project.Description) %>%
        mutate(oea_program = case_when(is.na(.$oea_program) ~ .$Program, TRUE ~ .$oea_program),
           oea_applicant = case_when(is.na(.$oea_applicant) ~ .$Full.Applicant.Name, TRUE ~ .$oea_applicant),
           oea_description = case_when(is.na(.$oea_description) ~ .$Project.Short.Descrip, TRUE ~ .$oea_description))

# check for projects with oea descriptions still missing
# there are 367 from 2012/2013 (one from 2017) with NA oea_description, 
# but that's because they have NA project.short.descr - none are approved, can be ignored
sum(is.na(merged3$oea_description))
sum(is.na(merged3$Project.Short.Descrip))
merged3 %>% filter(is.na(oea_description)) %>% distinct(FY, Status, Project.Short.Descrip, oea_description)

merged3 %>% filter(is.na(Status)) %>% select(FY, Project.No., Project.Short.Descrip, Control.)


###############################################################


# test to check for duplicates
if(sum(duplicated(merged3$Control.)) == 0) {
        print("Good: There are no duplicate Control Numbers")
} else {
        stop("Error: There are duplicate Control Numbers")
}


#########################################


# check opcs count/amount is unchanged
merged3_opcs_sum <- merged3 %>% filter(database == "opcs") %>% summarize(count = n(), amount = dollar(sum(Best.EDA.., na.rm = TRUE)))
opcs_no_dup_sum <- opcs_no_dup %>% summarize(count = n(), amount = dollar(sum(as.numeric(`Best EDA $`), na.rm = TRUE)))

if(merged3_opcs_sum$count == opcs_no_dup_sum$count & merged3_opcs_sum$amount == opcs_no_dup_sum$amount) {
        print("Good: The merged3 counts/amounts match the opcs_no_dup counts/amounts")
} else {
        stop("Error: The merged3 counts/amounts do not match the opcs_no_dup counts/amounts")
}

# check gol count/amount is unchanged
merged3_gol_sum <- merged3 %>% filter(database == "gol") %>% summarize(count = n(), amount = dollar(sum(Best.EDA.., na.rm = TRUE)))
gol_awards <- gol %>% filter(AWARD_STATUS == "Signed and Complete") %>% 
        summarize(count = n(), award_amount = sum(AWARD_FED_SHARE, na.rm = TRUE))
gol_apps <- gol %>% filter(AWARD_STATUS != "Signed and Complete") %>% 
        summarize(count = n(), app_amount = sum(APP_FED_SHARE, na.rm = TRUE))
gol_sum <- data.frame(count = gol_awards$count + gol_apps$count, amount = dollar(gol_awards$award_amount + gol_apps$app_amount))

if(merged3_gol_sum$count == gol_sum$count & merged3_gol_sum$amount == gol_sum$amount) {
        print("Good: The merged3 counts/amounts match the gol counts/amounts")
} else {
        stop("Error: The merged3 counts/amounts do not match the gol counts/amounts")
}


####################################################################


# create csv filename for merged data
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
merged_filename <- str_c("master_data_", date2, ".csv")

# write.csv(merged, file = merged_filename, row.names = FALSE, fileEncoding = "UTF-8")
setwd("G:/PNP/Performance Measurement/master_data")
# setwd("C:/Users/sdevine/Desktop/master_data")
# setwd("C:/Users/mlofthus/Desktop")
write_csv(merged3, merged_filename)
