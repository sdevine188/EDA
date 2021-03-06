library(stringr)
library(dplyr)
library(lubridate)
library(readr)
library(lazyeval)

# set working directory where files are saved
setwd("G:/PNP/Performance Measurement/master_data")

## read in and clean opcs data

# turn-off scientific notation
options(scipen=999)

# find current version of option1 file with oit unqueryable/truncated fields
# opcs file is saved as csv directly from impromptu
opcs_filename <- list.files()[str_detect(list.files(), "opcs_20")]

# read-in opcs data from impromptu
opcs <- read.csv(opcs_filename, stringsAsFactors = FALSE, colClasses = c("Control." = "character",
                           "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                           "Initiatives" = "character", "Appl.Contact.Name" = "character", "Contact.Email" = "character", "DUNS.." = "character", "IRS.." = "character",
                           "Local.Applicant.." = "character", "Total.Project.." = "character", "Best.EDA.." = "character", "Private.Investment" = "character"), na.strings = c("", "NA"))

opcs2 <- opcs

# pad Appl.FIPS.ST and Appl.Cong.Dist, and create Appl.State.Cong variable as unique congressional district identifier 
opcs2$Appl.FIPS.ST <- str_pad(opcs2$Appl.FIPS.ST, width = 2, side = "left", pad = "0")
opcs2$Appl.Cong.Dist <- str_pad(opcs2$Appl.Cong.Dist, width = 2, side = "left", pad = "0")
opcs2$Appl.State.Cong <- str_c(opcs2$Appl.FIPS.ST, opcs2$Appl.Cong.Dist)

# pad Proj.FIPS.ST and Proj.Cong.Dist, and create Proj.State.Cong variable as unique congressional district identifier 
opcs2$Proj.FIPS.ST <- str_pad(opcs2$Proj.FIPS.ST, width = 2, side = "left", pad = "0")
opcs2$Proj.Cong.Dist <- str_pad(opcs2$Proj.Cong.Dist, width = 2, side = "left", pad = "0")
opcs2$Proj.State.Cong <- str_c(opcs2$Proj.FIPS.ST, opcs2$Proj.Cong.Dist)

# pad DUNS with zeroes
errors_index <- which(nchar(opcs2$DUNS..) < 9 & nchar(opcs2$DUNS..) > 0)
opcs2$DUNS..[errors_index] <- str_pad(opcs2$DUNS..[errors_index], width = 9, side = "right", pad = "0")

# pad leading zeroes back on zip codes
opcs2$Appl..Zip <- str_pad(opcs2$Appl..Zip, width = 5, side = "left", pad = "0")
opcs2$Proj.ZIP <- str_pad(opcs2$Proj.ZIP, width = 5, side = "left", pad = "0")
opcs2$Appl.ZIP.4 <- str_pad(opcs2$Appl.ZIP.4, width = 4, side = "left", pad = "0")

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
manual_lead <- opcs2 %>% filter(Control. == 96739 & Appl.Short.Name == "West TX A&M Univ" |
                                        Control. == 110646 & Appl.Short.Name == "Somerset County, PA" |
                                        Control. == 59400 & Appl.Short.Name == "Simi Valley, City of" |
                                        Control. == 59749 & Appl.Short.Name == "American Samoa Govt of" |
                                        Control. == 96924 & Appl.Short.Name == "Univ of Nevada, Reno")
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

# find and delete coapplicant records
opcs3 <- opcs2 %>% filter(is.na(coapp))

# convert dates to usable date format
opcs3$PCL.Date <- ymd_hm(opcs3$PCL.Date, tz = "EST")
opcs3$DEC.Date <- ymd_hm(opcs3$DEC.Date, tz = "EST")
opcs3$PPR.Date <- ymd_hm(opcs3$PPR.Date, tz = "EST")
opcs3$PRD.Date <- ymd_hm(opcs3$PRD.Date, tz = "EST")
opcs3$GSD.Date <- ymd_hm(opcs3$GSD.Date, tz = "EST")
opcs3$GPE.Date <- ymd_hm(opcs3$GPE.Date, tz = "EST")
opcs3$GPE.Date <- ymd_hm(opcs3$GPX.Date, tz = "EST")
opcs3$Report.Date.3.years <- ymd_hm(opcs3$Report.Date.3.years, tz = "EST")
opcs3$Report.Date.6.years <- ymd_hm(opcs3$Report.Date.6.years, tz = "EST")
opcs3$Report.Date.9.years <- ymd_hm(opcs3$Report.Date.9.years, tz = "EST")




## read in and clean oit data

# find current version of option1 file with oit unqueryable/truncated fields
oit_filename <- list.files()[str_detect(list.files(), "oit_20")]

# read in oit data
oit <- read.csv(oit_filename, stringsAsFactors = FALSE, colClasses = c("CONTROL_NO" = "character"))

# select desired columns from oit 
oit2 <- select(oit, CONTROL_NO, Proj.Comp..Code, Geographic.Need.Descr., Pressing.Need.Descr., General.Descr.,
               Scope.of.Work, GNS.Descr., Economic.Impact.or.Benefit, Comments,
               X.PPS._Date, X.PPE._Date, X.PX1._Date, X.PX2._Date)

# drop records with duplicate control # from oit_text
non_lead_applicant <- which(oit2$Proj.Comp..Code != 1)
oit3 <- oit2[-non_lead_applicant, ]

# should not be any duplicates after removing non-lead applicants, but just in case
dup <- (duplicated(oit3$CONTROL_NO))
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
oit4 <- oit3[non_dup_index, ]

# convert milestone dates to date format
oit4$X.PPS._Date <- dmy(oit4$X.PPS._Date, tz = "EST")
oit4$X.PPE._Date <- dmy(oit4$X.PPE._Date, tz = "EST")
oit4$X.PX1._Date <- dmy(oit4$X.PX1._Date, tz = "EST")
oit4$X.PX2._Date <- dmy(oit4$X.PX2._Date, tz = "EST")

## merge opcs and oit 


# left_join oit with opcs by control #
merged <- left_join(opcs3, oit4, by = c("Control." = "CONTROL_NO"))

# add database variable indicating what database data came from (opcs vs gol)
merged$database <- "opcs"



## read in grants online data

# find current version of option1 file with oit unqueryable/truncated fields
gol_filename <- list.files()[str_detect(list.files(), "gol_20")]

# read in gol data
gol <- read_csv(gol_filename, col_types = list(AWARD_NUMBER = col_character(), APPLICATION_ID = col_character(), AWARD_FED_SHARE = col_number(),
                                               AWARD_NONFED_SHARE = col_number(), APP_FED_SHARE = col_number(), APP_NONFED_SHARE = col_number(),
                                               SPEC_INIT_CODES = col_character(), APPLICANT_ZIP = col_character(), ESTIMATED_PRIVATE_INVESTMENT = col_number(),
                                               DUNS_NUMBER = col_character())) %>% data.frame(.)

gol2 <- select(gol, LINE_OFFICE, PROGRAM_OFFICE, AWARD_NUMBER, APPLICATION_ID, APPLICANT_NAME, PROJECT_TITLE, RECEIVED_DT, FUNDING_FY, PROJECT_DESC,
               AWARD_FED_SHARE, AWARD_NONFED_SHARE, APP_FED_SHARE, APP_NONFED_SHARE, GO_SIGN_DT, CONSTRUCTION_AWARD, GRANT_STATUS, RFA_NAME,
               COMPETITION_NAME, SPEC_INIT_CODES, APPLICANT_STREET, APPLICANT_CITY, APPLICANT_COUNTY, APPLICANT_STATE, APPLICANT_ZIP, 
               ESTIMATED_JOB_CREATED, ESTIMATED_JOB_SAVED, ESTIMATED_PRIVATE_INVESTMENT, APPLICATION_CONTACT, CONTACT_PHONE, CONTACT_EMAIL,
               CFDA_NUMBER, APPLICATION_STATUS, DUNS_NUMBER, MSI_CODE, APPROPRIATION_CODE, ASSIGNED_FPO,
               EIN_NUMBER, FIPS_CITY_CD, FIPS_COUNTY_CD, FIPS_STATE_CD, PROJECT_START_DT, PROJECT_END_DT, AWARD_START_DT, AWARD_END_DT,
               FIRST_FALD_DT, PROJECT_BENEFIT_AREA, PROJECT_NEED, ANTICIP_ECO_BENEFIT)

# correct DUNS that have four trailing zeroes
duns_errors <- which(nchar(gol2$DUNS_NUMBER) == 13)
# test that all duns with 13 digits have four trailing zeroes before removing them
if(length(duns_errors) == length(which(str_sub(gol2$DUNS_NUMBER[duns_errors], start = 10, end = 13) == "0000"))) {
        gol2$DUNS_NUMBER[duns_errors] <- sapply(gol2$DUNS_NUMBER[duns_errors], function(x) { str_replace(x, "0000", "") })
}

# if grant_status is NA, interpolate with application_status
for(i in 1:nrow(gol2)){
        if(is.na(gol2$GRANT_STATUS[i])){
                gol2$GRANT_STATUS[i] <- gol2$APPLICATION_STATUS[i]
        }
}

# compute FY, use GO_SIGN_DT if available, otherwise use RECEIVED_DT, will subset year in code below
# gol2$FY <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$GO_SIGN_DT[row])) {gol2$RECEIVED_DT[row]} else 
# {gol2$GO_SIGN_DT[row]})
gol2$FY <- gol2$FUNDING_FY

# convert gol dates to proper format
# gol2$FY <- mdy_hm(gol2$FY)
# gol2$FY <- year(gol2$FY)

# add state and county fips, and congressional district using gol Appl.Zip and HUD's quarterly crosswalk files
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html

# add state and county fips
# note fips are imputed from appl.zip code for those records without a FIPS_STATE_CD or FIPS_COUNTY_CD listed already
zip_county_filename <- list.files()[str_detect(list.files(), "ZIP_COUNTY_")]
zip_county <- read_csv(zip_county_filename)
gol2$Appl.ZIP.4 <- str_sub(gol2$APPLICANT_ZIP, -4)
gol2$APPLICANT_ZIP <- str_sub(gol2$APPLICANT_ZIP, 1, 5)
gol2$app_fips_state_county <- NA
# note there are about 15 records with zip that is not in hud list, though 3 are NA
# gol2$APPLICANT_ZIP[which(!(gol2$APPLICANT_ZIP %in% zip_county$ZIP))]
for(i in 1:nrow(gol2)){
        if(!(is.na(gol2$APPLICANT_ZIP[i]))){
                if(gol2$APPLICANT_ZIP[i] %in% zip_county$ZIP){
                        zip_match_index <- which(zip_county$ZIP == gol2$APPLICANT_ZIP[i])
                        # note there will likely be some warnings, since some zip codes span multiple counties
                        # it will just assign the first county in the list though, which isnt great, but better than all NAs
                        gol2$app_fips_state_county[i] <- zip_county$COUNTY[zip_match_index]
                }
        }
}
gol2$app_fips_state_county <- str_pad(gol2$app_fips_state_county, width = 5, side = "left", pad = "0")
gol2$Appl.FIPS.State <- str_sub(gol2$app_fips_state_county, 1, 2)
gol2$Appl.FIPS.County <- str_sub(gol2$app_fips_state_county, 3, 5)

# overwrite with FIPS_STATE_CD or FIPS_COUNTY_CD if available
for(i in 1:nrow(gol2)) {
        if(!(is.na(gol2$FIPS_STATE_CD[i]))) {
                gol2$Appl.FIPS.State[i] <- gol2$FIPS_STATE_CD[i]
        }
        if(!(is.na(gol2$FIPS_COUNTY_CD[i]))) {
                gol2$Appl.FIPS.County[i] <- gol2$FIPS_COUNTY_CD[i]
        }
}

# add county names
# using 2010 counties & fips from census https://www.census.gov/geo/reference/codes/cou.html
counties <- read_csv("us_counties.txt", col_names = c("state", "fips_state", "fips_county", "county", "some_variable"))
counties$fips_state_county <- str_c(counties$fips_state, counties$fips_county)

# test to see if any gol2 counties are not in census list of counties
match <- which(gol2$app_fips_state_county %in% counties$fips_state_county)
non_match <- gol2[-match, ]
non_match %>% select(app_fips_state_county)

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
        }
}

# add congressional district
zip_cd_filename <- list.files()[str_detect(list.files(), "ZIP_CD_")]
# will be problem warnings since some CD values are ** for some reason,
# these will be marked to NA, which is probably ok since all CDs with NA have ZIPs that are repeated
# see test below
zip_cd <- read_csv(zip_cd_filename)
gol2$Appl.Cong.Dist <- NA
gol2$Appl.State.Cong <- NA
for(i in 1:nrow(gol2)){
        if(!(is.na(gol2$APPLICANT_ZIP[i]))){
                if(gol2$APPLICANT_ZIP[i] %in% zip_cd$ZIP){
                        zip_match_index <- which(zip_cd$ZIP == gol2$APPLICANT_ZIP[i])
                        # note there will likely be some warnings, since some zip codes span multiple counties
                        # it will just assign the first county in the list though, which isnt great, but better than all NAs
                        gol2$Appl.State.Cong[i] <- str_pad(zip_cd$CD[zip_match_index], width = 4, side = "left", pad = "0") 
                        gol2$Appl.Cong.Dist[i] <- str_sub(zip_cd$CD[zip_match_index], start = -2)
                }
        }
}

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
                if(gol2$LINE_OFFICE[i] == "HQ"){
                        gol2$Region.Name[i] <- "Headquarters"
                }
        }
}

# test to confirm that NA CDs have ZIPs that are repeated, and so the ZIPs will still be assigned
# x <- problems(zip_cd)
# zip_na <- zip_cd[x$row, ] %>% select(ZIP)
# dups <- sapply(1:nrow(zip_na), function(x) length(which(zip_cd$ZIP == zip_na$ZIP[x])))

# create empty dataframe with correct number of columns to fit merged and rows to house gol
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
gol3[ , which(names(merged) == "Status")] <- gol2$GRANT_STATUS
gol3[ , which(names(merged) == "Control.")] <- gol2$APPLICATION_ID
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
gol3[ , which(names(merged) == "Prog.Tool.Name")] <- gol2$RFA_NAME
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
gol3[ , which(names(merged) == "Appl.State.Cong")] <- gol2$Appl.State.Cong
gol3[ , which(names(merged) == "Proj.City.Name")] <- gol2$APPLICANT_CITY
gol3[ , which(names(merged) == "Proj.ST.Abbr")] <- gol2$APPLICANT_STATE
gol3[ , which(names(merged) == "Proj.ZIP")] <- gol2$APPLICANT_ZIP
gol3[ , which(names(merged) == "Proj.FIPS.ST")] <- gol2$Appl.FIPS.State
gol3[ , which(names(merged) == "Proj.FIPS.Cnty")] <- gol2$Appl.FIPS.County
gol3[ , which(names(merged) == "Proj.County.Name")] <- gol2$Appl.Cnty.Name 
gol3[ , which(names(merged) == "Proj.Cong.Dist")] <- gol2$Appl.Cong.Dist
gol3[ , which(names(merged) == "Proj.State.Cong")] <- gol2$Appl.State.Cong
gol3[ , which(names(merged) == "Contact.Email")] <- gol2$CONTACT_EMAIL
gol3[ , which(names(merged) == "Appl.Cont.Phone")] <- gol2$CONTACT_PHONE
gol3[ , which(names(merged) == "Appl.Contact.Name")] <- gol2$APPLICATION_CONTACT
gol3[ , which(names(merged) == "Region.Name")] <- gol2$Region.Name
gol3[ , which(names(merged) == "DUNS..")] <- gol2$DUNS_NUMBER
gol3[ , which(names(merged) == "MSI.Indicator")] <- gol2$MSI_CODE
gol3[ , which(names(merged) == "Appr.Code")] <- gol2$APPROPRIATION_CODE
gol3[ , which(names(merged) == "IRS..")] <- gol2$EIN_NUMBER
gol3[ , which(names(merged) == "Appl.FIPS.City")] <- gol2$FIPS_CITY_CD
# gol3[ , which(names(merged) == "Appl.FIPS.ST")] <- gol2$PRIMARY_NAICS
# gol3[ , which(names(merged) == "Appl.FIPS.ST")] <- gol2$GLOBAL_PARENT_DUNS_NUMBER
gol3[ , which(names(merged) == "X.PPS._Date")] <- gol2$PROJECT_START_DT
gol3[ , which(names(merged) == "X.PPE._Date")] <- gol2$PROJECT_END_DT
gol3[ , which(names(merged) == "GSD.Date")] <- gol2$AWARD_START_DT
gol3[ , which(names(merged) == "GPE.Date")] <- gol2$AWARD_END_DT
gol3[ , which(names(merged) == "PRD.Date")] <- gol2$FIRST_FALD_DT
gol3[ , which(names(merged) == "EDA.Official.Name")] <- gol2$ASSIGNED_FPO
gol3[ , which(names(merged) == "Entity.Code")] <- gol2$PROJECT_BENEFIT_AREA




# compute Best.EDA.., Local.Applicant.., and Total.Project.. from award_fed_share if available, app_nonfed_share if not
gol3[ , which(names(merged) == "Best.EDA..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_FED_SHARE[row])) {gol2$APP_FED_SHARE[row]} else 
        {gol2$AWARD_FED_SHARE[row]})
gol3[ , which(names(merged) == "Local.Applicant..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_NONFED_SHARE[row])) {gol2$APP_NONFED_SHARE[row]} else 
        {gol2$AWARD_NONFED_SHARE[row]})
gol3[ , which(names(merged) == "Total.Project..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_NONFED_SHARE[row])) {sum(gol2$APP_NONFED_SHARE[row], gol2$APP_FED_SHARE[row])} else 
        {sum(gol2$AWARD_NONFED_SHARE[row], gol2$AWARD_FED_SHARE[row])})

names(gol3) <- names(merged)

# check for duplicates
dup <- duplicated(gol3$Control.)
dup_index <- which(dup == TRUE)
print(str_c("there were ", length(dup_index), " duplicates removed"))
non_dup_index <- which(dup == FALSE)
gol3 <- gol3[non_dup_index, ]

# fill in database variable showing data origin
gol3$database <- "gol"

# convert gol Status "Accepted" to "Approved"
accepted_index <- which(gol3$Status == "Accepted")
gol3$Status[accepted_index] <- "Approved"

# change Grant Status of "Expired" or "Expired - De-obligated" to Approved
# these were Approved, but their Period of Performance has run out and they are finished
gol3 <- gol3 %>% mutate(Status = case_when(.$Status %in% c("Expired", "Expired - De-obligated") ~ "Approved",
                                                 TRUE ~ .$Status))

# need to convert gol data fields to posix in order to rbind without error
gol3$Report.Date.3.years[1] <- "1996-09-05"
gol3$Report.Date.3.years <- ymd(gol3$Report.Date.3.years[1])
gol3$Report.Date.3.years[1] <- NA
gol3$Report.Date.6.years[1] <- "1996-09-05"
gol3$Report.Date.6.years <- ymd(gol3$Report.Date.6.years[1])
gol3$Report.Date.6.years[1] <- NA
gol3$Report.Date.9.years[1] <- "1996-09-05"
gol3$Report.Date.9.years <- ymd(gol3$Report.Date.9.years[1])
gol3$Report.Date.9.years[1] <- NA
gol3$PCL.Date[1] <- "1996-09-05"
gol3$PCL.Date <- ymd(gol3$PCL.Date)
gol3$PCL.Date[1] <- NA
# gol3$X.PPS._Date[1] <- "1996-09-05"
# gol3$X.PPS._Date <- ymd(gol3$X.PPS._Date)
# gol3$X.PPS._Date[1] <- NA
# gol3$PRD.Date[1] <- "1996-09-05"
# gol3$PRD.Date <- ymd(gol3$PRD.Date)
# gol3$PRD.Date[1] <- NA
# gol3$X.PPE._Date[1] <- "1996-09-05"
# gol3$X.PPE._Date <- ymd(gol3$X.PPE._Date)
# gol3$X.PPE._Date[1] <- NA
gol3$X.PX1._Date[1] <- "1996-09-05"
gol3$X.PX1._Date <- ymd(gol3$X.PX1._Date)
gol3$X.PX1._Date[1] <- NA
gol3$X.PX2._Date[1] <- "1996-09-05"
gol3$X.PX2._Date <- ymd(gol3$X.PX2._Date)
gol3$X.PX2._Date[1] <- NA
# gol3$GSD.Date[1] <- "1996-09-05"
# gol3$GSD.Date <- ymd(gol3$GSD.Date)
# gol3$GSD.Date[1] <- NA
# gol3$GPE.Date[1] <- "1996-09-05"
# gol3$GPE.Date <- ymd(gol3$GPE.Date)
# gol3$GPE.Date[1] <- NA
gol3$DEC.Date <- mdy_hm(gol3$DEC.Date)
gol3$PPR.Date <- mdy_hm(gol3$PPR.Date)
gol3$X.PPS._Date <- mdy_hm(gol3$X.PPS._Date)
gol3$X.PPE._Date <- mdy_hm(gol3$X.PPE._Date)
gol3$GSD.Date <- mdy_hm(gol3$GSD.Date)
gol3$GPE.Date <- mdy_hm(gol3$GPE.Date)
gol3$PRD.Date <- mdy_hm(gol3$PRD.Date)

# rbind gol data to merged oit and opcs data
merged <- rbind(merged, gol3)

# create Program variable spelling out the Prog.Abbr variable
merged$Program <- merged$Prog.Abbr

for(i in 1:nrow(merged)){
        if(!(is.na(merged$Program[i]))){
                if(merged$Program[i] == "PW"){
                        merged$Program[i] <- "Public Works"
                }
                if(merged$Program[i] == "PL"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA"){
                        merged$Program[i] <- "Technical Assistance"
                }
                if(merged$Program[i] == "T9"){
                        merged$Program[i] <- "Economic Adjustment Assistance"
                }
                if(merged$Program[i] == "TJ"){
                        merged$Program[i] <- "Trade Adjustment Assistance for Firms"
                }
                if(merged$Program[i] == "RE"){
                        merged$Program[i] <- "Research"
                }
                if(merged$Program[i] == "EV"){
                        merged$Program[i] <- "Evaluation"
                }
                if(merged$Program[i] == "PL-ATRO"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA-ATRO"){
                        merged$Program[i] <- "Technical Assistance"
                }
                if(merged$Program[i] == "PL-AURO"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA-AURO"){
                        merged$Program[i] <- "Technical Assistance"
                }
                if(merged$Program[i] == "PL-CRO"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA-CRO"){
                        merged$Program[i] <- "Technical Assistance"
                }
                if(merged$Program[i] == "PL-DRO"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA-DRO"){
                        merged$Program[i] <- "Technical Assistance"
                }
                if(merged$Program[i] == "OIE"){
                        merged$Program[i] <- "Regional Innovation Strategies"
                }
                if(merged$Program[i] == "RNTA"){
                        merged$Program[i] <- "Research and National Technical Assistance"
                }
                if(merged$Program[i] == "TAAF"){
                        merged$Program[i] <- "Trade Adjustment Assistance for Firms"
                }
                if(merged$Program[i] == "PL-PRO"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA-PRO"){
                        merged$Program[i] <- "Technical Assistance"
                }
                if(merged$Program[i] == "PL-SRO"){
                        merged$Program[i] <- "Planning"
                }
                if(merged$Program[i] == "TA-SRO"){
                        merged$Program[i] <- "Technical Assistance"
                }
        }
}

# standardize the Prog.Abbr variable across opcs and gol
for(i in 1:nrow(merged)){
        if(!(is.na(merged$Prog.Abbr[i]))){
                if(merged$Prog.Abbr[i] == "T9"){
                        merged$Prog.Abbr[i] <- "EAA"
                }
                if(merged$Prog.Abbr[i] == "TJ"){
                        merged$Prog.Abbr[i] <- "TAAF"
                }
                if(merged$Prog.Abbr[i] == "PL-ATRO"){
                        merged$Prog.Abbr[i] <- "PL"
                }
                if(merged$Prog.Abbr[i] == "TA-ATRO"){
                        merged$Prog.Abbr[i] <- "TA"
                }
                if(merged$Prog.Abbr[i] == "PL-AURO"){
                        merged$Prog.Abbr[i] <- "PL"
                }
                if(merged$Prog.Abbr[i] == "TA-AURO"){
                        merged$Prog.Abbr[i] <- "TA"
                }
                if(merged$Prog.Abbr[i] == "PL-CRO"){
                        merged$Prog.Abbr[i] <- "PL"
                }
                if(merged$Prog.Abbr[i] == "TA-CRO"){
                        merged$Prog.Abbr[i] <- "TA"
                }
                if(merged$Prog.Abbr[i] == "PL-DRO"){
                        merged$Prog.Abbr[i] <- "PL"
                }
                if(merged$Prog.Abbr[i] == "TA-DRO"){
                        merged$Prog.Abbr[i] <- "TA"
                }
                if(merged$Prog.Abbr[i] == "PL-PRO"){
                        merged$Prog.Abbr[i] <- "PL"
                }
                if(merged$Prog.Abbr[i] == "TA-PRO"){
                        merged$Prog.Abbr[i] <- "TA"
                }
                if(merged$Prog.Abbr[i] == "PL-SRO"){
                        merged$Prog.Abbr[i] <- "PL"
                }
                if(merged$Prog.Abbr[i] == "TA-SRO"){
                        merged$Prog.Abbr[i] <- "TA"
                }
                if(merged$Prog.Abbr[i] == "OIE"){
                        merged$Prog.Abbr[i] <- "RIS"
                }
        }
}

# create csv filename for merged data
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
merged_filename <- str_c("master_data_", date2, ".csv")

# write.csv(merged, file = merged_filename, row.names = FALSE, fileEncoding = "UTF-8")
setwd("C:/Users/sdevine/Desktop/master_data")
# setwd("C:/Users/mlofthus/Desktop")
write_csv(merged, merged_filename)
