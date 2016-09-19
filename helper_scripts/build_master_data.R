library(stringr)
library(dplyr)
library(lubridate)
library(readr)

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
                        "Initiatives" = "character", "Appl.Contact.Name" = "character", "Contact.Email" = "character", "DUNS.." = "character", 
                        "Local.Applicant.." = "character", "Total.Project.." = "character", "Best.EDA.." = "character", "Private.Investment" = "character"))

# pad leading zeroes back on zip codes 
opcs2 <- opcs
opcs2$Appl..Zip <- str_pad(opcs2$Appl..Zip, width = 5, side = "left", pad = "0")
opcs2$Proj.ZIP <- str_pad(opcs2$Proj.ZIP, width = 5, side = "left", pad = "0")

# convert dates to usable date format
opcs2$PCL.Date <- ymd_hm(opcs2$PCL.Date, tz = "EST")
opcs2$DEC.Date <- ymd_hm(opcs2$DEC.Date, tz = "EST")
opcs2$PPR.Date <- ymd_hm(opcs2$PPR.Date, tz = "EST")
opcs2$PRD.Date <- ymd_hm(opcs2$PRD.Date, tz = "EST")
opcs2$GSD.Date <- ymd_hm(opcs2$GSD.Date, tz = "EST")
opcs2$GPE.Date <- ymd_hm(opcs2$GPE.Date, tz = "EST")
opcs2$GPE.Date <- ymd_hm(opcs2$GPX.Date, tz = "EST")

# change blanks for Cons.Non to NA
blank_index <- which(opcs2$Cons.Non == "")
opcs2$Cons.Non[blank_index] <- NA

# pad Appl.FIPS.ST and Appl.Cong.Dist, and create Appl.State.Cong variable as unique congressional district identifier 
opcs2$Appl.FIPS.ST <- str_pad(opcs2$Appl.FIPS.ST, width = 2, side = "left", pad = "0")
opcs2$Appl.Cong.Dist <- str_pad(opcs2$Appl.Cong.Dist, width = 2, side = "left", pad = "0")
opcs2$Appl.State.Cong <- str_c(opcs2$Appl.FIPS.ST, opcs2$Appl.Cong.Dist)

# pad Proj.FIPS.ST and Proj.Cong.Dist, and create Proj.State.Cong variable as unique congressional district identifier 
opcs2$Proj.FIPS.ST <- str_pad(opcs2$Proj.FIPS.ST, width = 2, side = "left", pad = "0")
opcs2$Proj.Cong.Dist <- str_pad(opcs2$Proj.Cong.Dist, width = 2, side = "left", pad = "0")
opcs2$Proj.State.Cong <- str_c(opcs2$Appl.FIPS.ST, opcs2$Appl.Cong.Dist)

# remove any duplicates
dup <- (duplicated(opcs2$Control.))
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
opcs3 <- opcs2[non_dup_index, ]

# pad DUNS with zeroes and convert blanks to NA
errors_index <- which(nchar(opcs3$DUNS..) < 9 & nchar(opcs3$DUNS..) > 0)
opcs3$DUNS..[errors_index] <- str_pad(opcs3$DUNS..[errors_index], width = 9, side = "right", pad = "0")
for(i in 1:nrow(opcs3)) {
        if(opcs3$DUNS..[i] == "") {
                opcs3$DUNS..[i] <- NA
        }
}

# convert MSI.Indicator code into acronym
opcs3$MSI.Indicator <- as.character(opcs3$MSI.Indicator)
for(i in 1:nrow(opcs3)) {
        if(opcs3$MSI.Indicator[i] == "1" & !is.na(opcs3$MSI.Indicator[i])) {
                opcs3$MSI.Indicator[i] <- "HBCU"
        }
        if(opcs3$MSI.Indicator[i] == "2" & !is.na(opcs3$MSI.Indicator[i])) {
                opcs3$MSI.Indicator[i] <- "HSI"
        }
        if(opcs3$MSI.Indicator[i] == "3" & !is.na(opcs3$MSI.Indicator[i])) {
                opcs3$MSI.Indicator[i] <- "TCU"
        }
        if(opcs3$MSI.Indicator[i] == "4" & !is.na(opcs3$MSI.Indicator[i])) {
                opcs3$MSI.Indicator[i] <- "Other"
        }
        if(opcs3$MSI.Indicator[i] == "5" & !is.na(opcs3$MSI.Indicator[i])) {
                opcs3$MSI.Indicator[i] <- "AKHIPPI"
        }
}

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

# correct column classes for merged
merged$Report.Date.3.years <- ymd_hm(merged$Report.Date.3.years, tz = "EST")
merged$Report.Date.6.years <- ymd_hm(merged$Report.Date.6.years, tz = "EST")
merged$Report.Date.9.years <- ymd_hm(merged$Report.Date.9.years, tz = "EST")

## read in grants online data

# find current version of option1 file with oit unqueryable/truncated fields
gol_filename <- list.files()[str_detect(list.files(), "gol_20")]

# read in gol data
gol <- read_csv(gol_filename, col_types = list(AWARD_NUMBER = col_character(), APPLICATION_ID = col_character(), AWARD_FED_SHARE = col_number(),
                                               AWARD_NONFED_SHARE = col_number(), APP_FED_SHARE = col_number(), APP_NONFED_SHARE = col_number(),
                                               SPEC_INIT_CODES = col_character(), APPLICANT_ZIP = col_character(), ESTIMATED_PRIVATE_INVESTMENT = col_number(),
                                               DUNS_NUMBER = col_character())) %>% data.frame(.)

gol2 <- select(gol, LINE_OFFICE, PROGRAM_OFFICE, AWARD_NUMBER, APPLICATION_ID, APPLICANT_NAME, PROJECT_TITLE, RECEIVED_DT, PROJECT_DESC,
               AWARD_FED_SHARE, AWARD_NONFED_SHARE, APP_FED_SHARE, APP_NONFED_SHARE, GO_SIGN_DT, CONSTRUCTION_AWARD, GRANT_STATUS, RFA_NAME,
               COMPETITION_NAME, SPEC_INIT_CODES, APPLICANT_STREET, APPLICANT_CITY, APPLICANT_COUNTY, APPLICANT_STATE, APPLICANT_ZIP, 
               ESTIMATED_JOB_CREATED, ESTIMATED_JOB_SAVED, ESTIMATED_PRIVATE_INVESTMENT, AUTH_REP_EMAIL, CFDA_NUMBER, APPLICATION_STATUS, DUNS_NUMBER, MSI_CODE, APPROPRIATION_CODE)

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
gol2$FY <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$GO_SIGN_DT[row])) {gol2$RECEIVED_DT[row]} else 
        {gol2$GO_SIGN_DT[row]})

# convert gol dates to proper format
gol2$FY <- mdy_hm(gol2$FY)
gol2$FY <- year(gol2$FY)

# add state and county fips, and congressional district using gol Appl.Zip and HUD's quarterly crosswalk files
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html

# add state and county fips
zip_county_filename <- list.files()[str_detect(list.files(), "ZIP_COUNTY_")]
zip_county <- read_csv(zip_county_filename)
gol2$Appl.ZIP.4 <- str_sub(gol2$APPLICANT_ZIP, -4)
gol2$APPLICANT_ZIP <- str_sub(gol2$APPLICANT_ZIP, 1, 5)
gol2$app_fips_state_county <- NA
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
        if(!(is.na(gol2$LINE_OFFICE))){
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
gol3[ , which(names(merged) == "GNS.Descr.")] <- gol2$PROJECT_DESC
gol3[ , which(names(merged) == "Economic.Impact.or.Benefit")] <- gol2$PROJECT_DESC
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
gol3[ , which(names(merged) == "Contact.Email")] <- gol2$AUTH_REP_EMAIL
gol3[ , which(names(merged) == "Region.Name")] <- gol2$Region.Name
gol3[ , which(names(merged) == "DUNS..")] <- gol2$DUNS_NUMBER
gol3[ , which(names(merged) == "MSI.Indicator")] <- gol2$MSI_CODE
gol3[ , which(names(merged) == "Appr.Code")] <- gol2$APPROPRIATION_CODE

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
non_dup_index <- which(dup == FALSE)
gol3 <- gol3[non_dup_index, ]

# fill in database variable showing data origin
gol3$database <- "gol"

# convert gol Status "Accepted" to "Approved"
accepted_index <- which(gol3$Status == "Accepted")
gol3$Status[accepted_index] <- "Approved"

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
gol3$X.PPS._Date[1] <- "1996-09-05"
gol3$X.PPS._Date <- ymd(gol3$X.PPS._Date)
gol3$X.PPS._Date[1] <- NA
gol3$PRD.Date[1] <- "1996-09-05"
gol3$PRD.Date <- ymd(gol3$PRD.Date)
gol3$PRD.Date[1] <- NA
gol3$X.PPE._Date[1] <- "1996-09-05"
gol3$X.PPE._Date <- ymd(gol3$X.PPE._Date)
gol3$X.PPE._Date[1] <- NA
gol3$X.PX1._Date[1] <- "1996-09-05"
gol3$X.PX1._Date <- ymd(gol3$X.PX1._Date)
gol3$X.PX1._Date[1] <- NA
gol3$X.PX2._Date[1] <- "1996-09-05"
gol3$X.PX2._Date <- ymd(gol3$X.PX2._Date)
gol3$X.PX2._Date[1] <- NA
gol3$GSD.Date[1] <- "1996-09-05"
gol3$GSD.Date <- ymd(gol3$GSD.Date)
gol3$GSD.Date[1] <- NA
gol3$GPE.Date[1] <- "1996-09-05"
gol3$GPE.Date <- ymd(gol3$GPE.Date)
gol3$GPE.Date[1] <- NA
gol3$DEC.Date <- mdy_hm(gol3$DEC.Date)
gol3$PPR.Date <- mdy_hm(gol3$PPR.Date)

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
                        merged$Program[i] <- "Office of Innovation and Entrepreneurship"
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
