library(stringr)
library(dplyr)
library(lubridate)
library(readr)

# set working directory where files are saved
setwd("G:/PNP/Performance Measurement/master_data")

## read in and clean opcs data

# find current version of option1 file with oit unqueryable/truncated fields
# opcs file is saved as csv directly from impromptu
opcs_filename <- list.files()[str_detect(list.files(), "opcs_20")]

# read-in opcs data from impromptu
opcs <- read.csv(opcs_filename, stringsAsFactors = FALSE, colClasses = c("Control." = "character", 
                        "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                        "Initiatives" = "character", "Appl.Contact.Name" = "character", "Contact.Email" = "character"))

# pad leading zeroes back on zip codes 
opcs2 <- opcs
opcs2$Appl..Zip <- str_pad(opcs2$Appl..Zip, width = 5, side = "left", pad = "0")
opcs2$Proj.ZIP <- str_pad(opcs2$Proj.ZIP, width = 5, side = "left", pad = "0")

# change blanks for Cons.Non to NA
blank_index <- which(opcs2$Cons.Non == "")
opcs2$Cons.Non[blank_index] <- NA

# remove any duplicates
dup <- (duplicated(opcs2$Control.))
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
opcs3 <- opcs2[non_dup_index, ]

## read in and clean oit data

# find current version of option1 file with oit unqueryable/truncated fields
oit_filename <- list.files()[str_detect(list.files(), "oit_20")]

# read in oit data
oit <- read.csv(oit_filename, stringsAsFactors = FALSE, colClasses = c("CONTROL_NO" = "character"))

# select desired columns from oit
oit2 <- select(oit, CONTROL_NO, Proj.Comp..Code, Geographic.Need.Descr., Pressing.Need.Descr., General.Descr.,
                   Scope.of.Work, GNS.Descr., Economic.Impact.or.Benefit, Comments, X.DEC._Action.Code,
               X.DEC._Date, X.PPR._Action.Code, X.PPR._Date, X.PRD._Action.Code, X.PRD._Date, X.PCL._Date, 
               X.PPS._Date, X.PPE._Date, X.PX1._Date, X.PX2._Date, X.GSD._Date, X.GPE._Date )

# drop records with duplicate control # from oit_text
non_lead_applicant <- which(oit2$Proj.Comp..Code != 1)
oit3 <- oit2[-non_lead_applicant, ]

# should not be any duplicates after removing non-lead applicants, but just in case
dup <- (duplicated(oit3$CONTROL_NO))
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
oit4 <- oit3[non_dup_index, ]

# convert milestone dates to date format
oit4$X.PCL._Date <- dmy(oit4$X.PCL._Date, tz = "EST")
oit4$X.DEC._Date <- dmy(oit4$X.DEC._Date, tz = "EST")
oit4$X.PPR._Date <- dmy(oit4$X.PPR._Date, tz = "EST")
oit4$X.PRD._Date <- dmy(oit4$X.PRD._Date, tz = "EST")
oit4$X.PPS._Date <- dmy(oit4$X.PPS._Date, tz = "EST")
oit4$X.PPE._Date <- dmy(oit4$X.PPE._Date, tz = "EST")
oit4$X.PX1._Date <- dmy(oit4$X.PX1._Date, tz = "EST")
oit4$X.PX2._Date <- dmy(oit4$X.PX2._Date, tz = "EST")
oit4$X.GSD._Date <- dmy(oit4$X.GSD._Date, tz = "EST")
oit4$X.GPE._Date <- dmy(oit4$X.GPE._Date, tz = "EST")

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
gol <- read.csv(gol_filename, stringsAsFactors = FALSE, colClasses = c("SPEC_INIT_CODES" = "character",
                "APPLICANT_ZIP" = "character"), na.strings = c(""))

gol2 <- select(gol, PROGRAM_OFFICE, AWARD_NUMBER, APPLICATION_ID, APPLICANT_NAME, PROJECT_TITLE, RECEIVED_DT,  
               AWARD_FED_SHARE, AWARD_NONFED_SHARE, APP_FED_SHARE, APP_NONFED_SHARE, GO_SIGN_DT, CONSTRUCTION_AWARD, AWARD_STATUS.1, 
               COMPETITION_NAME, SPEC_INIT_CODES, APPLICANT_STREET, APPLICANT_CITY, APPLICANT_COUNTY, APPLICANT_STATE, APPLICANT_ZIP, 
               ESTIMATED_JOB_CREATED, ESTIMATED_JOB_SAVED, ESTIMATED_PRIVATE_INVESTMENT, AUTH_REP_EMAIL)

# compute FY, use GO_SIGN_DT if available, otherwise use RECEIVED_DT, will subset year in code below
gol2$FY <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$GO_SIGN_DT[row])) {gol2$RECEIVED_DT[row]} else 
        {gol2$GO_SIGN_DT[row]})

# convert gol dates to proper format
gol2$FY <- mdy_hm(gol2$FY)
gol2$FY <- year(gol2$FY)

# create empty dataframe with correct number of columns to fit merged and rows to house gol
gol3 <- as.data.frame(matrix(as.character(""), ncol = ncol(merged), nrow = nrow(gol2)))

# convert empty dataframe to class character
for(i in 1:ncol(gol3)){
        gol3[ , i] <- as.character(gol3[ , i])
}

# assign gol columns to empty dataframe to map with merged column numbers
gol3[ , which(names(merged) == "FY")] <- gol2$FY
gol3[ , which(names(merged) == "Appropriation")] <- gol2$PROGRAM_OFFICE
gol3[ , which(names(merged) == "Prog.Abbr")] <- gol2$PROGRAM_OFFICE
gol3[ , which(names(merged) == "Control.")] <- gol2$APPLICATION_ID
gol3[ , which(names(merged) == "Project.No.")] <- gol2$AWARD_NUMBER
gol3[ , which(names(merged) == "Appl.Short.Name")] <- gol2$APPLICANT_NAME
gol3[ , which(names(merged) == "Full.Applicant.Name")] <- gol2$APPLICANT_NAME
gol3[ , which(names(merged) == "Project.Short.Descrip")] <- gol2$PROJECT_TITLE
gol3[ , which(names(merged) == "X.PPR._Date")] <- gol2$RECEIVED_DT
gol3[ , which(names(merged) == "X.DEC._Date")] <- gol2$GO_SIGN_DT
gol3[ , which(names(merged) == "Cons.Non")] <- gol2$CONSTRUCTION_AWARD
gol3[ , which(names(merged) == "Status")] <- gol2$AWARD_STATUS.1
gol3[ , which(names(merged) == "Appr.Desc")] <- gol2$COMPETITION_NAME
gol3[ , which(names(merged) == "Prog.Tool.Name")] <- gol2$COMPETITION_NAME
gol3[ , which(names(merged) == "Initiatives")] <- gol2$SPEC_INIT_CODES
gol3[ , which(names(merged) == "Jobs.Created")] <- gol2$ESTIMATED_JOB_CREATED
gol3[ , which(names(merged) == "Jobs.Saved")] <- gol2$ESTIMATED_JOB_SAVED
gol3[ , which(names(merged) == "Private.Investment")] <- gol2$ESTIMATED_PRIVATE_INVESTMENT
gol3[ , which(names(merged) == "Appl.Street.Addr.1")] <- gol2$APPLICANT_STREET
gol3[ , which(names(merged) == "Appl.City.Name")] <- gol2$APPLICANT_CITY
gol3[ , which(names(merged) == "Appl.State.Abbr")] <- gol2$APPLICANT_STATE
gol3[ , which(names(merged) == "Appl..Zip")] <- gol2$APPLICANT_ZIP
gol3[ , which(names(merged) == "Contact.Email")] <- gol2$AUTH_REP_EMAIL
# compute Best.EDA.., Local.Applicant.., and Total.Project.. from award_fed_share if available, app_fed_share if not
gol3[ , which(names(merged) == "Best.EDA..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_FED_SHARE[row])) {gol2$APP_FED_SHARE[row]} else 
        {gol2$AWARD_FED_SHARE[row]})
gol3[ , which(names(merged) == "Local.Applicant..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_NONFED_SHARE[row])) {gol2$APP_NONFED_SHARE[row]} else 
        {gol2$AWARD_NONFED_SHARE[row]})
gol3[ , which(names(merged) == "Total.Project..")] <- sapply(1:nrow(gol2), function(row) if(is.na(gol2$AWARD_NONFED_SHARE[row])) {sum(gol2$APP_NONFED_SHARE[row], gol2$APP_FED_SHARE[row])} else 
        {sum(gol2$AWARD_NONFED_SHARE[row], gol2$AWARD_FED_SHARE[row])})

names(gol3) <- names(merged)

# check for duplicates
dup <- (duplicated(gol3$Control.))
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
gol3$X.PCL._Date[1] <- "1996-09-05"
gol3$X.PCL._Date <- ymd(gol3$X.PCL._Date)
gol3$X.PCL._Date[1] <- NA
gol3$X.PPS._Date[1] <- "1996-09-05"
gol3$X.PPS._Date <- ymd(gol3$X.PPS._Date)
gol3$X.PPS._Date[1] <- NA
gol3$X.PRD._Date[1] <- "1996-09-05"
gol3$X.PRD._Date <- ymd(gol3$X.PRD._Date)
gol3$X.PRD._Date[1] <- NA
gol3$X.PPE._Date[1] <- "1996-09-05"
gol3$X.PPE._Date <- ymd(gol3$X.PPE._Date)
gol3$X.PPE._Date[1] <- NA
gol3$X.PX1._Date[1] <- "1996-09-05"
gol3$X.PX1._Date <- ymd(gol3$X.PX1._Date)
gol3$X.PX1._Date[1] <- NA
gol3$X.PX2._Date[1] <- "1996-09-05"
gol3$X.PX2._Date <- ymd(gol3$X.PX2._Date)
gol3$X.PX2._Date[1] <- NA
gol3$X.GSD._Date[1] <- "1996-09-05"
gol3$X.GSD._Date <- ymd(gol3$X.GSD._Date)
gol3$X.GSD._Date[1] <- NA
gol3$X.GPE._Date[1] <- "1996-09-05"
gol3$X.GPE._Date <- ymd(gol3$X.GPE._Date)
gol3$X.GPE._Date[1] <- NA
gol3$X.DEC._Date <- mdy_hm(gol3$X.DEC._Date)
gol3$X.PPR._Date <- mdy_hm(gol3$X.PPR._Date)

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
                        merged$Program[i] <- "Research and National Technical Assistance"
                }
                if(merged$Program[i] == "EV"){
                        merged$Program[i] <- "Research and National Technical Assistance"
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
                if(merged$Prog.Abbr[i] == "RE"){
                        merged$Prog.Abbr[i] <- "RNTA"
                }
                if(merged$Prog.Abbr[i] == "EV"){
                        merged$Prog.Abbr[i] <- "RNTA"
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
write_csv(merged, merged_filename)
