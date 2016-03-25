library(stringr)
library(dplyr)
library(lubridate)

# set working directory where files are saved
setwd("G:/PNP/Performance Measurement/master_data")


## read in and clean opcs data

# find current version of option1 file with oit unqueryable/truncated fields
opcs_filename <- list.files()[str_detect(list.files(), "opcs_20")]

# read-in opcs data from impromptu
opcs <- read.csv(opcs_filename, stringsAsFactors = FALSE, colClasses = c("Control." = "character", 
                        "Project.No." = "character", "Proj.ZIP" = "character", "Appl..Zip" = "character",
                        "Initiatives" = "character"))

# pad leading zeroes back on zip codes and single initiative codes
opcs2 <- opcs
opcs2$Appl..Zip <- str_pad(opcs2$Appl..Zip, width = 5, side = "left", pad = "0")
opcs2$Proj.ZIP <- str_pad(opcs2$Proj.ZIP, width = 5, side = "left", pad = "0")

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
               X.DEC._Date, X.PPR._Action.Code, X.PPR._Date, X.PRD._Action.Code, X.PRD._Date)

# drop records with duplicate control # from oit_text
non_lead_applicant <- which(oit2$Proj.Comp..Code != 1)
oit3 <- oit2[-non_lead_applicant, ]

# should not be any duplicates after removing non-lead applicants, but just in case
dup <- (duplicated(oit3$CONTROL_NO))
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
oit4 <- oit3[non_dup_index, ]

# convert milestone dates to date format
oit4$X.DEC._Date <- dmy(oit4$X.DEC._Date, tz = "EST")
oit4$X.PPR._Date <- dmy(oit4$X.PPR._Date, tz = "EST")
oit4$X.PRD._Date <- dmy(oit4$X.PRD._Date, tz = "EST")

## merge opcs and oit 


# left_join oit with opcs by control #
merged <- left_join(opcs3, oit4, by = c("Control." = "CONTROL_NO"))

# add database variable indicating what database data came from (opcs vs gol)
merged$database <- "opcs"

# correct column classes for merged
merged$Report.Date.3.years <- ymd_hm(merged$Report.Date.3.years, tz = "EST")
merged$Report.Date.6.years <- ymd_hm(merged$Report.Date.6.years, tz = "EST")
merged$Report.Date.9.years <- ymd_hm(merged$Report.Date.9.years, tz = "EST")
merged$Prog. <- as.factor(merged$Prog.)
merged$Total.Project.. <- as.numeric(merged$Total.Project..)
merged$Jobs.Created <- as.numeric(merged$Jobs.Created)
merged$Jobs.Saved <- as.numeric(merged$Jobs.Saved)
merged$Best.EDA.. <- as.numeric(merged$Best.EDA..)
merged$Priv.Inv.at.3.years <- as.numeric(merged$Priv.Inv.at.3.years)
merged$Jobs.Created.at.3.years <- as.numeric(merged$Jobs.Created.at.3.years)
merged$Jobs.Retained.at.3.years <- as.numeric(merged$Jobs.Retained.at.3.years)
merged$Priv.Inv.at.6.years <- as.numeric(merged$Priv.Inv.at.6.years)
merged$Jobs.Created.at.6.years <- as.numeric(merged$Jobs.Created.at.6.years)
merged$Jobs.Retained.at.6.years <- as.numeric(merged$Jobs.Retained.at.6.years)
merged$Priv.Inv.at.9.years <- as.numeric(merged$Priv.Inv.at.9.years)
merged$Jobs.Created.at.9.years <- as.numeric(merged$Jobs.Created.at.9.years)
merged$Jobs.Retained.at.9.years <- as.numeric(merged$Jobs.Retained.at.9.years)
merged$SIC1 <- as.numeric(merged$SIC1)
merged$SIC2 <- as.numeric(merged$SIC2)
merged$SIC3 <- as.numeric(merged$SIC3)
merged$FY <- as.numeric(merged$FY)
merged$RO.. <- as.numeric(merged$RO..)
merged$Proj.FIPS.ST <- as.numeric(merged$Proj.FIPS.ST)
merged$Proj.FIPS.Cnty <- as.numeric(merged$Proj.FIPS.Cnty)
merged$Proj.FIPS.City <- as.numeric(merged$Proj.FIPS.City)
merged$Appl.FIPS.ST <- as.numeric(merged$Appl.FIPS.ST)
merged$Appl.FIPS.City <- as.numeric(merged$Appl.FIPS.City)
merged$Appl.FIPS.Cnty <- as.numeric(merged$Appl.FIPS.Cnty)
merged$Appl.Cong.Dist <- as.numeric(merged$Appl.Cong.Dist)
merged$Proj.Cong.Dist <- as.numeric(merged$Proj.Cong.Dist)
merged$Proj.Cong.State <- as.numeric(merged$Proj.Cong.State)
merged$Proj.Comp..Code <- as.numeric(merged$Proj.Comp..Code)


## read in grants online data

# find current version of option1 file with oit unqueryable/truncated fields
gol_filename <- list.files()[str_detect(list.files(), "gol_20")]

# read in oit data
gol <- read.csv(gol_filename, stringsAsFactors = FALSE, colClasses = c("SPEC_INIT_CODES" = "character",
                "APPLICANT_ZIP" = "character"))

gol2 <- select(gol, PROGRAM_OFFICE, AWARD_NUMBER, APPLICANT_NAME, PROJECT_TITLE, RECEIVED_DT,  
               AWARD_FED_SHARE, GO_SIGN_DT, CONSTRUCTION_AWARD, AWARD_STATUS.1, 
               COMPETITION_NAME, SPEC_INIT_CODES, APPLICANT_STREET, APPLICANT_CITY, APPLICANT_COUNTY, APPLICANT_STATE, APPLICANT_ZIP, 
               ESTIMATED_JOB_CREATED, ESTIMATED_JOB_SAVED, ESTIMATED_PRIVATE_INVESTMENT)

# convert gol dates to proper format
gol2$GO_SIGN_DT <- mdy_hm(gol2$GO_SIGN_DT)
gol2$GO_SIGN_DT <- round_date(gol2$GO_SIGN_DT, "day")

gol2$RECEIVED_DT <- mdy_hm(gol2$RECEIVED_DT)
gol2$RECEIVED_DT <- round_date(gol2$RECEIVED_DT, "day")

# create empty dataframe with correct number of columns to fit merged and rows to house gol
gol3 <- as.data.frame(matrix(as.character(""), ncol = ncol(merged), nrow = nrow(gol2)))

# convert empty dataframe to class character
for(i in 1:ncol(gol3)){
        gol3[ , i] <- as.character(gol3[ , i])
}

# assign gol columns to empty dataframe to map with merged column numbers
gol3[ , which(names(merged) == "Appropriation")] <- gol2$PROGRAM_OFFICE
gol3[ , which(names(merged) == "Control.")] <- gol2$AWARD_NUMBER
gol3[ , which(names(merged) == "Project.No.")] <- gol2$AWARD_NUMBER
gol3[ , which(names(merged) == "Appl.Short.Name")] <- gol2$APPLICANT_NAME
gol3[ , which(names(merged) == "Full.Applicant.Name")] <- gol2$APPLICANT_NAME
gol3[ , which(names(merged) == "Project.Short.Descrip")] <- gol2$PROJECT_TITLE
gol3[ , which(names(merged) == "X.PRD._Date")] <- gol2$RECEIVED_DT
gol3[ , which(names(merged) == "Best.EDA..")] <- gol2$AWARD_FED_SHARE
gol3[ , which(names(merged) == "X.DEC._Date")] <- gol2$GO_SIGN_DT
gol3[ , which(names(merged) == "Cons.Non")] <- gol2$CONSTRUCTION_AWARD
gol3[ , which(names(merged) == "Status")] <- gol2$AWARD_STATUS.1
gol3[ , which(names(merged) == "Appr.Desc")] <- gol2$COMPETITION_NAME
gol3[ , which(names(merged) == "Initiatives")] <- gol2$SPEC_INIT_CODES
gol3[ , which(names(merged) == "Jobs.Created")] <- gol2$ESTIMATED_JOB_CREATED
gol3[ , which(names(merged) == "Jobs.Saved")] <- gol2$ESTIMATED_JOB_SAVED
gol3[ , which(names(merged) == "Private.Investment")] <- gol2$ESTIMATED_PRIVATE_INVESTMENT
gol3[ , which(names(merged) == "Appl.Street.Addr.1")] <- gol2$APPLICANT_STREET
gol3[ , which(names(merged) == "Appl.City.Name")] <- gol2$APPLICANT_CITY
gol3[ , which(names(merged) == "Appl.State.Abbr")] <- gol2$APPLICANT_STATE
gol3[ , which(names(merged) == "Appl..Zip")] <- gol2$APPLICANT_ZIP


names(gol3) <- names(merged)

# fill in database variable showing data origin
gol3$database <- "gol"

# syncronize colClasses between gol and merged
gol3$Prog. <- as.factor(gol3$Prog.)
gol3$Total.Project.. <- as.numeric(gol3$Total.Project..)
gol3$Jobs.Created <- as.numeric(gol3$Jobs.Created)
gol3$Jobs.Saved <- as.numeric(gol3$Jobs.Saved)
gol3$Best.EDA.. <- as.numeric(gol3$Best.EDA..)
gol3$Priv.Inv.at.3.years <- as.numeric(gol3$Priv.Inv.at.3.years)
gol3$Jobs.Created.at.3.years <- as.numeric(gol3$Jobs.Created.at.3.years)
gol3$Jobs.Retained.at.3.years <- as.numeric(gol3$Jobs.Retained.at.3.years)
gol3$Priv.Inv.at.6.years <- as.numeric(gol3$Priv.Inv.at.6.years)
gol3$Jobs.Created.at.6.years <- as.numeric(gol3$Jobs.Created.at.6.years)
gol3$Jobs.Retained.at.6.years <- as.numeric(gol3$Jobs.Retained.at.6.years)
gol3$Priv.Inv.at.9.years <- as.numeric(gol3$Priv.Inv.at.9.years)
gol3$Jobs.Created.at.9.years <- as.numeric(gol3$Jobs.Created.at.9.years)
gol3$Jobs.Retained.at.9.years <- as.numeric(gol3$Jobs.Retained.at.9.years)
gol3$SIC1 <- as.numeric(gol3$SIC1)
gol3$SIC2 <- as.numeric(gol3$SIC2)
gol3$SIC3 <- as.numeric(gol3$SIC3)
gol3$FY <- as.numeric(gol3$FY)
gol3$RO.. <- as.numeric(gol3$RO..)
gol3$Proj.FIPS.ST <- as.numeric(gol3$Proj.FIPS.ST)
gol3$Proj.FIPS.Cnty <- as.numeric(gol3$Proj.FIPS.Cnty)
gol3$Proj.FIPS.City <- as.numeric(gol3$Proj.FIPS.City)
gol3$Appl.FIPS.ST <- as.numeric(gol3$Appl.FIPS.ST)
gol3$Appl.FIPS.City <- as.numeric(gol3$Appl.FIPS.City)
gol3$Appl.FIPS.Cnty <- as.numeric(gol3$Appl.FIPS.Cnty)
gol3$Appl.Cong.Dist <- as.numeric(gol3$Appl.Cong.Dist)
gol3$Proj.Cong.Dist <- as.numeric(gol3$Proj.Cong.Dist)
gol3$Proj.Cong.State <- as.numeric(gol3$Proj.Cong.State)
gol3$Proj.Comp..Code <- as.numeric(gol3$Proj.Comp..Code)
gol3$Report.Date.3.years[1] <- "1996-09-05"
gol3$Report.Date.3.years <- ymd(gol3$Report.Date.3.years[1])
gol3$Report.Date.3.years[1] <- NA
gol3$Report.Date.6.years[1] <- "1996-09-05"
gol3$Report.Date.6.years <- ymd(gol3$Report.Date.6.years[1])
gol3$Report.Date.6.years[1] <- NA
gol3$Report.Date.9.years[1] <- "1996-09-05"
gol3$Report.Date.9.years <- ymd(gol3$Report.Date.9.years[1])
gol3$Report.Date.9.years[1] <- NA
gol3$X.PPR._Date[1] <- "1996-09-05"
gol3$X.PPR._Date <- ymd(gol3$X.PPR._Date)
gol3$X.PPR._Date[1] <- NA

# rbind gol data to merged oit and opcs data
merged2 <- rbind(merged, gol3)

# create csv filename for merged data
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
merged_filename <- str_c("master_data_", date2, ".csv")

write.csv(merged2, file = merged_filename, row.names = FALSE)

