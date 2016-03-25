library(stringr)
library(dplyr)

# script to merge new csv from impromptu query with oit unqueryable/truncated fields from opcs

## set working directory where files are saved
setwd("G:/PNP/Performance Measurement/master_data")

# find current version of option1 file with oit unqueryable/truncated fields
oit_filename <- list.files()[str_detect(list.files(), "oit_unqueryable")]

# read in oit data
oit <- read.csv(oit_filename, stringsAsFactors = FALSE, colClasses = c("CONTROL_NO" = "character"))

# select id variable and unqueryable/truncated text fields
# do not select milestone variables
oit_text <- select(oit, CONTROL_NO, Proj.Comp..Code, Fiscal.Year, Geographic.Need.Descr., Pressing.Need.Descr., Short.Descr., General.Descr.,
                   Scope.of.Work, GNS.Descr., Economic.Impact.or.Benefit)

# drop records with duplicate control # from oit_text
non_lead_applicant <- which(oit_text$Proj.Comp..Code != 1)
oit_text2 <- oit_text[-non_lead_applicant, ]

## should not be any duplicates after removing non-lead applicants, but just in case
dup <- (duplicated(oit_text2$CONTROL_NO))
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
oit_text3 <- oit_text2[non_dup_index, ]

# read in new csv from impromptu query
# script assumes new csv is saved in master_date folder; if not, setwd() working directory as needed

## input name of new csv file, for instance new_name <- "newfilename_2015.csv"
new_name <- "power_v3.csv"
new_csv <- read.csv(new_name, stringsAsFactors = FALSE, colClasses = c("Control." = "character"))

# left_join oit_text with new_csv by project #
merged <- left_join(new_csv, oit_text3, by = c("Control." = "CONTROL_NO"))

# create csv filename for merged data
new_name2 <- str_replace(new_name, ".csv", "")
merged_filename <- str_c(new_name2, "_merged", ".csv")
write.csv(merged, file = merged_filename, row.names = FALSE)



