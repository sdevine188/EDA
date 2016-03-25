library(stringr)
library(RDCOMClient)
library(dplyr)

# set working directory
setwd("G:/PNP/Performance Measurement/email_regional_staff_script")

# read in eda email addresses
email_file <- list.files()[str_detect(list.files(), "email_addresses")]
raw_emails <- read.csv(email_file, stringsAsFactors = FALSE, header = FALSE)
names(raw_emails)[1] <- "raw"

# split email addresses
raw_emails$split <- str_split(raw_emails$raw, ">;")

# create a dataframe with clean email addresses and first/last names
string <- unlist(raw_emails$split)
string <- as.character(string)
emails <- data.frame(string, stringsAsFactors = FALSE)

# clean emails
emails$address <- sapply(str_split(emails$string, "<"), "[", 2)
emails$address <- str_replace(emails$address, ">", "")
emails$address <- str_trim(emails$address, side = "both")

# clean names
emails$name <- sapply(str_split(emails$string, "<"), "[", 1)
emails$first_name <- sapply(str_split(emails$name, ","), "[", 2)
emails$first_name <- str_trim(emails$first_name, side = "both")
emails$last_name <- sapply(str_split(emails$name, ","), "[", 1)
emails$last_name <- str_trim(emails$last_name, side = "both")

# remove distribution lists, which have NA listed for first_name
emails <- filter(emails, !is.na(first_name))

# load csv from the data folder
file_name <- "data/workforce_training_sic_omissions_20150928.csv"
df <- read.csv(file_name, stringsAsFactors = FALSE, colClasses = c("Project.No." = "character"))

## should not be any duplicates after removing non-lead applicants, but just in case
dup <- duplicated(df$Project.No.)
dup_index <- which(dup == TRUE)
non_dup_index <- which(dup == FALSE)
df <- df[non_dup_index, ]

# merge email address into file
df$edr_address <- "not found"
for(i in 1:nrow(emails)) {
        match_rows <- grep(emails$last_name[i], df$EDR.Name, ignore.case = TRUE)
        if(length(match_rows) > 0 && tolower(str_sub(emails$first_name[i], 1, 1)) == tolower(str_sub(df$EDR.Name[match_rows], 1, 1))) {
                df$edr_address[match_rows] <- emails$address[i]
        }            
}           

df$official_address <- "not found"
for(i in 1:nrow(emails)) {
        match_rows <- grep(emails$last_name[i], df$EDA.Official.Name, ignore.case = TRUE)
        if(length(match_rows) > 0 && tolower(str_sub(emails$first_name[i], 1, 1)) == tolower(str_sub(df$EDA.Official.Name[match_rows], 1, 1))) {
                df$official_address[match_rows] <- emails$address[i]
        }            
}

# remove rows where email address was not found
both_not_found <- which(df$edr_address == "not found" & df$official_address == "not found")
df1 <- df[-both_not_found, ]
df_not_found <- df[both_not_found, ]

# create variable for email_recipients
for(i in 1:nrow(df1)){
        if(df1$edr_address[i] == "not found" && df1$official_address[i] != "not found"){
                df1$email_recipients[i] <- df1$official_address[i]        
        }
        if(df1$edr_address[i] != "not found" && df1$official_address[i] == "not found"){
                df1$email_recipients[i] <- df1$edr_address[i]        
        }
        if(df1$edr_address[i] != "not found" && df1$official_address[i] != "not found"){
                df1$email_recipients[i] <- str_c(df1$edr_address[i], df1$official_address[i], sep = ";")
        }
}

## loop through mailing acceptance letters
# need to customize contents of email
for(i in 1:nrow(df1)){
        
        ## create custom email variables
#         to <- str_c("sdevine@eda.gov;indirectcosts@eda.gov")
#         to <- df1$email_recipients[i] 
#         cc <- str_c("mlofthus@eda.gov", "sdevine@eda.gov", sep = ";")
#         cc <- str_c("sdevine@eda.gov;indirectcosts@eda.gov")
        subject <- str_c("Workforce Training Initiative Code - Project #", df1$Project.No.[i])
        fy <- str_c("FY", df1$FY[i], sep = " ")
        body1 <- str_c("Hello,\n\nTo help close out %s, we've been asked to confirm with Regional staff that all the projects which merit the Workforce",
                        " Training (WT) initiative code are flagged as such in OPCS.  Based on a query of keywords and industry codes in OPCS, Project #%s was identified", 
                       " as one that potentially merits the WT initiative code.  Can you please confirm by COB Friday, October 9th", 
                       " whether or not you think the WT iniative code should be flagged for this project?\n\nIf you have any questions, please don't hesitate", 
                       " to let us know.  Thanks very much.\n\nStephen Devine\nProgram Analyst\nPerformance and National Programs Division\nEconomic",
                        " Development Administration\n202-482-9076")
        body <- sprintf(body1, fy, df1$Project.No.[i])
        
        ## init com api
        OutApp <- COMCreate("Outlook.Application")
        
        ## create an email 
        outMail <- OutApp$CreateItem(0)
        
        ## configure  email parameter 
        outMail[["To"]] <- to
        outMail[["cc"]] <- cc
        outMail[["subject"]] <- subject
        outMail[["body"]] <- body
        
        ## add attachment(s) - to add multiple, just keep adding code with new file names
        # bc of email client limitations, attachments must be saved to local folder like desktop or C:, cannot be saved on share drives
#         outMail[["Attachments"]]$Add("C:/users/sdevine/desktop/icr_test/Test123.docx")
#         outMail[["Attachments"]]$Add("C:/users/sdevine/desktop/icr_test/Test235.docx")
        
        ## send email                     
        outMail$Send()
        print(str_c("Email sent to ", to))
}

# print # of projects with no email found
not_found <- length(both_not_found)
if(length(not_found) == 0L){
        not_found <- "0"
}
print(str_c("There were ", not_found, " projects with no email addresses found."))
