library(stringr)
library(RDCOMClient)
library(dplyr)
library(readr)

# set working directory
setwd("G:/PNP/Performance Measurement/R Helper Scripts/email_regional_staff_script")

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

# write.csv(emails, "G:/PNP/Performance Measurement/Data Calls/emails.csv", row.names = FALSE)

# load csv from the data folder
file_name <- "data/missing.csv"
df <- read_csv(file_name)

## should not be any duplicates after removing non-lead applicants, but just in case
dup <- duplicated(df$Control.)
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
df1 <- df
if(length(both_not_found) > 0) {
        df1 <- df[-both_not_found, ]
}
df_not_found <- df[both_not_found, ]
print(str_c("original project count = ", dim(df)[1], " ; after removing projects with no email address, count is ", dim(df)[1] - dim(df_not_found)[1]))

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
        to <- str_c("kshadrina@eda.gov", "sdevine@eda.gov", sep = ";")
        # to <- df1$email_recipients[i] 
        cc <- str_c("kshadrina@eda.gov", "sdevine@eda.gov", sep = ";")
#         cc <- str_c("sdevine@eda.gov;indirectcosts@eda.gov")
        subject <- str_c("POWER Application - Control #", df1$Control.[i])
        fy <- str_c("FY", df1$FY[i], sep = " ")
        body1 <- str_c("Hello,\n\nTo help with EDA's POWER Initiative, we've been asked to confirm with Regional staff that all the applications which merit the POWER",
                        " (PO) initiative code are flagged as such in OPCS.  Based on a query in OPCS, Control #%s was identified", 
                       " as one that potentially merits the PO initiative code.  Can you please confirm by COB Friday, February 12th", 
                       " whether or not you think the PO iniative code should be flagged for this project, and if so, whether you were able to update OPCS accordingly?\n\nIf you have any questions, please don't hesitate", 
                       " to let us know.  Thanks very much.\n\nStephen Devine\nProgram Analyst\nPerformance and National Programs Division\nEconomic",
                        " Development Administration\n202-482-9076")
        body <- sprintf(body1, df1$Control.[i])
        
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
