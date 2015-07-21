## load libraries
library(RDCOMClient)

## set working directory for indirect cost log

## for testing
setwd("C:/Users/sdevine/Desktop/icr_test")

## for sending final emails
setwd("G:/PNP/Indirect Costs")

## for testing
log <- read.csv("indirect_costs_test_full.csv", stringsAsFactors = FALSE)

## for sending final emails
log <- read.csv("indirect_costs_log.csv", stringsAsFactors = FALSE)

## get records with acceptance letters that need to be emailed
log1 <- subset(log, log$acceptance_letter == 0 | log$acceptance_letter == 1)

## loop through mailing acceptance letters
for(i in 1:length(log1$grantee_email)){
        print(log1$grantee[i])
        
        ## create email variables
        to <- log1$grantee_email[i]
        cc <- log1$eda_email[i]
        subject <- paste("Indirect Cost", log1$type[i], "Acceptance Letter", sep = " ")
        grantee <- log1$grantee[i]
        fy <- paste("FY", log1$fy_requested[i])
        body1 <- "Hello,\n\nPlease find attached the signed %s %s for %s.  If you have any questions, please don't hesitate to let me know.\n\nStephen Devine\nProgram Analyst\nPerformance and National Programs Division\nEconomic Development Administration\n202-482-9076"
        body <- sprintf(body1, fy, subject, grantee)
        attachment <- log1$attachment[i]
        
        ## init com api
        OutApp <- COMCreate("Outlook.Application")
        
        ## create an email 
        outMail <- OutApp$CreateItem(0)
        
        ## configure  email parameter 
        outMail[["To"]] <- to
        outMail[["cc"]] <- cc
        outMail[["subject"]] <- subject
        outMail[["body"]] <- body
                
        ## add attachment
        outMail[["Attachments"]]$Add(attachment)
        
        ## send email                     
        outMail$Send()
        print(paste("Email sent to", grantee))
}


