library(RDCOMClient)

## init com api
OutApp <- COMCreate("Outlook.Application")

## create an email 
outMail <- OutApp$CreateItem(0)

## configure  email parameter 
outMail[["To"]] <- "sdevine188@gmail.com; sdevine@eda.gov; indirectcosts@eda.gov"
outMail[["cc"]] <- ""
outMail[["subject"]] <- "some subject 2"
outMail[["body"]] <- "some body 4"

## send it                     
outMail$Send()

## add attachment
outMail[["Attachments"]]$Add("C:/Users/sdevine/Desktop/mail_merge/test_document_lincoln.pdf")






library(sendmailR)

#set working directory
setwd("C:/workingdirectorypath")

#####send plain email

from <- "sdevine@eda.gov"
to <- "sdevine@eda.gov"
subject <- "Email Subject"
body <- "Email body."                     
mailControl=list(smtpServer="2080e003-7f43-4d47-9880-3368495b0615@eda.gov")

sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
sendmail(from=from,to=to,subject=subject,msg=body,control=list(smtpServer="2080e003-7f43-4d47-9880-3368495b0615@eda.gov", smtp))

#####send same email with attachment

#needs full path if not in working directory
attachmentPath <- "subfolder/log.txt"

#same as attachmentPath if using working directory
attachmentName <- "log.txt"

#key part for attachments, put the body and the mime_part in a list for msg
attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
bodyWithAttachment <- list(body,attachmentObject)

sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)

