library(qdapTools)
library(stringr)
library(dplyr)
library(textreadr)
library(readr)

# setwd
setwd("G:/OEA/LegAffairs/Grant Announcements/Released Grants")

# create function to get all info on awards for given year/state
extract_award_info <- function(year_input = "all") {
        # create output placeholders
        year_output <- c()
        state_output <- c()
        title_output <- c()
        award_num_output <- c()
        program_output <- c()
        appl_output <- c()
        impact_output <- c()
        description_output <- c()
        unread_.doc_format_output <- c()
        unread_bad_format_output <- c()
        
        # identify year folders
        years <- list.files()
        years <- years[-which(years == "GNS Grants Pre 2012")]
        print(str_c("available years = ", str_c(years, collapse = ", ")))
        print(str_c("year_input = ", year_input))
        
        # loop through year folders
        for(y in 1:length(years)) {
                if(years[y] %in% year_input | year_input == "all") {
                
                        # identify state folders
                        print(years[y])
                        year_folder_path <- str_c(getwd(), years[y], sep = "/")
                        states <- list.files(year_folder_path)
                        
                        # loop through state folders
                        for(s in 1:length(states)) {
                                print(states[s])
                                
                                # skip to next if state folder contains numbers, sometimes there are random folders in there
                                if(str_detect(states[s], "[0-9]")) {
                                        print("Error: 'state' folder name contains numbers, skipping to next state folder")
                                        next
                                }

                                state_folder_path <- str_c(getwd(), years[y], states[s], sep = "/")
                                # print(state_folder_path)
                                doc_list <- list.files(state_folder_path)
                                # print(doc_list)
                                
                                # loop through docs
                                if(length(doc_list) > 0) {
                                        for(d in 1:length(doc_list)) {
                                                
                                                if(str_sub(doc_list[d], 1, 1) != "~" && doc_list[d] != "Thumbs.db") {
                                                        
                                                        # print(doc_list[d])
                                                
                                                        # identify document type
                                                        dot_index <- str_locate_all(doc_list[d], "\\.")
                                                        dot_index <- dot_index[[1]][nrow(dot_index[[1]])]
                                                        doc_format <- str_sub(doc_list[d], (dot_index + 1), nchar(doc_list[d]))
                                                        # print(doc_format)
                                                        
                                                        # skip to next if it's a folder instead of a document
                                                        if(!(str_detect(doc_list[d], ".doc"))) {
                                                                next
                                                        }
                                                        
                                                        # read in .docx document format
                                                        if(doc_format == "docx") {
                                                                doc_file_path <- str_c(getwd(), years[y], states[s], doc_list[d], sep = "/")
                                                                # print(doc_file_path)
                                                                doc <- read_docx(doc_file_path)
                                                        }
                                                        
                                                        
################################################################################################################################################                                                        
                                                  ### beginning ###
                                                        
                                                        
                                                        # check for old .doc format, flag it, extract info, then skip to next
                                                        if(doc_format == "doc") {
                                                                doc_file_path <- str_c(getwd(), years[y], states[s], doc_list[d], sep = "/")
                                                                # print(str_c("unread .doc format: ", doc_file_path, "; reading in using read_doc"))
                                                                # print(str_c("unread .doc format: ", doc_file_path, "; skipping to next"))
                                                                unread_.doc_format_output <- append(unread_.doc_format_output, doc_file_path)
                                                                # print(unread_.doc_format_output)
                                                                # next
                                                                
                                                                # extract info from .doc format
                                                                        
                                                                # read in doc
                                                                doc <- read_doc(doc_file_path)
                                                                
                                                                # extract year
                                                                year_output <- append(year_output, years[y])
                                                                # print(years[y])
                                                                
                                                                # extract state
                                                                state_output <- append(state_output, states[s])
                                                                # print(state_output)
                                                                
                                                                # extract title
                                                                title_index <- which(str_detect(doc, "Title:") == TRUE)[1]
                                                                title <- doc[title_index]
                                                                title_start <- str_locate_all(title, "\\|")[[1]][2] + 1
                                                                title_end_df <- data.frame(str_locate_all(str_sub(title, title_start, nchar(title)), "[A-z0-9]")[[1]])
                                                                title_end <- title_end_df[nrow(title_end_df), 1] + (title_start - 1)
                                                                title <- str_sub(title, title_start, title_end)
                                                                
                                                                # remove latin1 encoding errors
                                                                Encoding(title) <- "latin1"
                                                                title <- gsub("Ã¢Â???Â", "\"", title)
                                                                title <- gsub("Ã¢Â???Âo", "\"", title)
                                                                title <- gsub("â???T", "'", title)
                                                                title <- gsub("â???o", "'", title)
                                                                title <- gsub("â???"", "-", title)
                                                                title <- gsub("â???"", "-", title)
                                                                title <- gsub("â???'", "-", title)
                                                                title <- gsub("â???", "-", title)
                                                                title <- gsub("Â", " ", title)
                                                                title <- gsub("Ì¶", "-", title)
                                                                
                                                                title_output <- append(title_output, title)
                                                                # print(title_output)
                                                                
                                                                # extract award_num
                                                                award_num_index <- which(str_detect(doc, "Investment No|Investment Num|Investment Amount") == TRUE)
                                                                award_num <- doc[award_num_index[1]]
                                                                # sometimes the actual numeric award_num is in the same element as the "Investment No" text, but sometimes not
                                                                while(!str_detect(award_num, "[0-9]")) {
                                                                        award_num_index <- award_num_index[1] + 1
                                                                        award_num <- doc[(award_num_index)]
                                                                }
                                                                # flag error if project number can't be found
                                                                if(!str_detect(award_num, "[0-9]")) {
                                                                        print("Error: Project Number not found for ", doc_file_path)
                                                                        unread_bad_format_output <- append(unread_bad_format_output, doc_file_path)
                                                                        # print(unread_bad_format_output)
                                                                        # remove year, state, and title output already appended
                                                                        year_output <- year_output[-length(year_output)]
                                                                        state_output <- state_output[-length(state_output)]
                                                                        title_output <- title_output[-length(title_output)]
                                                                        next
                                                                }
                                                                
                                                                award_num_start <- str_locate_all(award_num, "\\|")[[1]][2] + 1
                                                                award_num_end_df <- data.frame(str_locate_all(str_sub(award_num, award_num_start, nchar(award_num)), "[0-9]")[[1]])
                                                                award_num_end <- award_num_end_df[nrow(award_num_end_df), 1] + (award_num_start - 1)
                                                                award_num <- str_sub(award_num, award_num_start, award_num_end)
                                                                award_num <- str_trim(award_num)
                                                                award_num <- str_replace_all(award_num, "-|,|\\.", "")
                                                                award_num <- str_replace_all(award_num, " ", "")
                                                                
                                                                if(str_sub(award_num, 1, 5) == "GRANT") {
                                                                        print(str_c("Error: Grants.gov format for ", doc_file_path))
                                                                        unread_bad_format_output <- append(unread_bad_format_output, doc_file_path)
                                                                        # remove year, state, and title output already appended
                                                                        year_output <- year_output[-length(year_output)]
                                                                        state_output <- state_output[-length(state_output)]
                                                                        title_output <- title_output[-length(title_output)]
                                                                        next
                                                                }
                                                                
                                                                # flag duplicates
                                                                if(award_num %in% award_num_output) {
                                                                        print(str_c("Error: award_num ", award_num, " is already in award_num_output"))
                                                                }
                                                                award_num_output <- append(award_num_output, award_num)
                                                                # print(award_num_output)
                                                                
                                                                # extract program
                                                                program_index <- which(str_detect(doc, "Investment No|Investment Num|Investment Amount") == TRUE)[1]
                                                                program <- doc[program_index]
                                                                program_section_start <- str_locate_all(program, "\\|")[[1]][2] + 1
                                                                program_start <- str_locate(str_sub(program, program_section_start, nchar(program)), "[A-z]{4,}")[1] + (program_section_start - 1)
                                                                program_end_df <- data.frame(str_locate_all(str_sub(program, program_start, nchar(program)), "[A-z0-9]")[[1]])
                                                                program_end <- program_end_df[nrow(program_end_df), 1] + (program_start - 1)
                                                                program <- str_sub(program, program_start, program_end)
                                                                
                                                                # remove latin1 encoding errors
                                                                Encoding(program) <- "latin1"
                                                                program <- gsub("Ã¢Â???Â", "\"", program)
                                                                program <- gsub("Ã¢Â???Âo", "\"", program)
                                                                program <- gsub("â???T", "'", program)
                                                                program <- gsub("â???o", "'", program)
                                                                program <- gsub("â???"", "-", program)
                                                                program <- gsub("â???"", "-", program)
                                                                program <- gsub("â???'", "-", program)
                                                                program <- gsub("â???", "-", program)
                                                                program <- gsub("Â", " ", program)
                                                                program <- gsub("Ì¶", "-", program)
                                                                
                                                                program_output <- append(program_output, program)
                                                                # print(program_output)
                                                                
                                                                # extract appl
                                                                appl_index <- which(str_detect(doc, "Applicant") == TRUE)
                                                                appl <- doc[appl_index]
                                                                appl_start <- str_locate_all(appl, "\\|")[[1]][2] + 1
                                                                appl_end_df <- data.frame(str_locate_all(str_sub(appl, appl_start, nchar(appl)), "[A-z0-9]")[[1]])
                                                                appl_end <- appl_end_df[nrow(appl_end_df), 1] + (appl_start - 1)
                                                                appl <- str_sub(appl, appl_start, appl_end)
                                                                
                                                                # remove latin1 encoding errors
                                                                Encoding(appl) <- "latin1"
                                                                appl <- gsub("Ã¢Â???Â", "\"", appl)
                                                                appl <- gsub("Ã¢Â???Âo", "\"", appl)
                                                                appl <- gsub("â???T", "'", appl)
                                                                appl <- gsub("â???o", "'", appl)
                                                                appl <- gsub("â???"", "-", appl)
                                                                appl <- gsub("â???"", "-", appl)
                                                                appl <- gsub("â???'", "-", appl)
                                                                appl <- gsub("â???", "-", appl)
                                                                appl <- gsub("Â", " ", appl)
                                                                appl <- gsub("Ì¶", "-", appl)
                                                                
                                                                appl_output <- append(appl_output, appl)
                                                                # print(appl_output)
                                                                
                                                                # extract impact
                                                                impact_keyword_index <- which(str_detect(doc, "Impact"))
                                                                outcome_keyword_index <- which(str_detect(doc, "[O|o]utcome"))
                                                                combined_keyword_index <- c(impact_keyword_index, outcome_keyword_index)
                                                                combined_keyword_index <- combined_keyword_index[duplicated(combined_keyword_index)][1]
                                                                impact_index <- combined_keyword_index
                                                                
                                                                # impact_index <- which(str_detect(doc, "Anticipated Impact") == TRUE)
                                                                if(length(impact_index) == 0) {
                                                                        print(str_c("Error: 'Anticiapted Impact' text not found, so no impact_index identified"))
                                                                }
                                                                impact <- doc[impact_index]
                                                                
                                                                # sometimes with .doc files the impact states breaks up, so "Anticipated" and part of text is in one element
                                                                # and "Impact/Outcome" and part of text is in another element
                                                                anticipated_keyword_index <- which(str_detect(doc, "Anticipated"))
                                                                if(anticipated_keyword_index != impact_index) {
                                                                        impact_index <- impact_index - 1
                                                                        impact <- doc[impact_index]
                                                                        
                                                                }
                                                                
                                                                impact_start <- str_locate_all(impact, "\\|")[[1]][2] + 1
                                                                # sometimes the impact description spills over into multiple elements, 
                                                                # so start with Description element and work backward looking for prior element containing text 
                                                                # sometimes there is blank line after Impact, so the elements immediately before Description can be blanks
                                                                impact_end_index <- which(str_detect(doc, "[D|d]escription|[B|b]ackground|This EDA investment") == TRUE)[1] - 1
                                                                if(length(impact_end_index) > 1) {
                                                                        print(str_c("Error: impact_end_index has length > 1 for ", doc_file_path))
                                                                }
                                                                while(!str_detect(doc[impact_end_index], "[A-z0-9]")) {
                                                                        impact_end_index <- impact_end_index - 1
                                                                }
                                                                impact <- str_c(doc[impact_index:impact_end_index], collapse = " ")
                                                                impact <- str_replace_all(impact, "\\|", "")
                                                                impact <- str_replace_all(impact, "\\s{2,}", " ")
                                                                if(anticipated_keyword_index != combined_keyword_index && str_detect(str_sub(impact, 1, 11), "[A|a]nticipated")) {
                                                                        impact <- str_sub(impact, 12, nchar(impact))
                                                                        impact <- str_replace(impact, "Impact/Outcome", "")
                                                                }
                                                                impact <- str_trim(impact, side = "both")
                                                                
                                                                # remove latin1 encoding errors
                                                                Encoding(impact) <- "latin1"
                                                                impact <- gsub("Ã¢Â???Â", "\"", impact)
                                                                impact <- gsub("Ã¢Â???Âo", "\"", impact)
                                                                impact <- gsub("â???T", "'", impact)
                                                                impact <- gsub("â???o", "'", impact)
                                                                impact <- gsub("â???"", "-", impact)
                                                                impact <- gsub("â???"", "-", impact)
                                                                impact <- gsub("â???'", "-", impact)
                                                                impact <- gsub("â???", "-", impact)
                                                                impact <- gsub("Â", " ", impact)
                                                                impact <- gsub("Ì¶", "-", impact)
                                                                
                                                                impact_output <- append(impact_output, impact)
                                                                # print(impact_output)
                                                                
                                                                # extract description
                                                                description_index <- which(str_detect(doc, "[D|d]escription|[B|b]ackground|This EDA investment") == TRUE)
                                                                description <- doc[description_index[1]]
                                                                description_end_index <- length(doc)
                                                                if(sum(str_detect(doc, "482-2900")) > 0) {
                                                                        description_end_index <- which(str_detect(doc, "482-2900") == "TRUE") - 1
                                                                }
                                                                if(length(description_end_index) > 1) {
                                                                        description_end_index <- description_end_index[2]
                                                                }
                                                                # tryCatch(  
                                                                #         description <- str_c(doc[description_index[1]:description_end_index], collapse = " ")
                                                                #         warning = function(w) {
                                                                #                 print(w)
                                                                #                 print("Warning shown above for ", doc_file_path)
                                                                #         }
                                                                # )
                                                                # print(str_c("description_index is: ", description_index[1]))
                                                                # print(str_c("description_end_index is: ", description_end_index))
                                                                description <- str_c(doc[description_index[1]:description_end_index], collapse = " ")
                                                                description <- str_replace_all(description, "\\|", "")
                                                                description <- str_replace_all(description, "\\s{2,}", " ")
                                                                description <- str_trim(description, side = "both")
                                                                description <- str_replace(description, "Approvals:", "")
                                                                if(str_detect(str_sub(description, 1, 19), "[P|p]roject [D|d]escription")) {
                                                                        description <- str_sub(description, 20, nchar(description))
                                                                }
                                                                if(str_detect(str_sub(description, 1, 7), "[P|p]roject")) {
                                                                        description <- str_sub(description, 8, nchar(description))
                                                                }
                                                                if(str_detect(str_sub(description, 1, 1), "\\.")) {
                                                                        description <- str_sub(description, 2, nchar(description))
                                                                }
                                                                description <- str_trim(description, side = "both")
                                                                # print(description)
                                                                
                                                                # remove latin1 encoding errors
                                                                Encoding(description) <- "latin1"
                                                                description <- gsub("Ã¢Â???Â", "\"", description)
                                                                description <- gsub("Ã¢Â???Âo", "\"", description)
                                                                description <- gsub("â???T", "'", description)
                                                                description <- gsub("â???o", "'", description)
                                                                description <- gsub("â???"", "-", description)
                                                                description <- gsub("â???"", "-", description)
                                                                description <- gsub("â???'", "-", description)
                                                                description <- gsub("â???", "-", description)
                                                                description <- gsub("Â", " ", description)
                                                                description <- gsub("Ì¶", "-", description)
                                                                
                                                                description_output <- append(description_output, description)
                                                                # print(description_output)
                                                                
                                                                next
                                                        }
                                                        
                                                        
                                                        ### end ###
############################################################################################################################################################                                                        
                                                        
                                                        
                                                        # extract year
                                                        year_output <- append(year_output, years[y])
                                                        
                                                        # extract state
                                                        state_output <- append(state_output, states[s])
                                                        
                                                        # extract title
                                                        title_index <- which(str_detect(doc, "Title:") == TRUE)[1]
                                                        title <- doc[title_index + 1]
                                                        
                                                        # remove latin1 encoding errors
                                                        Encoding(title) <- "latin1"
                                                        title <- gsub("Ã¢Â???Â", "\"", title)
                                                        title <- gsub("Ã¢Â???Âo", "\"", title)
                                                        title <- gsub("â???T", "'", title)
                                                        title <- gsub("â???o", "'", title)
                                                        title <- gsub("â???"", "-", title)
                                                        title <- gsub("â???"", "-", title)
                                                        title <- gsub("â???'", "-", title)
                                                        title <- gsub("â???", "-", title)
                                                        title <- gsub("Â", " ", title)
                                                        title <- gsub("Ì¶", "-", title)
                                                        

                                                        title_output <- append(title_output, title)
                                                        # print(title)
                                                        
                                                        # extract award number
                                                        award_num_index <- which(str_detect(doc, "Investment No|Investment Num|Investment Amount") == TRUE)
                                                        award_num <- doc[award_num_index[1]]
                                                        # sometimes the actual numeric award_num is in the same element as the "Investment No" text, but sometimes not
                                                        while(!str_detect(award_num, "[0-9]")) {
                                                                award_num_index <- award_num_index[1] + 1
                                                                award_num <- doc[(award_num_index)]
                                                        }
                                                        # flag error if project number can't be found
                                                        if(!str_detect(award_num, "[0-9]")) {
                                                                print("Error: Project Number not found for ", doc_file_path)
                                                                unread_bad_format_output <- append(unread_bad_format_output, doc_file_path)
                                                                # print(unread_bad_format_output)
                                                                # remove year, state, and title output already appended
                                                                year_output <- year_output[-length(year_output)]
                                                                state_output <- state_output[-length(state_output)]
                                                                title_output <- title_output[-length(title_output)]
                                                                next
                                                        }
                                                
                                                        award_num_start <- str_locate_all(award_num, "[0-9]")
                                                        award_num_start <- data.frame(award_num_start[[1]])
                                                        award_num_start <- award_num_start[1, 1]
                                                        # sometimes the award_num is "GRANT12345678" which is the Grant.gov #, not a GOL/OPCS award_num, so need to skip these
                                                        if(str_sub(award_num, (award_num_start - 5), (award_num_start - 1)) == "GRANT") {
                                                                print(str_c("Error: Grants.gov format for ", doc_file_path))
                                                                unread_bad_format_output <- append(unread_bad_format_output, doc_file_path)
                                                                # remove year, state, and title output already appended
                                                                year_output <- year_output[-length(year_output)]
                                                                state_output <- state_output[-length(state_output)]
                                                                title_output <- title_output[-length(title_output)]
                                                                next
                                                        }
                                                        if(str_sub(award_num, (award_num_start - 2), (award_num_start - 1)) == "ED") {
                                                                award_num_start <- award_num_start - 2
                                                        }
                                                        # sometimes the GOL award number begins with "EDA..." instead of "ED..."
                                                        if(str_sub(award_num, (award_num_start - 3), (award_num_start - 1)) == "EDA") {
                                                                award_num_start <- award_num_start - 3
                                                        }
                                        
                                                        award_num_end_list <- str_locate_all(award_num, "[0-9]")
                                                        award_num_end_df <- data.frame(award_num_end_list[[1]])
                                                        award_num_end_digit <- nrow(award_num_end_df)
                                                        award_num_end_value <- award_num_end_df[award_num_end_digit, 1]
                                                        
                                                        # sometimes there is a numeric character in the "Type" text (e.g. SC2 award)
                                                        # need to artificially set award_num_start for GOL awards to bypass the "EDxxHDQ" type of text 
                                                        if(str_sub(award_num, award_num_start, (award_num_start + 1)) == "ED") {
                                                                while(str_detect(str_sub(award_num, (award_num_start + 8), award_num_end_value), "[A-z]| ")) {
                                                                        award_num_end_digit <- award_num_end_digit - 1
                                                                        award_num_end_value <- award_num_end_df[award_num_end_digit, 1]
                                                                }
                                                        }
                                                        if(str_sub(award_num, award_num_start, (award_num_start + 1)) != "ED") {
                                                                while(str_detect(str_sub(award_num, award_num_start, award_num_end_value), "[A-z]")) {
                                                                        award_num_end_digit <- award_num_end_digit - 1
                                                                        award_num_end_value <- award_num_end_df[award_num_end_digit, 1]
                                                                }
                                                        }
                                                        
                                                        award_num <- str_sub(award_num, award_num_start, award_num_end_value)
                                                        award_num <- str_trim(award_num)
                                                        award_num <- str_replace_all(award_num, "-|,|\\.", "")
                                                        award_num <- str_replace_all(award_num, " ", "")
                                                        # flag duplicates
                                                        if(award_num %in% award_num_output) {
                                                                print(str_c("Error: award_num ", award_num, " is already in award_num_output"))
                                                        }
                                                        
                                                        award_num_output <- append(award_num_output, award_num)
                                                        if(!(length(award_num) > 0)) {
                                                                print(str_c("Error: ", doc_file_path))
                                                        }
                                                        print(award_num)
                                                        
                                                        # extract program
                                                        program_index <- which(str_detect(doc, "Investment No|Investment Num|Investment Amount") == TRUE)[1]
                                                        program <- str_trim(doc[program_index + 1])
                                                        program_start <- str_locate(program, " [A-z]")[1, 1]
                                                        program <- str_sub(program, (program_start + 1), nchar(program))
                                                        program <- str_trim(program)
                                                        
                                                        # remove latin1 encoding errors
                                                        Encoding(program) <- "latin1"
                                                        program <- gsub("Ã¢Â???Â", "\"", program)
                                                        program <- gsub("Ã¢Â???Âo", "\"", program)
                                                        program <- gsub("â???T", "'", program)
                                                        program <- gsub("â???o", "'", program)
                                                        program <- gsub("â???"", "-", program)
                                                        program <- gsub("â???"", "-", program)
                                                        program <- gsub("â???'", "-", program)
                                                        program <- gsub("â???", "-", program)
                                                        program <- gsub("Â", " ", program)
                                                        program <- gsub("Ì¶", "-", program)
                                                        
                                                        program_output <- append(program_output, program)
                                                        # print(program)
                                                        
                                                        # extract appl
                                                        appl_index <- which(str_detect(doc, "Applicant") == TRUE)
                                                        appl <- str_trim(doc[appl_index[1] + 1])
                                                        
                                                        # remove latin1 encoding errors
                                                        Encoding(appl) <- "latin1"
                                                        appl <- gsub("Ã¢Â???Â", "\"", appl)
                                                        appl <- gsub("Ã¢Â???Âo", "\"", appl)
                                                        appl <- gsub("â???T", "'", appl)
                                                        appl <- gsub("â???o", "'", appl)
                                                        appl <- gsub("â???"", "-", appl)
                                                        appl <- gsub("â???"", "-", appl)
                                                        appl <- gsub("â???'", "-", appl)
                                                        appl <- gsub("â???", "-", appl)
                                                        appl <- gsub("Â", " ", appl)
                                                        appl <- gsub("Ì¶", "-", appl)
                                                        
                                                        appl_output <- append(appl_output, appl)
                                                        # print(appl)

                                                        # extract impact
                                                        impact_keyword_index <- which(str_detect(doc, "Impact"))
                                                        outcome_keyword_index <- which(str_detect(doc, "[O|o]utcome"))
                                                        
                                                        # sometimes there is not Impact or Outcome section, if so leave as NA and avoid cleaning below
                                                        if(length(impact_keyword_index) == 0 && length(outcome_keyword_index) == 0) {
                                                                
                                                        }
                                                        combined_keyword_index <- c(impact_keyword_index, outcome_keyword_index)
                                                        impact_index <- combined_keyword_index[duplicated(combined_keyword_index)][1]
                                                        # impact_index <- which(str_detect(doc, "Impact") == TRUE)
                                                        # sometimes the word "Impact" is in title or applicant name, so extract the one not within first 10 elements of doc
                                                        impact_index <- impact_index[which(impact_index > 10)]
                                                        if(length(impact_index) > 0){
                                                                impact <- doc[impact_index[1] + 1]
                                                                if(!str_detect(impact, "[A-z]")) {
                                                                        impact <- doc[impact_index[1] + 2]
                                                                        print(str_c("Warning: Initial impact_index element contained no text, so selected next element: ", impact))
                                                                }
                                                        }
                                                        if(!(length(impact_index) > 0)) {
                                                                impact <- "NA"
                                                        }
                                                        
                                                        # remove latin1 encoding errors
                                                        Encoding(impact) <- "latin1"
                                                        impact <- gsub("Ã¢Â???Â", "\"", impact)
                                                        impact <- gsub("Ã¢Â???Âo", "\"", impact)
                                                        impact <- gsub("â???T", "'", impact)
                                                        impact <- gsub("â???o", "'", impact)
                                                        impact <- gsub("â???"", "-", impact)
                                                        impact <- gsub("â???"", "-", impact)
                                                        impact <- gsub("â???'", "-", impact)
                                                        impact <- gsub("â???", "-", impact)
                                                        impact <- gsub("Â", " ", impact)
                                                        impact <- gsub("Ì¶", "-", impact)
                                                        
                                                        impact_output <- append(impact_output, impact)
                                                        # if(length(appl) > 1) {
                                                        #         print(str_c("Error: impact length is: ", length(appl)))
                                                        # }
                                                        # print(length(impact_output) - length(year_output))
                                                        # print(impact)
                                                        
                                                        # extract description
                                                        description_index <- which(str_detect(doc, "[D|d]escription|[B|b]ackground|This EDA investment") == TRUE)
                                                        # print(str_c("current description_index: ", doc[description_index]))
                                                        # some documents read-in with "Description" header as its own item, others it's combined with text, so can't be dropped
                                                        if(nchar(str_trim(doc[description_index[1]])) > 25) {
                                                                description <- doc[description_index[1]:length(doc)]
                                                                description[1] <- str_replace(description[1], "Description: ", "")
                                                                
                                                                # print("description not isolated")
                                                                # description <- doc[description_index[1]:length(doc)]
                                                                # description[1] <- str_replace(description[1], "Description: ", "")
                                                        }
                                                        if(nchar(str_trim(doc[description_index[1]])) < 25) { 
                                                                description <- doc[(description_index[1] + 1):length(doc)]
                                                                
                                                                # print("description isolated")
                                                                # description <- doc[(description_index[1] + 1):length(doc)]
                                                                
                                                        }
                                                        # print(description)
                                                        # remove closing remarks which are sometimes at end of document
                                                        closing_remarks_index <- which(str_detect(description, "482-2900"))
                                                        if(length(closing_remarks_index) > 0) {
                                                                description <- description[1:(closing_remarks_index - 1)]
                                                        }
                                                        approved_by_index <- which(str_detect(description, "This investment was approved by "))
                                                        if(length(approved_by_index) > 0) {
                                                                description <- description[1:(approved_by_index - 1)]
                                                        }
                                                        description <- str_trim(str_c(description, collapse = " "))
                                                        if(str_detect(str_sub(description, 1, 19), "[P|p]roject [D|d]escription")) {
                                                                description <- str_sub(description, 20, nchar(description))
                                                        }
                                                        if(str_detect(str_sub(description, 1, 7), "[P|p]roject")) {
                                                                description <- str_sub(description, 8, nchar(description))
                                                        }
                                                        if(str_detect(str_sub(description, 1, 1), "\\.")) {
                                                                description <- str_sub(description, 2, nchar(description))
                                                        }
                                                        description <- str_trim(description, side = "both")
                                                        
                                                        # remove latin1 encoding errors
                                                        Encoding(description) <- "latin1"
                                                        description <- gsub("Ã¢Â???Â", "\"", description)
                                                        description <- gsub("Ã¢Â???Âo", "\"", description)
                                                        description <- gsub("â???T", "'", description)
                                                        description <- gsub("â???o", "'", description)
                                                        description <- gsub("â???"", "-", description)
                                                        description <- gsub("â???"", "-", description)
                                                        description <- gsub("â???'", "-", description)
                                                        description <- gsub("â???", "-", description)
                                                        description <- gsub("Â", " ", description)
                                                        description <- gsub("Ì¶", "-", description)
                                                        
                                                        description_output <- append(description_output, description)
                                                        # print(description)
                                                }
                                                
                                        }
                                }
                                
                        }
                }
        }
        
        print(str_c("year_output: ", length(year_output)))
        print(str_c("state_output: ", length(state_output)))
        print(str_c("title_output: ", length(title_output)))
        print(str_c("award_num_output: ", length(award_num_output)))
        print(str_c("program_output: ", length(program_output)))
        print(str_c("appl_output: ", length(appl_output)))
        print(str_c("impact_output: ", length(impact_output)))
        print(str_c("description_output: ", length(description_output)))
        
        # collapse unread_.doc_format
        print(str_c("unread_.doc_format_output: ", length(unread_.doc_format_output)))
        unread_.doc_format_output <- str_c(unread_.doc_format_output, collapse = " | ")
        unread_.doc_format_output <- rep(unread_.doc_format_output, length(year_output))
        if(length(unread_.doc_format_output) == 0) {
                unread_.doc_format_output <- rep(NA, length(year_output))
        }
        
        # collapse unread_bad_format
        print(str_c("unread_bad_format_output: ", length(unread_bad_format_output)))
        unread_bad_format_output <- str_c(unread_bad_format_output, collapse = " | ")
        unread_bad_format_output <- rep(unread_bad_format_output, length(year_output))
        if(length(unread_bad_format_output) == 0) {
                unread_bad_format_output <- rep(NA, length(year_output))
        }
        
        # combine output into dataframe and return output
        output <- data.frame(year = year_output, state = state_output, title = title_output, award_num = award_num_output, program = program_output, 
                             appl = appl_output, impact = impact_output, description = description_output, unread_.doc_format = unread_.doc_format_output)
        output
}

# setwd
setwd("G:/OEA/LegAffairs/Grant Announcements/Released Grants")
year_input_value <- 2012
award_info <- extract_award_info(year_input = year_input_value)
unread_.doc <- unlist(str_split(award_info$unread_.doc_format[1], " \\| "))
unread_bad <- unlist(str_split(award_info$unread_bad_format[1], " \\| "))
if(sum(str_detect(names(award_info), "unread_bad_format")) > 0) {
        award_info <- award_info %>% select(-c(unread_.doc_format, unread_bad_format))
} else {
        award_info <- award_info %>% select(-unread_.doc_format)
}

# head(award_info)
dim(award_info)
glimpse(award_info)

award_info %>% group_by(state) %>% tally() %>% data.frame(.)
award_info %>% group_by(year) %>% tally()
award_info %>% group_by(program) %>% tally() %>% data.frame(.)
award_info %>% group_by(award_num) %>% tally() %>% data.frame(.)
award_info %>% group_by(appl) %>% tally() %>% data.frame(.)

award_info %>% select(state, award_num)
award_info %>% select(state, title)
award_info %>% select(state, program)
award_info %>% select(state, impact)
award_info %>% select(state, description)
length(unread_.doc)
length(unread_bad)

setwd("G:/PNP/Performance Measurement/Data Calls/OEA Award Announcements")
file_name <- str_c("OEA_", year_input_value, ".csv")
write_csv(award_info, file_name)

###############################################


setwd("G:/PNP/Performance Measurement/Data Calls/OEA Award Announcements")
oea_2012 <- read_csv("OEA_2012.csv")
oea_2013 <- read_csv("OEA_2013.csv")
oea_2014 <- read_csv("OEA_2014.csv")
oea_2015 <- read_csv("OEA_2015.csv")
oea_2016 <- read_csv("OEA_2016.csv")
oea_2017 <- read_csv("OEA_2017.csv")
oea <- rbind(oea_2012, oea_2013, oea_2014, oea_2015, oea_2016, oea_2017)

write_csv(oea, "oea.csv")

##############################################
        
sum(duplicated(oea_2012$award_num))       
sum(duplicated(oea_2013$award_num))       
sum(duplicated(oea_2014$award_num))       
sum(duplicated(oea_2015$award_num))       
sum(duplicated(oea_2016$award_num))       
sum(duplicated(oea_2017$award_num))       
