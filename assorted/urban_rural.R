library(stringr)
library(readr)
library(dplyr)

options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(), 
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# get urban/rural split bullet
# restrict to pw and eaa because more of them are rural/urban
# unrestricted calculations are done below just as a check

shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                 Program %in% c("Public Works", "Economic Adjustment Assistance")) %>% group_by(Entity.Code) %>% tally()

shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                 Program %in% c("Public Works", "Economic Adjustment Assistance")) %>% group_by(Entity.Code) %>% tally() %>%
        mutate(pct = n / sum(n)) 

shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                 Program %in% c("Public Works", "Economic Adjustment Assistance")) %>% 
        mutate(entity_code_fct = case_when(.$Entity.Code %in% c("Urban", "Substate\r\r\nUrban") ~ "Urban",
                                           .$Entity.Code %in% c("Rural", "Rural Not\r\r\nMSA", "Rural MSA",
                                                                "Native American", "Indian") ~ "Rural",
                                           TRUE ~ "Other")) %>% group_by(entity_code_fct) %>% tally() %>% 
        mutate(pct = n / sum(n))

urban <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                          Program %in% c("Public Works", "Economic Adjustment Assistance"), 
                          Entity.Code %in% c("Urban", "Substate\r\r\nUrban")) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

rural <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                          Program %in% c("Public Works", "Economic Adjustment Assistance"), 
                          Entity.Code %in% c("Rural", "Rural Not\r\r\nMSA", "Rural MSA",
                                             "Native American", "Indian")) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

pw_eaa <- rbind(urban, rural, total)
pw_eaa$area <- c("urban", "rural", "total")
pw_eaa 

362890406 / 1051923285 # 34% urban share
646269803 / 1051923285 # 61% rural share


################################################################


# re-calculate same percentages without restricting to pw and eaa
# get urban/rural split bullet

shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>% group_by(Entity.Code) %>% tally()

total_unrestricted <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

urban_unrestricted  <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                          Entity.Code %in% c("Urban", "Substate\r\r\nUrban")) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

rural_unrestricted  <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                          Entity.Code %in% c("Rural", "Rural Not\r\r\nMSA", "Rural MSA",
                                             "Native American", "Indian")) %>% 
        summarize(count = n(), amount = dollar(sum(EDA.Funding, na.rm = TRUE)))

table_unrestricted <- rbind(urban_unrestricted, rural_unrestricted, total_unrestricted)
table_unrestricted$area <- c("urban_unrestricted", "rural_unrestricted", "total_unrestricted")
table_unrestricted


440983011 / 1357255827 # 32% urban share of funding
800171942 / 1357255827 # 59% rural share of funding


################################################


# create tables showing breakdown of entity codes

ec1_table <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                              Program %in% c("Public Works", "Economic Adjustment Assistance")) %>% 
        mutate(Entity.Code = case_when(is.na(.$Entity.Code) ~ "NA", .$Entity.Code == "Rural Not\r\r\nMSA" ~ "Rural Not MSA",
                                       .$Entity.Code == "Substate\r\r\nUrban" ~ "Substate Urban",
                                       TRUE ~ .$Entity.Code)) %>% arrange(Entity.Code) %>%
        group_by(Entity.Code) %>% tally() %>%
        mutate(pct = n / sum(n))

ec1_totals <- ec1_table %>% summarize(total_count = sum(n, na.rm = TRUE), total_pct = sum(pct, na.rm = TRUE))
ec1_totals <- cbind("Totals", ec1_totals)
names(ec1_totals) <- names(ec1_table)
ec1_table <- rbind(ec1_table, ec1_totals)

ec1_table$pct <- percent(ec1_table$pct)
names(ec1_table) <- c("Entity.Code", "Count", "Percentage")

# create main table
ec1_table_output <- tableGrob(ec1_table, rows = NULL)
title <- textGrob(paste("Entity Codes for FY 2012-2016", "\n", "PW & EAA Awards"), gp = gpar(fontsize = 15, fontface = "bold"))
padding <- unit(0.5, "line")
ec1_table_output <- gtable_add_rows(ec1_table_output, heights = grobHeight(title) + padding, pos = 0)
ec1_table_output <- gtable_add_grob(ec1_table_output, list(title), t = 1, l = 1, r = ncol(ec1_table_output))

# create totals row table
totals_table_output <- tableGrob(ec1_table[nrow(ec1_table), ], rows = NULL, 
                                 cols = as.character(ec1_table[nrow(ec1_table), 1:ncol(ec1_table)]))

# combine main table with totals row
ec1_table_output <- rbind(ec1_table_output[-nrow(ec1_table_output), ], totals_table_output[-nrow(totals_table_output), ])
# grid.newpage()
# grid.draw(ec1_table_output)

# save to file
grid.newpage()
pdf("ec1_table.pdf") 
grid.draw(ec1_table_output)
dev.off()



#######################################

ec2_table <- shiny %>% filter(Status == "Approved", FY > 2011, FY < 2017, 
                              Program %in% c("Public Works", "Economic Adjustment Assistance")) %>% 
        mutate(entity_code_fct = case_when(.$Entity.Code %in% c("Urban", "Substate\r\r\nUrban") ~ "Urban",
                                           .$Entity.Code %in% c("Rural", "Rural Not\r\r\nMSA", "Rural MSA",
                                                                "Native American", "Indian") ~ "Rural",
                                           TRUE ~ "Other")) %>% group_by(entity_code_fct) %>% tally() %>% 
        mutate(pct = n / sum(n))

ec2_totals <- ec2_table %>% summarize(total_count = sum(n, na.rm = TRUE), total_pct = sum(pct, na.rm = TRUE))
ec2_totals <- cbind("Totals", ec2_totals)
names(ec2_totals) <- names(ec2_table)
ec2_table <- rbind(ec2_table, ec2_totals)

ec2_table$pct <- percent(ec2_table$pct)
names(ec2_table) <- c("Entity.Code", "Count", "Percentage")

# create main table
ec2_table_output <- tableGrob(ec2_table, rows = NULL)
title <- textGrob(paste("Entity Codes", "\n", "for FY 2012-2016", "\n", "PW & EAA Awards"), gp = gpar(fontsize = 15, fontface = "bold"))
padding <- unit(0.5, "line")
ec2_table_output <- gtable_add_rows(ec2_table_output, heights = grobHeight(title) + padding, pos = 0)
ec2_table_output <- gtable_add_grob(ec2_table_output, list(title), t = 1, l = 1, r = ncol(ec2_table_output))

# create totals row table
totals_table_output <- tableGrob(ec2_table[nrow(ec2_table), ], rows = NULL, 
                                 cols = as.character(ec2_table[nrow(ec2_table), 1:ncol(ec2_table)]))

# combine main table with totals row
ec2_table_output <- rbind(ec2_table_output[-nrow(ec2_table_output), ], totals_table_output[-nrow(totals_table_output), ])
# grid.newpage()
# grid.draw(ec2_table_output)

# save to file
grid.newpage()
pdf("ec2_table.pdf") 
grid.draw(ec2_table_output)
dev.off()
