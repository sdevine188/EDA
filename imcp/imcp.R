library(readr)
library(stringr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)

# setwd
# setwd("C:/Users/Stephen/Desktop/imcp")
setwd("G:/PNP/Performance Measurement/IMCP/Data/IMCP Funding Sources/csv/manually_cleaned_csv")

# read in data
list.files()
names <- c("name", "org", "org_type", "imcp", "member", "app_date", "preference", "funding", "matching", "award_date")

alamo <- read_csv("alamo.csv")
alamo$comm <- "alamo"
ca_valley <- read_csv("ca_central_valley_agplus.csv")
ca_valley$comm <- "ca_valley"
chicago <- read_csv("chicago.csv")
chicago$comm <- "chicago"
kansas <- read_csv("kansas.csv")
kansas$comm <- "kansas"
louisiana <- read_csv("louisiana.csv")
louisiana$comm <- "louisiana"
portland <- read_csv("me_portland.csv")
portland$comm <- "portland"
# memphis <- read_csv("memphis.csv")
# memphis$comm <- "memphis"
milwaukee <- read_csv("milwaukee.csv")
milwaukee$comm <- "milwaukee"
msp <- read_csv("minneapolis_st_paul.csv")
msp$comm <- "msp"
nw_ga <- read_csv("nw_ga.csv")
nw_ga$comm <- "nw_ga"
pac_nw <- read_csv("pacific_northwest.csv")
pac_nw$comm <- "pac_nw"
peoria <- read_csv("peoria.csv")
peoria$comm <- "peoria"
pittsburgh <- read_csv("pittsburgh.csv")
pittsburgh$comm <- "pittsburgh"
southern_ca <- read_csv("southern_ca.csv")
southern_ca$comm <- "southern_ca"
alabama <- read_csv("sw_alabama.csv")
alabama$comm <- "alabama"
tn <- read_csv("tn_valley.csv")
tn$comm <- "tn"
utah <- read_csv("utah.csv")
utah$comm <- "utah"
ohio <- read_csv("ohio.csv")
ohio$comm <- "ohio"
wa_puget <- read_csv("wa_puget.csv")
wa_puget$comm <- "wa_puget"
michigan <- read_csv("michigan.csv")
michigan$comm <- "michigan"

# combine data
data <- rbind(alamo, ca_valley, chicago, kansas, louisiana, portland, milwaukee, msp, nw_ga, pac_nw, 
              peoria, pittsburgh, southern_ca, alabama, tn, utah, michigan, ohio, wa_puget)

# clean data
# clean org_type
data$org_type <- str_replace(data$org_type, "Federal Government", "Federal")
data$org_type <- sapply(data$org_type, function(x) { ifelse(x == "State", "State or Local Government", x) })

# clean imcp_agency 
names(data)[which(names(data) == "imcp")] <- "imcp_agency"
data$imcp_agency <- sapply(data$imcp_agency, function(x) { ifelse(x == "No / State", "No", x)})
data$imcp_agency <- sapply(data$imcp_agency, function(x) { ifelse(x == "yes", "Yes", x)})
data$imcp_agency <- sapply(data$imcp_agency, function(x) { ifelse(is.na(x), "No", x)})

# clean date fields
data$app_date <- sapply(data$app_date, function(x) { ifelse(x == "N/A", NA, x) })
data$app_date <- mdy(data$app_date)
data$award_date <- mdy(data$award_date)

# clean preference 
data$preference <- sapply(data$preference, function(x) { ifelse(x == "no", "No", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(x == "NO", "No", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(x == "yes", "Yes", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(x == "IMCP referenced, but no preference provided", "No", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(x == "Not, specifically, but mentioned in application", "No", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(x == "Yes, PNMP/IMCP is recognized under Oregon Statute", "Yes", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(x == "Pending", "Yes", x) })
data$preference <- sapply(data$preference, function(x) { ifelse(is.na(x), "No", x) })

# identify awarded
data$awarded <- ifelse(!is.na(data$award_date), 1, 0)

# identify year applied
data$year_applied <- ifelse(!is.na(data$app_date), year(data$app_date), NA)

# identify year awarded
data$year_awarded <- ifelse(data$awarded == 1, year(data$award_date), NA)

# drop any federal agency observations for agencies (FAA) where no funding amount is ever reported for agency
# since this causes NaN for rate calculations (eg 0/0)
no_funding_reported <- data %>% filter(org_type == "Federal") %>% group_by(org) %>% 
        summarize(funding_sum = sum(funding, na.rm = TRUE)) %>% filter(funding_sum == 0) %>% select(org)
data <- data %>% filter(org != no_funding_reported$org)

# convert matching to numeric
data$matching <- as.numeric(data$matching)

# set variable for count of communities by cohort
data <- data %>% group_by(cohort) %>% mutate(cohort_comm_count = n_distinct(comm)) %>% ungroup()

# write cleaned csv to file
setwd("G:/PNP/Performance Measurement/IMCP/Data/IMCP Funding Sources/csv/r_cleaned_csv")

write_csv(data, "imcp.csv")

# summarize

# total application count/amount
data %>% filter(org_type == "Federal") %>% summarize(total_funding = sum(funding, na.rm = TRUE))
data %>% filter(org_type == "Federal") %>% tally()

# total award count/amount
data %>% filter(org_type == "Federal", awarded == 1) %>% summarize(total_funding = sum(funding, na.rm = TRUE))
data %>% filter(org_type == "Federal", awarded == 1) %>% tally()

# 1) total awarded funding
agency_total_awarded_funding <- data %>% filter(org_type == "Federal", awarded == 1) %>% group_by(org) %>% summarize(funding_sum = sum(funding, na.rm = TRUE)) %>%
        arrange(desc(funding_sum)) %>% mutate(org = factor(org, org))

agency_total_awarded_funding_plot <- ggplot(agency_total_awarded_funding, aes(x = org, y = funding_sum)) + 
        geom_bar(stat = "identity", fill = "#3366ff", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Funding", labels = dollar, limits = c(0, max(agency_total_awarded_funding$funding_sum))) +
        xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Total federal funding awarded to IMCP communities") +
        geom_text(aes(label = sprintf("$%.0fm", agency_total_awarded_funding$funding_sum / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)

# agency_total_awarded_funding_violin_plot <- ggplot(agency_total_awarded_funding, aes(x = org, y = funding_sum)) + 
#         geom_violin(aes(fill = factor(org))) + 
#         scale_y_continuous(name = "Funding", labels = dollar, limits = c(0, max(agency_total_awarded_funding$funding_sum))) +
#         xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Total agency funding awarded to IMCP communities") +
#         geom_text(aes(label = sprintf("$%.0fm", agency_total_awarded_funding$funding_sum / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)


# 2) total awarded count
agency_total_awarded_count <- data %>% filter(org_type == "Federal", awarded == 1) %>% group_by(org) %>% tally() %>%
        arrange(desc(n)) %>% mutate(org = factor(org, org))

agency_total_awarded_count_plot <- ggplot(agency_total_awarded_count, aes(x = org, y = n)) + 
        geom_bar(stat = "identity", fill = "#3366ff", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Count of awards", labels = dollar, limits = c(0, max(agency_total_awarded_count$n))) +
        xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Count of federal funding awards to IMCP communities") +
        geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25)

# 3) total applied amount
agency_total_applied_funding <- data %>% filter(org_type == "Federal") %>% group_by(org) %>% summarize(funding_sum = sum(funding, na.rm = TRUE)) %>%
        arrange(desc(funding_sum)) %>% mutate(org = factor(org, org))

agency_total_applied_funding_plot <- ggplot(agency_total_applied_funding, aes(x = org, y = funding_sum)) + 
        geom_bar(stat = "identity", fill = "#3366ff", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Funding", labels = dollar, limits = c(0, max(agency_total_applied_funding$funding_sum))) +
        xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Total federal funding applied for by IMCP communities") +
        geom_text(aes(label = sprintf("$%.0fm", agency_total_applied_funding$funding_sum / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)

# 4) total applied count
agency_total_applied_count <- data %>% filter(org_type == "Federal") %>% group_by(org) %>% tally() %>%
        arrange(desc(n)) %>% mutate(org = factor(org, org))

agency_total_applied_count_plot <- ggplot(agency_total_applied_count, aes(x = org, y = n)) + 
        geom_bar(stat = "identity", fill = "#3366ff", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Count of awards", labels = dollar, limits = c(0, max(agency_total_applied_count$n))) +
        xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Count of federal funding applications by IMCP communities") +
        geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25)

# 5) awarded funding rate
agency_awarded_funding_rate <- data %>% filter(org_type == "Federal") %>% group_by(org) %>% 
        summarize(awarded_funding = sum(funding[awarded == 1], na.rm = TRUE), 
                  awarded_funding_rate = awarded_funding / sum(funding, na.rm = TRUE), sum = sum(funding, na.rm = TRUE)) %>%
        arrange(desc(awarded_funding_rate)) %>% mutate(org = factor(org, org))

agency_awarded_funding_rate_plot <- ggplot(agency_awarded_funding_rate, aes(x = org, y = awarded_funding_rate)) + 
        geom_bar(stat = "identity", fill = "#3366ff", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Funding awarded / requested", labels = dollar, limits = c(0, max(agency_awarded_funding_rate$awarded_funding_rate))) +
        xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Agency ratio of federal funding awarded to funding applied for by IMCP communities") +
        geom_text(aes(label = sprintf("%1.0f%%", awarded_funding_rate * 100)), position = position_dodge(width = 0.9), vjust = -0.25)

# 6) avg app/award count by cohort
cohort_avg_count <- data %>% filter(org_type == "Federal") %>% group_by(cohort, awarded) %>% 
        summarize(cohort_comm_count = mean(cohort_comm_count), count = n()) %>%
        mutate(total_count = count) %>%
        mutate(total_count = ifelse((awarded == 0 & cohort == 1), count[cohort == 1 & awarded == 0] + count[cohort == 1 & awarded == 1], total_count)) %>% 
        mutate(total_count = ifelse((awarded == 0 & cohort == 2), count[cohort == 2 & awarded == 0] + count[cohort == 2 & awarded == 1], total_count)) %>% 
        mutate(avg_cohort_count = total_count / cohort_comm_count) %>% data.frame(.)

cohort_avg_count_plot <- ggplot(cohort_avg_count, aes(x = factor(cohort), y = avg_cohort_count, fill = factor(awarded))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Count", limits = c(0, max(cohort_avg_count$avg_cohort_count))) +
        xlab("Cohort") + 
        ggtitle("Average count of federal applications and awards per IMCP community") +
        geom_text(aes(label = round(avg_cohort_count, digits = 1)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels=c("1st Cohort", "2nd Cohort")) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Funding Status", breaks = c(0, 1), labels = c("Applications", "Awards")) 

# 7) avg app/award funding by cohort
cohort_avg_funding <- data %>% filter(org_type == "Federal") %>% group_by(cohort, awarded) %>% 
        summarize(comm_count = n_distinct(comm), funding = sum(funding, na.rm = TRUE)) %>% 
        mutate(total_funding = funding) %>%
        mutate(total_funding = ifelse((awarded == 0 & cohort == 1), funding[cohort == 1 & awarded == 0] + funding[cohort == 1 & awarded == 1], total_funding)) %>% 
        mutate(total_funding = ifelse((awarded == 0 & cohort == 2), funding[cohort == 2 & awarded == 0] + funding[cohort == 2 & awarded == 1], total_funding)) %>% 
        mutate(avg_cohort_funding = (total_funding / comm_count) / 1000000) %>% data.frame(.)

cohort_avg_funding_plot <- ggplot(cohort_avg_funding, aes(x = factor(cohort), y = avg_cohort_funding, fill = factor(awarded))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, limits = c(0, max(cohort_avg_funding$avg_cohort_funding))) +
        xlab("Cohort") + 
        ggtitle("Average federal funding applied for and awarded per IMCP community") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_avg_funding$avg_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels=c("1st Cohort", "2nd Cohort")) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Funding Status", breaks = c(0, 1), labels = c("Applications", "Awards")) 

# 8) total app/award count by cohort
cohort_total_count <- data %>% filter(org_type == "Federal") %>% group_by(cohort, awarded) %>% 
        summarize(count = n()) %>% 
        mutate(total_cohort_count = count) %>%
        mutate(total_cohort_count = ifelse((awarded == 0 & cohort == 1), count[cohort == 1 & awarded == 0] + count[cohort == 1 & awarded == 1], total_cohort_count)) %>% 
        mutate(total_cohort_count = ifelse((awarded == 0 & cohort == 2), count[cohort == 2 & awarded == 0] + count[cohort == 2 & awarded == 1], total_cohort_count)) %>% 
        data.frame(.)

cohort_total_count_plot <- ggplot(cohort_total_count, aes(x = factor(cohort), y = total_cohort_count, fill = factor(awarded))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Count", limits = c(0, max(cohort_total_count$total_cohort_count))) +
        xlab("Cohort") + 
        ggtitle("Total IMCP community count of federal applications and awards") +
        geom_text(aes(label = total_cohort_count), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels=c("1st Cohort", "2nd Cohort")) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Funding Status", breaks = c(0, 1), labels = c("Applications", "Awards")) 

# 9) total app/award funding by cohort
cohort_total_funding <- data %>% filter(org_type == "Federal") %>% group_by(cohort, awarded) %>% 
        summarize(funding = sum(funding, na.rm = TRUE) / 1000000) %>% 
        mutate(total_cohort_funding = funding) %>%
        mutate(total_cohort_funding = ifelse((awarded == 0 & cohort == 1), funding[cohort == 1 & awarded == 0] + funding[cohort == 1 & awarded == 1], total_cohort_funding)) %>% 
        mutate(total_cohort_funding = ifelse((awarded == 0 & cohort == 2), funding[cohort == 2 & awarded == 0] + funding[cohort == 2 & awarded == 1], total_cohort_funding)) %>% 
        data.frame(.)

cohort_total_funding_plot <- ggplot(cohort_total_funding, aes(x = factor(cohort), y = total_cohort_funding, fill = factor(awarded))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_total_funding$total_cohort_funding))) +
        xlab("Cohort") + 
        ggtitle("Total IMCP community federal funding applied for and awarded") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_total_funding$total_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels=c("1st Cohort", "2nd Cohort")) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Funding Status", breaks = c(0, 1), labels = c("Applications", "Awards")) 


# 10) total applied funding by year by cohort
cohort_total_applied_funding_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>%
        summarize(total_cohort_funding = sum(funding, na.rm = TRUE) / 1000000) %>% data.frame(.)

cohort_total_applied_funding_year_plot <- ggplot(cohort_total_applied_funding_year, aes(x = factor(year_applied), y = total_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_total_applied_funding_year$total_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Total IMCP community federal funding applied for") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_total_applied_funding_year$total_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_total_applied_funding_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 11) total awarded funding by year by cohort
cohort_total_awarded_funding_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_awarded) %>%
        summarize(total_cohort_funding = sum(funding, na.rm = TRUE) / 1000000) %>% data.frame(.)

cohort_total_applied_funding_year_plot <- ggplot(cohort_total_awarded_funding_year, aes(x = factor(year_awarded), y = total_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_total_awarded_funding_year$total_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Total IMCP community federal funding awarded") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_total_awarded_funding_year$total_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_total_awarded_funding_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 12) awarded rate funding by year by cohort
cohort_awarded_funding_rate_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>%
        summarize(awarded_funding = sum(funding[awarded == 1], na.rm = TRUE), 
                  awarded_funding_rate = awarded_funding / sum(funding, na.rm = TRUE), sum = sum(funding, na.rm = TRUE)) %>% 
        data.frame(.)

cohort_awarded_funding_rate_year_plot <- ggplot(cohort_awarded_funding_rate_year, aes(x = factor(year_applied), y = awarded_funding_rate, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding awarded / applied", label = percent, 
                           limits = c(0, max(cohort_awarded_funding_rate_year$awarded_funding_rate))) +
        xlab("FY") + 
        ggtitle("Ratio of federal funding awarded to funding applied for IMCP communities") +
        geom_text(aes(label = sprintf("%.0f%%", cohort_awarded_funding_rate_year$awarded_funding_rate * 100)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_awarded_funding_rate_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 12.5) awarded rate count by year by cohort
cohort_awarded_count_rate_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>%
        summarize(app_count = n(), award_count = sum(awarded, na.rm = TRUE),  
                  awarded_count_rate = award_count / app_count) %>% 
        data.frame(.)

cohort_awarded_count_rate_year_plot <- ggplot(cohort_awarded_count_rate_year, aes(x = factor(year_applied), y = awarded_count_rate, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding awarded / applied", label = percent, 
                           limits = c(0, max(cohort_awarded_count_rate_year$awarded_count_rate))) +
        xlab("FY") + 
        ggtitle("Count of awards per application for by IMCP communities") +
        geom_text(aes(label = sprintf("%.0f%%", cohort_awarded_count_rate_year$awarded_count_rate * 100)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_awarded_count_rate_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 


# 13) total count applications by year by cohort
cohort_count_applied_funding_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>%
        summarize(count_cohort_funding = n()) %>% data.frame(.)

cohort_count_applied_funding_year_plot <- ggplot(cohort_count_applied_funding_year, aes(x = factor(year_applied), y = count_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Count of applications", 
                           limits = c(0, max(cohort_count_applied_funding_year$count_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Total count of IMCP community federal funding applications") +
        geom_text(aes(label = cohort_count_applied_funding_year$count_cohort_funding), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_count_applied_funding_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 14) total count of awards by year by cohort
cohort_total_awarded_count_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_awarded) %>%
        summarize(total_cohort_count = n()) %>% data.frame(.)

cohort_total_awarded_count_year_plot <- ggplot(cohort_total_awarded_count_year, aes(x = factor(year_awarded), y = total_cohort_count, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Count of awards", 
                           limits = c(0, max(cohort_total_awarded_count_year$total_cohort_count))) +
        xlab("FY") + 
        ggtitle("Total count of IMCP community federal funding awards") +
        geom_text(aes(label = cohort_total_awarded_count_year$total_cohort_count), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_total_awarded_count_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 15) avg applied for funding by cohort by year
cohort_avg_applied_funding_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(app_count = n(), total_funding = sum(funding, na.rm = TRUE)) %>% 
        mutate(avg_cohort_funding = (total_funding / app_count) / 1000000) %>% data.frame(.)

cohort_avg_applied_funding_year_plot <- ggplot(cohort_avg_applied_funding_year, aes(x = factor(year_applied), y = avg_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_applied_funding_year$avg_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Average amount of federal funding requested per IMCP community application") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_avg_applied_funding_year$avg_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_applied_funding_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 15.b) avg applied for funding by cohort by year
cohort_avg_applied_funding_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(total_funding = sum(funding, na.rm = TRUE), cohort_comm_count = mean(cohort_comm_count)) %>% 
        mutate(avg_cohort_funding = (total_funding / cohort_comm_count) / 1000000) %>% data.frame(.)

cohort_avg_applied_funding_year_plot <- ggplot(cohort_avg_applied_funding_year, aes(x = factor(year_applied), y = avg_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_applied_funding_year$avg_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Average amount of federal funding requested per IMCP community") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_avg_applied_funding_year$avg_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_applied_funding_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 16) avg awarded funding by cohort by year
cohort_avg_awarded_funding_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_awarded) %>% 
        summarize(award_count = n(), total_funding = sum(funding, na.rm = TRUE)) %>% 
        mutate(avg_cohort_funding = (total_funding / award_count) / 1000000) %>% data.frame(.)

cohort_avg_awarded_funding_year_plot <- ggplot(cohort_avg_awarded_funding_year, aes(x = factor(year_awarded), y = avg_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_awarded_funding_year$avg_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Average amount of federal funding per IMCP community award") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_avg_awarded_funding_year $avg_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_awarded_funding_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 16.b) avg awarded funding by cohort by year
cohort_avg_awarded_funding_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_awarded) %>% 
        summarize(total_funding = sum(funding, na.rm = TRUE), cohort_comm_count = mean(cohort_comm_count)) %>% 
        mutate(avg_cohort_funding = (total_funding / cohort_comm_count) / 1000000) %>% data.frame(.)

cohort_avg_awarded_funding_year_plot <- ggplot(cohort_avg_awarded_funding_year, aes(x = factor(year_awarded), y = avg_cohort_funding, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_awarded_funding_year$avg_cohort_funding))) +
        xlab("FY") + 
        ggtitle("Average amount of federal funding awarded per IMCP community") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_avg_awarded_funding_year $avg_cohort_funding)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_awarded_funding_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 17) avg applied count by cohort by year
cohort_avg_applied_count_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(total_count = n(), cohort_comm_count = mean(cohort_comm_count)) %>% 
        mutate(avg_cohort_count = (total_count / cohort_comm_count)) %>% data.frame(.)

cohort_avg_applied_count_year_plot <- ggplot(cohort_avg_applied_count_year, aes(x = factor(year_applied), y = avg_cohort_count, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Avg. count of applications", label = dollar, 
                           limits = c(0, max(cohort_avg_applied_count_year$avg_cohort_count))) +
        xlab("FY") + 
        ggtitle("Average count of federal funding applications per IMCP community") +
        geom_text(aes(label = sprintf("%.1f", cohort_avg_applied_count_year$avg_cohort_count)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_applied_count_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 18) avg awarded count by cohort by year
cohort_avg_awarded_count_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(total_count = n(), cohort_comm_count = mean(cohort_comm_count)) %>% 
        mutate(avg_cohort_count = (total_count / cohort_comm_count)) %>% data.frame(.)

cohort_avg_awarded_count_year_plot <- ggplot(cohort_avg_awarded_count_year, aes(x = factor(year_applied), y = avg_cohort_count, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Avg. count of awards", label = dollar, 
                           limits = c(0, max(cohort_avg_awarded_count_year$avg_cohort_count))) +
        xlab("FY") + 
        ggtitle("Average count of federal funding awards per IMCP community") +
        geom_text(aes(label = sprintf("%.1f", cohort_avg_awarded_count_year$avg_cohort_count)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_awarded_count_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 19) total pi applied by cohort by year
cohort_total_applied_pi_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>%
        summarize(total_cohort_pi = sum(matching, na.rm = TRUE) / 1000000) %>% data.frame(.)

cohort_total_applied_pi_year_plot <- ggplot(cohort_total_applied_pi_year, aes(x = factor(year_applied), y = total_cohort_pi, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_total_applied_pi_year$total_cohort_pi))) +
        xlab("FY") + 
        ggtitle("Total matching funds for IMCP community applications") +
        geom_text(aes(label = sprintf("$%.0fm", cohort_total_applied_pi_year$total_cohort_pi)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_total_applied_pi_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 20) total awarded pi by year by cohort
cohort_total_awarded_pi_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_applied) %>%
        summarize(total_cohort_pi = sum(matching, na.rm = TRUE) / 1000000) %>% data.frame(.)

cohort_total_applied_funding_year_plot <- ggplot(cohort_total_awarded_pi_year, aes(x = factor(year_awarded), y = total_cohort_pi, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_total_awarded_pi_year$total_cohort_pi))) +
        xlab("FY") + 
        ggtitle("Total matching funds for IMCP community awards") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_total_awarded_pi_year$total_cohort_pi)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_total_awarded_pi_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 21) avg applied for pi by cohort by year
cohort_avg_applied_pi_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(app_count = n(), total_pi = sum(matching, na.rm = TRUE)) %>% 
        mutate(avg_cohort_pi = (total_pi / app_count) / 1000000) %>% data.frame(.)

cohort_avg_applied_pi_year_plot <- ggplot(cohort_avg_applied_pi_year, aes(x = factor(year_applied), y = avg_cohort_pi, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_applied_pi_year$avg_cohort_pi))) +
        xlab("FY") + 
        ggtitle("Average amount of matching funds per IMCP community application") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_avg_applied_pi_year$avg_cohort_pi)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_applied_pi_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 21.b) avg applied for pi by cohort by year
cohort_avg_applied_pi_year <- data %>% filter(org_type == "Federal", !is.na(year_applied)) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(total_pi = sum(matching, na.rm = TRUE), cohort_comm_count = mean(cohort_comm_count)) %>% 
        mutate(avg_cohort_pi = (total_pi / cohort_comm_count) / 1000000) %>% data.frame(.)

cohort_avg_applied_pi_year_plot <- ggplot(cohort_avg_applied_pi_year, aes(x = factor(year_applied), y = avg_cohort_pi, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_applied_pi_year$avg_cohort_pi))) +
        xlab("FY") + 
        ggtitle("Average amount of matching funds pledged in application per IMCP community") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_avg_applied_pi_year$avg_cohort_pi)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_applied_pi_year$year_applied))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort"))

# 22) avg awarded pi by cohort by year
cohort_avg_awarded_pi_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(award_count = n(), total_pi = sum(matching, na.rm = TRUE)) %>% 
        mutate(avg_cohort_pi = (total_pi / award_count) / 1000000) %>% data.frame(.)

cohort_avg_awarded_pi_year_plot <- ggplot(cohort_avg_awarded_pi_year, aes(x = factor(year_awarded), y = avg_cohort_pi, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_awarded_pi_year$avg_cohort_pi))) +
        xlab("FY") + 
        ggtitle("Average amount of matching funds per IMCP community award") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_avg_awarded_pi_year$avg_cohort_pi)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_awarded_pi_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# 22.b) avg awarded pi by cohort by year
cohort_avg_awarded_pi_year <- data %>% filter(org_type == "Federal", awarded == 1) %>% 
        group_by(cohort, year_applied) %>% 
        summarize(total_pi = sum(matching, na.rm = TRUE), cohort_comm_count = mean(cohort_comm_count)) %>% 
        mutate(avg_cohort_pi = (total_pi / cohort_comm_count) / 1000000) %>% data.frame(.)

cohort_avg_awarded_pi_year_plot <- ggplot(cohort_avg_awarded_pi_year, aes(x = factor(year_awarded), y = avg_cohort_pi, fill = factor(cohort))) + 
        geom_bar(stat = "identity", width = .85, position = position_dodge()) + 
        scale_y_continuous(name = "Funding (millions)", label = dollar, 
                           limits = c(0, max(cohort_avg_awarded_pi_year$avg_cohort_pi))) +
        xlab("FY") + 
        ggtitle("Average amount of award matching funds per IMCP community") +
        geom_text(aes(label = sprintf("$%.1fm", cohort_avg_awarded_pi_year$avg_cohort_pi)), position = position_dodge(width = 0.9), vjust = -0.25) +
        scale_x_discrete(labels = sort(unique(cohort_avg_awarded_pi_year$year_awarded))) + 
        theme_hc() + scale_colour_economist() +
        scale_fill_brewer(palette = "Blues", name = "Cohort", breaks = c(1, 2), labels = c("1st Cohort", "2nd Cohort")) 

# imcp preference awarded funding 
# has zero dollars for some agencies
# recommend disregarding for now

# agency_preference_awarded_funding <- data %>% filter(org_type == "Federal", preference == "Yes") %>% group_by(org) %>% summarize(funding_sum = sum(funding, na.rm = TRUE)) %>%
#         arrange(desc(funding_sum)) %>% mutate(org = factor(org, org))
# agency_preference_awarded_funding_plot <- ggplot(agency_preference_awarded_funding, aes(x = org, y = funding_sum)) + 
#         geom_bar(stat = "identity", fill = "#3366ff", width = .85, position = position_dodge(width = 1.7)) + 
#         scale_y_continuous(name = "Funding", labels = dollar, limits = c(0, max(agency_preference_awarded_funding$funding_sum))) +
#         xlab("Agency") + theme_hc() + scale_colour_economist() + ggtitle("Agency awarded funding for IMCP communities (with IMCP preference)") +
#         geom_text(aes(label = sprintf("$%.0fm", agency_preference_awarded_funding$funding_sum / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)
# 


# avg funding/count by org_type? - not great because of data quality issues with org_type


# avg private investment
# avg private investment per grant
# avg private investment / funding ratio

