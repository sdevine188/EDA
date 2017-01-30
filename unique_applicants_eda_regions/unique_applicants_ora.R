library(readr)
library(stringr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# disable scientific notation
options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/Applicant Control Group/unique_applicants_ora_20161020")

# tally unique applicants by duns
# duns_app with partnership planning
duns_app_all <- shiny %>% filter(!is.na(DUNS), FY > 2000) %>% group_by(DUNS, Region.Name) %>% 
        summarize(app_count = n()) %>% arrange(desc(app_count)) %>% data.frame(.)
head(duns_app_all)
dim(duns_app)

# duns_award excluding partnership planning and TAAF by region
duns_app_region <- shiny %>% filter(!is.na(DUNS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                             Prog.Tool.Name != "Univ. Center") %>% 
        group_by(DUNS, Region.Name) %>% 
        summarize(app_count = n()) %>% arrange(desc(app_count)) %>% data.frame(.)
head(duns_app_region)
dim(duns_app_region)

# duns_award excluding partnership planning and TAAF total
duns_app_total <- shiny %>% filter(!is.na(DUNS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                    Prog.Tool.Name != "Univ. Center") %>% 
        group_by(DUNS) %>% 
        summarize(app_count = n()) %>% mutate(Region.Name = "EDA") %>% arrange(desc(app_count)) %>% data.frame(.)
head(duns_app_total)
dim(duns_app_total)


# duns_award including partership planning
duns_award_all <- shiny %>% filter(Status == "Approved", !is.na(DUNS), FY > 2000) %>% group_by(DUNS, Region.Name) %>% 
        summarize(award_count = n()) %>% 
        arrange(desc(award_count)) %>% data.frame(.)
head(duns_award_all)
dim(duns_award_all)

# duns_award excluding partnership planning
duns_award_region <- shiny %>% filter(Status == "Approved", !is.na(DUNS), FY > 2000, 
                                      !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), 
                               Program != "Trade Adjustment Assistance for Firms", Prog.Tool.Name != "Univ. Center") %>% 
        group_by(DUNS, Region.Name) %>% 
        summarize(award_count = n()) %>% 
        arrange(desc(award_count)) %>% data.frame(.)
head(duns_award_region)
dim(duns_award_region)

# duns_award excluding partnership planning and TAAF total
duns_award_total <- shiny %>% filter(Status == "Approved", !is.na(DUNS), FY > 2000, !grepl("Partnership Planning", 
                                        Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                   Prog.Tool.Name != "Univ. Center") %>% 
        group_by(DUNS) %>% 
        summarize(award_count = n()) %>% mutate(Region.Name = "EDA") %>% arrange(desc(award_count)) %>% data.frame(.)
head(duns_award_total)
dim(duns_award_total)

# duns_award excluding partnership planning and TAAF total by FY
duns_award_total_year <- shiny %>% filter(Status == "Approved", !is.na(DUNS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                     Prog.Tool.Name != "Univ. Center") %>% 
        group_by(DUNS, FY) %>% 
        summarize(app_count = n()) %>% mutate(Region.Name = "EDA") %>% arrange(desc(app_count)) %>% data.frame(.)
head(duns_award_total_year)
dim(duns_award_total_year)

# explore by join data to duns
# note that some of the duns_award_region duns will have had partnership planning etc and those will show up, but you've excluded them from duns_award_region file 
duns_award_region_join <- left_join(duns_award_region, shiny, by = "DUNS")
dim(duns_award_region_join)
duns_award_region_join <- duns_award_region_join %>% filter(Status == "Approved", !is.na(DUNS), FY > 2000, 
                                         !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), 
                                         Program != "Trade Adjustment Assistance for Firms", Prog.Tool.Name != "Univ. Center") %>%
        select(Appl.Short.Name, DUNS, award_count, Program, Prog.Tool.Name, Appr.Desc, Appropriation, Prog.Tool.Name) %>% head(.)

# check how many have NA for Region
duns_award_region %>% group_by(Region.Name) %>% summarize(count = sum(award_count))
duns_award_region %>% group_by(Region.Name) %>% tally()

# graph app_count per duns

# apps all regions
eda_duns_app_violin <- ggplot(duns_app_total, aes(x = Region.Name, y = app_count)) + geom_violin(fill = "blue") +
        ggtitle("Distribution of applications per DUNS for all EDA, since 2000, excl. TAAF, UC, EDDs") +
        theme_hc() + scale_colour_hc() + xlab(NULL) + ylab("Count of applications per DUNS") 
dev.copy(png,'eda_duns_app_violin.png', height = 700, width = 600)
dev.off()

ggplot(duns_app_total, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
eda_duns_app_hist <- ggplot(duns_app_total, aes(x = app_count)) + geom_histogram(bins = 50, fill = "blue") + stat_bin(binwidth = 1, geom = "text", 
                        aes(label = ..count..), vjust = -1.5) + 
        ggtitle("Count of applications per DUNS for all EDA, since 2000, excl. TAAF, UC, EDDs") + 
        scale_x_continuous(breaks = seq(1, 30, 1)) + ylab("Count of DUNS") + xlab("Count of applications per DUNS") +
        theme_hc() + scale_colour_economist()
dev.copy(png,'eda_duns_app_hist.png', height = 700, width = 600)
dev.off()

# apps by regions
# will remove 454 (as of 11/6/2016) records with NA for Region.Name
# ggplot(duns_app_region, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name)) + 
#         ggtitle("DUNS App Count by Region") + geom_text(aes(label = duns_app_region$app_count))
regions_duns_app_violin <- ggplot(duns_app_region, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name)) +
        scale_fill_brewer(palette = "Set3") +
        ggtitle("Distribution of applications per DUNS by Region, since 2000, excl. TAAF, UC, EDDs") +
        theme_hc() + scale_colour_hc() + xlab(NULL) + ylab("Count of applications per DUNS")
dev.copy(png,'regions_duns_app_violin.png', height = 700, width = 600)
dev.off()

ggplot(duns_app_region, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
# ggplot(duns_app_region, aes(x = app_count)) + geom_histogram(bins = 50, fill = "blue") + stat_bin(binwidth=1, 
#                 geom="text", aes(label=..count..), vjust=-1.5) +
#         ggtitle("Count of applications per DUNS by Region, since 2000, excl. TAAF, UC, EDDs") + 
#         scale_x_continuous(breaks = seq(1, 30, 1)) + ylab("Count of unique DUNS") + xlab("Count of applications per DUNS") +
#         theme_hc() + scale_colour_economist()

# all regions
eda_duns_award_violin <- ggplot(duns_award_total, aes(x = Region.Name, y = award_count)) + geom_violin(fill = "blue") +
        ggtitle("Distribution of awards per DUNS for all EDA, since 2000, excl. TAAF, UC, EDDs") +
        theme_hc() + scale_colour_economist() + xlab(NULL) + ylab("Count of awards per DUNS")
dev.copy(png, "eda_duns_award_violin.png", height = 700, width = 600)
dev.off()

ggplot(duns_award_total, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()

eda_duns_award_hist <- ggplot(duns_award_total, aes(x = award_count)) + geom_histogram(bins = 50, fill = "blue") + 
        stat_bin(binwidth = 1, geom = "text", aes(label = ..count..), vjust = -1.5) + 
        ggtitle("Count of awards per DUNS for all EDA, since 2000, excl. TAAF, UC, EDDs") + 
        scale_x_continuous(breaks = seq(1, 30, 1)) + ylab("Count of DUNS") + xlab("Count of awards per DUNS") +
        theme_hc() + scale_colour_economist()
dev.copy(png, "eda_duns_award_hist.png", height = 700, width = 600)
dev.off()

# by regions
# will remove ~57 records with NA for region.name 
regions_duns_award_violin <- ggplot(duns_award_region, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name)) +
        scale_fill_brewer(palette = "Set3") + xlab(NULL) + ylab("Count of awards per DUNS") + 
        ggtitle("Distribution of awards per DUNS by Region, since 2000, excl. TAAF, UC, EDDs") +
        theme_hc() + scale_colour_economist() + scale_y_continuous(breaks = seq(1, max(duns_award_region$award_count), 2))
dev.copy(png, "regions_duns_award_violin.png", height = 700, width = 600)
dev.off()
pdf("regions_duns_award_violin.pdf")
regions_duns_award_violin
dev.off()

ggplot(duns_award_region, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
# ggplot(duns_award_region, aes(x = award_count)) + geom_histogram(bins = 50) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5)







# tally unique applicants by ein
# ein_app with partnership planning
ein_app_all <- shiny %>% filter(!is.na(IRS), FY > 2000) %>% group_by(IRS, Region.Name) %>% 
        summarize(app_count = n()) %>% arrange(desc(app_count)) %>% data.frame(.)
head(ein_app_all)
dim(ein_app_all)

# ein_award excluding partnership planning and TAAF by region
ein_app_region <- shiny %>% filter(!is.na(IRS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                    Prog.Tool.Name != "Univ. Center") %>% 
        group_by(IRS, Region.Name) %>% 
        summarize(app_count = n()) %>% arrange(desc(app_count)) %>% data.frame(.)
head(ein_app_region)
dim(ein_app_region)

# ein_award excluding partnership planning and TAAF total
ein_app_total <- shiny %>% filter(!is.na(IRS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                   Prog.Tool.Name != "Univ. Center") %>% 
        group_by(IRS) %>% 
        summarize(app_count = n()) %>% mutate(Region.Name = "EDA") %>% arrange(desc(app_count)) %>% data.frame(.)
head(ein_app_total)
dim(ein_app_total)


# ein_award including partership planning
ein_award_all <- shiny %>% filter(Status == "Approved", !is.na(IRS), FY > 2000) %>% group_by(IRS, Region.Name) %>% 
        summarize(award_count = n()) %>% 
        arrange(desc(award_count)) %>% data.frame(.)
head(ein_award_all)
dim(ein_award_all)

# ein_award excluding partnership planning by region
ein_award_region <- shiny %>% filter(Status == "Approved", !is.na(IRS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), 
                                      Program != "Trade Adjustment Assistance for Firms", Prog.Tool.Name != "Univ. Center") %>% 
        group_by(IRS, Region.Name) %>% 
        summarize(award_count = n()) %>% 
        arrange(desc(award_count)) %>% data.frame(.)
head(ein_award_region)
dim(ein_award_region)

# ein_award excluding partnership planning and TAAF total
ein_award_total <- shiny %>% filter(Status == "Approved", !is.na(IRS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                     Prog.Tool.Name != "Univ. Center") %>% 
        group_by(IRS) %>% 
        summarize(award_count = n()) %>% mutate(Region.Name = "EDA") %>% arrange(desc(award_count)) %>% data.frame(.)
head(ein_award_total)
dim(ein_award_total)

# ein_award excluding partnership planning and TAAF total
ein_award_total_year <- shiny %>% filter(Status == "Approved", !is.na(IRS), FY > 2000, !grepl("Partnership Planning", Prog.Tool.Name, ignore.case), Program != "Trade Adjustment Assistance for Firms",
                                          Prog.Tool.Name != "Univ. Center") %>% 
        group_by(IRS, FY) %>% 
        summarize(app_count = n()) %>% mutate(Region.Name = "EDA") %>% arrange(desc(app_count)) %>% data.frame(.)
head(ein_award_total_year)
dim(ein_award_total_year)

# explore by join data to ein
# note that some of the ein_award_region ein will have had partnership planning etc and those will show up, but you've excluded them from ein_award_region file 
ein_award_region_join <- left_join(ein_award_region, shiny, by = "IRS")
dim(ein_award_region_join)
ein_award_region_join %>% select(Appl.Short.Name, IRS, award_count, Program, Appr.Desc, Appropriation, Prog.Tool.Name) %>% head(.)

# check how many have NA for Region
ein_award_region %>% group_by(Region.Name) %>% summarize(count = sum(award_count))
ein_award_region %>% group_by(Region.Name) %>% tally()

# graph app_count per ein

# all regions
ggplot(ein_app_total, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name))
ggplot(ein_app_total, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
ggplot(ein_app_total, aes(x = app_count)) + geom_histogram(bins = 50) + stat_bin(binwidth = 1, geom = "text", aes(label = ..count..), vjust = -1.5) + ggtitle("EIN App Count for all EDA")

# by regions
ggplot(ein_app, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name))
ggplot(ein_app, aes(x = Region.Name, y = app_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
ggplot(ein_app, aes(x = app_count)) + geom_histogram(bins = 50) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5)

# all regions
ggplot(ein_award_total, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name))
ggplot(ein_award_total, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
ggplot(ein_award_total, aes(x = award_count)) + geom_histogram(bins = 50) + stat_bin(binwidth = 1, geom = "text", aes(label = ..count..), vjust = -1.5) + ggtitle("EIN Award Count for all EDA")

# by regions
ggplot(ein_award_region, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name))
ggplot(ein_award_region, aes(x = Region.Name, y = award_count)) + geom_violin(aes(fill = Region.Name)) + geom_boxplot()
ggplot(ein_award_region, aes(x = award_count)) + geom_histogram(bins = 50) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.5)




# explore duns vs ein to see missing
# create output file for bill summarizing duns missing and present, and award/app count, by year
awards <- shiny %>% filter(Status == "Approved", FY > 2000)
dim(awards)
sum(is.na(awards$DUNS))
sum(is.na(awards$IRS))
awards_duns <- awards %>% group_by(FY) %>% summarize(awards_duns_na_count = sum(is.na(DUNS)),
                                                 awards_duns_present_count = sum(!is.na(DUNS)))
# write_csv(na_duns, "na_duns.csv")

apps <- shiny %>% filter(FY > 2000, FY < 2017)
apps_duns <- apps %>% group_by(FY) %>% summarize(apps_duns_present_count = sum(!is.na(DUNS)),
                                                 apps_duns_na_count = sum(is.na(DUNS)))
# write_csv(duns_present, "duns_present.csv")

total_awards <- awards %>% group_by(FY) %>% summarize(total_awards_count = n())
# write_csv(total_awards, "total_awards.csv")

total_apps <- apps %>% group_by(FY) %>% summarize(total_apps_count = n())

output <- cbind(total_apps, apps_duns[ , 2:3], total_awards[ , 2], awards_duns[ , 2:3])
write_csv(output, "output.csv")
