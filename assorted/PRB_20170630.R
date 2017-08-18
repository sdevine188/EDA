library(dplyr)
library(stringr)
library(lubridate)
library(scales)
library(readr)

# disable scientific notation
options(scipen=999)

setwd("G:/PNP/Performance Measurement/Performance Review Board PRB/FY 2017")
list.files()
allocations <- read_csv("Allocations_fy2017.csv")
glimpse(allocations)

setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")

setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Local.Applicant.Match = col_number(), Total.Proj.Cost = col_number(), EDA.Funding = col_number(),
                                                        Est.Private.Investment = col_number(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(),
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))

# set fy
year <- 2017

# set order of regions
region_order <- c("Atlanta", "Austin", "Chicago", "Denver", "Philadelphia", "Seattle")

# 1-Exports & FDI (PW & EAA Only)

# check because PRO has no awards with initiative code 27 or 28
shiny %>% filter(Region.Name == "Philadelphia", Status == "Approved", FY == "2017",
                 grepl("export|fdi", General_Descr, ignore.case = TRUE) |
                 grepl("export|fdi", Project.Short.Descrip, ignore.case = TRUE) |
                 grepl("export|fdi", GNS_Descr, ignore.case = TRUE) |
                 grepl("export|fdi", Scope_Of_Work, ignore.case = TRUE)) %>% tally()

PRB1 <- filter(shiny, grepl("27|28", Initiatives), grepl("20|70", Appr.Code), FY == year, Status=="Approved")
PRB1 <- PRB1 %>% group_by(Region.Name) %>% summarize(Numerator = sum(EDA.Funding, na.rm = TRUE))

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB1$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB1$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB1 <- rbind(atro_row, PRB1)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB1_top <- PRB1[1:missing_row_index - 1, ]
                PRB1_bottom <- PRB1[(missing_row_index):nrow(PRB1), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB1 <- rbind(PRB1_top, missing_row, PRB1_bottom)
        }
}

# 2-Adv Manufacturing & Innovation (PW & EAA Only)
PRB2 <- filter(shiny, grepl("AM|WT|03|24|26|29|32|33|34", Initiatives), grepl("20|70", Appr.Code), 
               FY == year,Status == "Approved")
PRB2 <- PRB2 %>% group_by(Region.Name) %>% summarize(Numerator = sum(EDA.Funding, na.rm = TRUE))

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB2$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB2$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB2 <- rbind(atro_row, PRB2)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB2_top <- PRB2[1:missing_row_index - 1, ]
                PRB2_bottom <- PRB2[(missing_row_index):nrow(PRB2), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB2 <- rbind(PRB2_top, missing_row, PRB2_bottom)
        }
}

# 3-Resiliency(PM) (PW & EAA Only)
PRB3 <- filter(shiny, grepl("PM", Initiatives), grepl("20|70", Appr.Code), FY == year, Status == "Approved")
PRB3 <- PRB3 %>% group_by(Region.Name) %>% summarize(Numerator = sum(EDA.Funding, na.rm = TRUE))

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB3$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB3$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB3 <- rbind(atro_row, PRB3)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB3_top <- PRB3[1:missing_row_index - 1, ]
                PRB3_bottom <- PRB3[(missing_row_index):nrow(PRB3), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB3 <- rbind(PRB3_top, missing_row, PRB3_bottom)
        }
}

# 4-90% EDAP(PW & EAA Only) Obligations (current fy allocation)
PRB4 <- filter(shiny, grepl("20|70", Appr.Code), FY == year, Status == "Approved")
PRB4 <- PRB4 %>% group_by(Region.Name) %>% summarize(Numerator = sum(EDA.Funding, na.rm = TRUE))

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB4$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB4$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB4 <- rbind(atro_row, PRB4)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB4_top <- PRB4[1:missing_row_index - 1, ]
                PRB4_bottom <- PRB4[(missing_row_index):nrow(PRB4), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB4 <- rbind(PRB4_top, missing_row, PRB4_bottom)
        }
}

# 5-90% EDAP(PW & EAA Only) Obligations (current FY allocation + prior year funds)
PRB5 <- filter(shiny, grepl("20|70", Appr.Code), FY == year, Status=="Approved")
PRB5 <- PRB5 %>% group_by(Region.Name) %>% summarize(Numerator = sum(EDA.Funding, na.rm = TRUE))

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB5$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB5$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB5 <- rbind(atro_row, PRB5)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB5_top <- PRB5[1:missing_row_index - 1, ]
                PRB5_bottom <- PRB5[(missing_row_index):nrow(PRB5), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB5 <- rbind(PRB5_top, missing_row, PRB5_bottom)
        }
}

# 6-Partnership Planning
# shiny %>% filter(grepl("40", Appr.Code), grepl("PM", Initiatives), FY == year, Status == "Approved") %>% 
#         group_by(Region.Name) %>% tally()

PRB6a <- filter(shiny, grepl("PM", Initiatives), grepl("40", Appr.Code), 
                grepl("Partnership Planning", Appr.Desc) | grepl("Partnership Planning", Prog.Tool.Name), 
                FY == year, Status == "Approved")
PRB6a <- PRB6a %>% group_by(Region.Name) %>% summarize(Numerator = n() )

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB6a$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB6a$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB6a <- rbind(atro_row, PRB6a)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB6a_top <- PRB6a[1:missing_row_index - 1, ]
                PRB6a_bottom <- PRB6a[(missing_row_index):nrow(PRB6a), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB6a <- rbind(PRB6a_top, missing_row, PRB6a_bottom)
        }
}

PRB6b <- filter(shiny, grepl("40", Appr.Code), grepl("Partnership Planning", Appr.Desc) | grepl("Partnership Planning", 
                Prog.Tool.Name), FY == year, Status=="Approved")
PRB6b <- PRB6b %>% group_by(Region.Name) %>% summarize(Denominator=n())

# handle cases where region has zero awards for this performance measure
if(sum(region_order %in% PRB6b$Region.Name) != 6) {
        
        # identify regions with zero awards
        region_index <- region_order %in% PRB6b$Region.Name
        missing_regions <- sort(region_order[which(region_index == FALSE)])
        
        # loop through regions with zero awards, adding in a placeholder row to showing they have zero awards
        for(i in 1:length(missing_regions)) {
                if(missing_regions[1] == "Atlanta") {
                        atro_row <- data.frame(Region.Name = c("Atlanta"), Numerator = c(0))
                        PRB6b <- rbind(atro_row, PRB6b)
                }
                missing_row_index <- which(region_order == missing_regions[i])
                PRB6b_top <- PRB6b[1:missing_row_index - 1, ]
                PRB6b_bottom <- PRB6b[(missing_row_index):nrow(PRB6b), ]
                missing_row <- data.frame(Region.Name = c(missing_regions[i]), Numerator = c(0))
                PRB6b <- rbind(PRB6b_top, missing_row, PRB6b_bottom)
        }
}

########################################################


#Atlanta
#measure 1
ATRO1 <- PRB1 %>% filter(Region.Name == "Atlanta") %>% select(Numerator)
ATRO1d <- allocations %>% filter(Region == "Atlanta") %>% select(Year)
t1 <- .3
atroa1 <- ATRO1/ATRO1d
atropta <- atroa1/t1

ATRO_Export_FDI <- data.frame(ATRO1, ATRO1d, t1, atroa1, atropta)
names(ATRO_Export_FDI) <- NULL
names(ATRO_Export_FDI) <- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 2
ATRO2 <- PRB2 %>% filter(Region.Name == "Atlanta") %>% select(Numerator)
ATRO2d <- allocations %>% filter(Region == "Atlanta") %>% select(Year)
t2 <- .35
atroa2 <- ATRO2/ATRO2d
atropta2 <- atroa2/t2

ATRO_AM_INNOVATE <- data.frame(ATRO2, ATRO2d, t2, atroa2, atropta2)
names(ATRO_AM_INNOVATE)<-NULL
names(ATRO_AM_INNOVATE)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 3
ATRO3 <- PRB3 %>% filter(Region.Name == "Atlanta") %>% select(Numerator)
ATRO3d <- allocations %>% filter(Region == "Atlanta") %>% select(Year)
t3 <- .07
atroa3 <- ATRO3/ATRO3d
atropta3 <- atroa3/t3

ATRO_RESILIENCY <- data.frame(ATRO3, ATRO3d, t3, atroa3, atropta3)
names(ATRO_RESILIENCY)<-NULL
names(ATRO_RESILIENCY)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 4
ATRO4 <- PRB4 %>% filter(Region.Name == "Atlanta") %>% select(Numerator)
ATRO4d <- allocations %>% filter(Region == "Atlanta") %>% select(Year)
t4 <- .9
atroa4 <- ATRO4/ATRO4d
atropta4 <- atroa4/t4

ATRO_90pct <- data.frame(ATRO4, ATRO4d, t4, atroa4, atropta4)
names(ATRO_90pct)<-NULL
names(ATRO_90pct)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 5
ATRO5 <- PRB5 %>% filter(Region.Name == "Atlanta") %>% select(Numerator)
ATRO5d <- allocations %>% filter(Region == "Atlanta") %>% select(Total)
t5 <- .9
atroa5 <- ATRO5/ATRO5d
atropta5 <- atroa5/t5

ATRO_90pcttotal <- data.frame(ATRO5, ATRO5d, t5, atroa5, atropta5)
names(ATRO_90pcttotal)<-NULL
names(ATRO_90pcttotal)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 6
ATRO6 <- PRB6a %>% filter(Region.Name == "Atlanta") %>% select(Numerator)
ATRO6d <- PRB6b %>% filter(Region.Name == "Atlanta") %>% select(Denominator)
t6 <- .7
atroa6 <- ATRO6/ATRO6d
atropta6 <- atroa6/t6

ATRO_CEDS <- data.frame(ATRO6, ATRO6d, t6, atroa6, atropta6)
names(ATRO_CEDS)<-NULL
names(ATRO_CEDS)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")


#################################################


#Austin
#measure 1
AURO1 <- PRB1 %>% filter(Region.Name == "Austin") %>% select(Numerator)
AURO1d <- allocations %>% filter(Region == "Austin") %>% select(Year)
t1 <- .3
AUROa1 <- AURO1/AURO1d
AUROpta <- AUROa1/t1

AURO_Export_FDI <- data.frame(AURO1, AURO1d, t1, AUROa1, AUROpta)
names(AURO_Export_FDI)<-NULL
names(AURO_Export_FDI)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 2
AURO2 <- PRB2 %>% filter(Region.Name == "Austin") %>% select(Numerator)
AURO2d <- allocations %>% filter(Region == "Austin") %>% select(Year)
t2 <- .35
AUROa2 <- AURO2/AURO2d
AUROpta2 <- AUROa2/t2

AURO_AM_INNOVATE <- data.frame(AURO2, AURO2d, t2, AUROa2, AUROpta2)
names(AURO_AM_INNOVATE)<-NULL
names(AURO_AM_INNOVATE)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 3
AURO3 <- PRB3 %>% filter(Region.Name == "Austin") %>% select(Numerator)
AURO3d <- allocations %>% filter(Region == "Austin") %>% select(Year)
t3 <- .07
AUROa3 <- AURO3/AURO3d
AUROpta3 <- AUROa3/t3

AURO_RESILIENCY <- data.frame(AURO3, AURO3d, t3, AUROa3, AUROpta3)
names(AURO_RESILIENCY)<-NULL
names(AURO_RESILIENCY)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 4
AURO4 <- PRB4 %>% filter(Region.Name == "Austin") %>% select(Numerator)
AURO4d <- allocations %>% filter(Region == "Austin") %>% select(Year)
t4 <- .9
AUROa4 <- AURO4/AURO4d
AUROpta4 <- AUROa4/t4

AURO_90pct <- data.frame(AURO4, AURO4d, t4, AUROa4, AUROpta4)
names(AURO_90pct)<-NULL
names(AURO_90pct)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 5
AURO5 <- PRB5 %>% filter(Region.Name == "Austin") %>% select(Numerator)
AURO5d <- allocations %>% filter(Region == "Austin") %>% select(Total)
t5 <- .9
AUROa5 <- AURO5/AURO5d
AUROpta5 <- AUROa5/t5

AURO_90pcttotal <- data.frame(AURO5, AURO5d, t5, AUROa5, AUROpta5)
names(AURO_90pcttotal)<-NULL
names(AURO_90pcttotal)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 6
AURO6 <- PRB6a %>% filter(Region.Name == "Austin") %>% select(Numerator)
AURO6d <- PRB6b %>% filter(Region.Name == "Austin") %>% select(Denominator)
t6 <- .7
AUROa6 <- AURO6/AURO6d
AUROpta6 <- AUROa6/t6

AURO_CEDS <- data.frame(AURO6, AURO6d, t6, AUROa6, AUROpta6)
names(AURO_CEDS)<-NULL
names(AURO_CEDS)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")


###################################################


#Chicago
#measure 1
CRO1 <- PRB1 %>% filter(Region.Name == "Chicago") %>% select(Numerator)
CRO1d <- allocations %>% filter(Region == "Chicago") %>% select(Year)
t1 <- .3
CROa1 <- CRO1/CRO1d
CROpta <- CROa1/t1

CRO_Export_FDI <- data.frame(CRO1, CRO1d, t1, CROa1, CROpta)
names(CRO_Export_FDI)<-NULL
names(CRO_Export_FDI)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 2
CRO2 <- PRB2 %>% filter(Region.Name == "Chicago") %>% select(Numerator)
CRO2d <- allocations %>% filter(Region == "Chicago") %>% select(Year)
t2 <- .35
CROa2 <- CRO2/CRO2d
CROpta2 <- CROa2/t2

CRO_AM_INNOVATE <- data.frame(CRO2, CRO2d, t2, CROa2, CROpta2)
names(CRO_AM_INNOVATE)<-NULL
names(CRO_AM_INNOVATE)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 3
CRO3 <- PRB3 %>% filter(Region.Name == "Chicago") %>% select(Numerator)
CRO3d <- allocations %>% filter(Region == "Chicago") %>% select(Year)
t3 <- .07
CROa3 <- CRO3/CRO3d
CROpta3 <- CROa3/t3

CRO_RESILIENCY <- data.frame(CRO3, CRO3d, t3, CROa3, CROpta3)
names(CRO_RESILIENCY)<-NULL
names(CRO_RESILIENCY)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 4
CRO4 <- PRB4 %>% filter(Region.Name == "Chicago") %>% select(Numerator)
CRO4d <- allocations %>% filter(Region == "Chicago") %>% select(Year)
t4 <- .9
CROa4 <- CRO4/CRO4d
CROpta4 <- CROa4/t4

CRO_90pct <- data.frame(CRO4, CRO4d, t4, CROa4, CROpta4)
names(CRO_90pct)<-NULL
names(CRO_90pct)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 5
CRO5 <- PRB5 %>% filter(Region.Name == "Chicago") %>% select(Numerator)
CRO5d <- allocations %>% filter(Region == "Chicago") %>% select(Total)
t5 <- .9
CROa5 <- CRO5/CRO5d
CROpta5 <- CROa5/t5

CRO_90pcttotal <- data.frame(CRO5, CRO5d, t5, CROa5, CROpta5)
names(CRO_90pcttotal)<-NULL
names(CRO_90pcttotal)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 6
CRO6 <- PRB6a %>% filter(Region.Name == "Chicago") %>% select(Numerator)
CRO6d <- PRB6b %>% filter(Region.Name == "Chicago") %>% select(Denominator)
t6 <- .7
CROa6 <- CRO6/CRO6d
CROpta6 <- CROa6/t6

CRO_CEDS <- data.frame(CRO6, CRO6d, t6, CROa6, CROpta6)
names(CRO_CEDS)<-NULL
names(CRO_CEDS)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")


######################################################################


#Denver
#measure 1
DRO1 <- PRB1 %>% filter(Region.Name == "Denver") %>% select(Numerator)
DRO1d <- allocations %>% filter(Region == "Denver") %>% select(Year)
t1 <- .3
DROa1 <- DRO1/DRO1d
DROpta <- DROa1/t1

DRO_Export_FDI <- data.frame(DRO1, DRO1d, t1, DROa1, DROpta)
names(DRO_Export_FDI)<-NULL
names(DRO_Export_FDI)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 2
DRO2 <- PRB2 %>% filter(Region.Name == "Denver") %>% select(Numerator)
DRO2d <- allocations %>% filter(Region == "Denver") %>% select(Year)
t2 <- .35
DROa2 <- DRO2/DRO2d
DROpta2 <- DROa2/t2

DRO_AM_INNOVATE <- data.frame(DRO2, DRO2d, t2, DROa2, DROpta2)
names(DRO_AM_INNOVATE)<-NULL
names(DRO_AM_INNOVATE)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 3
DRO3 <- PRB3 %>% filter(Region.Name == "Denver") %>% select(Numerator)
DRO3d <- allocations %>% filter(Region == "Denver") %>% select(Year)
t3 <- .07
DROa3 <- DRO3/DRO3d
DROpta3 <- DROa3/t3

DRO_RESILIENCY <- data.frame(DRO3, DRO3d, t3, DROa3, DROpta3)
names(DRO_RESILIENCY)<-NULL
names(DRO_RESILIENCY)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 4
DRO4 <- PRB4 %>% filter(Region.Name == "Denver") %>% select(Numerator)
DRO4d <- allocations %>% filter(Region == "Denver") %>% select(Year)
t4 <- .9
DROa4 <- DRO4/DRO4d
DROpta4 <- DROa4/t4

DRO_90pct <- data.frame(DRO4, DRO4d, t4, DROa4, DROpta4)
names(DRO_90pct)<-NULL
names(DRO_90pct)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 5
DRO5 <- PRB5 %>% filter(Region.Name == "Denver") %>% select(Numerator)
DRO5d <- allocations %>% filter(Region == "Denver") %>% select(Total)
t5 <- .9
DROa5 <- DRO5/DRO5d
DROpta5 <- DROa5/t5

DRO_90pcttotal <- data.frame(DRO5, DRO5d, t5, DROa5, DROpta5)
names(DRO_90pcttotal)<-NULL
names(DRO_90pcttotal)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 6
DRO6 <- PRB6a %>% filter(Region.Name == "Denver") %>% select(Numerator)
DRO6d <- PRB6b %>% filter(Region.Name == "Denver") %>% select(Denominator)
t6 <- .7
DROa6 <- DRO6/DRO6d
DROpta6 <- DROa6/t6

DRO_CEDS <- data.frame(DRO6, DRO6d, t6, DROa6, DROpta6)
names(DRO_CEDS)<-NULL
names(DRO_CEDS)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")


#################################################################


#Philadelphia
#measure 1
PRO1 <- PRB1 %>% filter(Region.Name == "Philadelphia") %>% select(Numerator)
PRO1d <- allocations %>% filter(Region == "Philadelphia") %>% select(Year)
t1 <- .3
PROa1 <- PRO1/PRO1d
PROpta <- PROa1/t1

PRO_Export_FDI <- data.frame(PRO1, PRO1d, t1, PROa1, PROpta)
names(PRO_Export_FDI)<-NULL
names(PRO_Export_FDI)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 2
PRO2 <- PRB2 %>% filter(Region.Name == "Philadelphia") %>% select(Numerator)
PRO2d <- allocations %>% filter(Region == "Philadelphia") %>% select(Year)
t2 <- .35
PROa2 <- PRO2/PRO2d
PROpta2 <- PROa2/t2

PRO_AM_INNOVATE <- data.frame(PRO2, PRO2d, t2, PROa2, PROpta2)
names(PRO_AM_INNOVATE)<-NULL
names(PRO_AM_INNOVATE)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 3
PRO3 <- PRB3 %>% filter(Region.Name == "Philadelphia") %>% select(Numerator)
PRO3d <- allocations %>% filter(Region == "Philadelphia") %>% select(Year)
t3 <- .07
PROa3 <- PRO3/PRO3d
PROpta3 <- PROa3/t3

PRO_RESILIENCY <- data.frame(PRO3, PRO3d, t3, PROa3, PROpta3)
names(PRO_RESILIENCY)<-NULL
names(PRO_RESILIENCY)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 4
PRO4 <- PRB4 %>% filter(Region.Name == "Philadelphia") %>% select(Numerator)
PRO4d <- allocations %>% filter(Region == "Philadelphia") %>% select(Year)
t4 <- .9
PROa4 <- PRO4/PRO4d
PROpta4 <- PROa4/t4

PRO_90pct <- data.frame(PRO4, PRO4d, t4, PROa4, PROpta4)
names(PRO_90pct)<-NULL
names(PRO_90pct)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 5
PRO5 <- PRB5 %>% filter(Region.Name == "Philadelphia") %>% select(Numerator)
PRO5d <- allocations %>% filter(Region == "Philadelphia") %>% select(Total)
t5 <- .9
PROa5 <- PRO5/PRO5d
PROpta5 <- PROa5/t5

PRO_90pcttotal <- data.frame(PRO5, PRO5d, t5, PROa5, PROpta5)
names(PRO_90pcttotal)<-NULL
names(PRO_90pcttotal)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 6
PRO6 <- PRB6a %>% filter(Region.Name == "Philadelphia") %>% select(Numerator)
PRO6d <- PRB6b %>% filter(Region.Name == "Philadelphia") %>% select(Denominator)
t6 <- .7
PROa6 <- PRO6/PRO6d
PROpta6 <- PROa6/t6

PRO_CEDS <- data.frame(PRO6, PRO6d, t6, PROa6, PROpta6)
names(PRO_CEDS)<-NULL
names(PRO_CEDS)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")


################################################################


#Seattle
#measure 1
SRO1 <- PRB1 %>% filter(Region.Name == "Seattle") %>% select(Numerator)
SRO1d <- allocations %>% filter(Region == "Seattle") %>% select(Year)
t1 <- .3
SROa1 <- SRO1/SRO1d
SROpta <- SROa1/t1

SRO_Export_FDI <- data.frame(SRO1, SRO1d, t1, SROa1, SROpta)
names(SRO_Export_FDI)<-NULL
names(SRO_Export_FDI)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 2
SRO2 <- PRB2 %>% filter(Region.Name == "Seattle") %>% select(Numerator)
SRO2d <- allocations %>% filter(Region == "Seattle") %>% select(Year)
t2 <- .35
SROa2 <- SRO2/SRO2d
SROpta2 <- SROa2/t2

SRO_AM_INNOVATE <- data.frame(SRO2, SRO2d, t2, SROa2, SROpta2)
names(SRO_AM_INNOVATE)<-NULL
names(SRO_AM_INNOVATE)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 3
SRO3 <- PRB3 %>% filter(Region.Name == "Seattle") %>% select(Numerator)
SRO3d <- allocations %>% filter(Region == "Seattle") %>% select(Year)
t3 <- .07
SROa3 <- SRO3/SRO3d
SROpta3 <- SROa3/t3

SRO_RESILIENCY <- data.frame(SRO3, SRO3d, t3, SROa3, SROpta3)
names(SRO_RESILIENCY)<-NULL
names(SRO_RESILIENCY)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 4
SRO4 <- PRB4 %>% filter(Region.Name == "Seattle") %>% select(Numerator)
SRO4d <- allocations %>% filter(Region == "Seattle") %>% select(Year)
t4 <- .9
SROa4 <- SRO4/SRO4d
SROpta4 <- SROa4/t4

SRO_90pct <- data.frame(SRO4, SRO4d, t4, SROa4, SROpta4)
names(SRO_90pct)<-NULL
names(SRO_90pct)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 5
SRO5 <- PRB5 %>% filter(Region.Name == "Seattle") %>% select(Numerator)
SRO5d <- allocations %>% filter(Region == "Seattle") %>% select(Total)
t5 <- .9
SROa5 <- SRO5/SRO5d
SROpta5 <- SROa5/t5

SRO_90pcttotal <- data.frame(SRO5, SRO5d, t5, SROa5, SROpta5)
names(SRO_90pcttotal)<-NULL
names(SRO_90pcttotal)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")

#measure 6
SRO6 <- PRB6a %>% filter(Region.Name == "Seattle") %>% select(Numerator)
SRO6d <- PRB6b %>% filter(Region.Name == "Seattle") %>% select(Denominator)
t6 <- .7
SROa6 <- SRO6/SRO6d
SROpta6 <- SROa6/t6

SRO_CEDS <- data.frame(SRO6, SRO6d, t6, SROa6, SROpta6)
names(SRO_CEDS)<-NULL
names(SRO_CEDS)<- c("Numerator", "Denominator", "Target", "Actual", "PctAchieved")


##########################################


# create report card table

measures <- data.frame("Performance_Requirement_Number"=c("1 - Exports & FDI", 
                                                             "2 - Adv. Manufacturing & Innovation",
                                                             "3 - Resiliency",
                                                             "4 - 90% EDAP Funds Obligated (current FY)",
                                                             "5 - 90% EDAP Funds Obligated (current FY + prior year carry-over allocations)",
                                                             "6 - 70% of CEDS Resilient"))

#All measures summary
ATRO_FINAL <- rbind(ATRO_Export_FDI, ATRO_AM_INNOVATE, ATRO_RESILIENCY, ATRO_90pct, ATRO_90pcttotal, ATRO_CEDS)
ATRO_FINAL <- cbind(measures, ATRO_FINAL)
ATRO_FINAL <- ATRO_FINAL %>% mutate(Numerator = dollar(Numerator), Denominator = dollar(Denominator),
                                     Target = percent(Target), Actual = percent(Actual), PctAchieved = percent(PctAchieved))
ATRO_FINAL$Numerator[6] <- str_replace_all(ATRO_FINAL$Numerator[6], "\\$|,", "")
ATRO_FINAL$Denominator[6] <- str_replace_all(ATRO_FINAL$Denominator[6], "\\$|,", "")
names(ATRO_FINAL) <- c("Performance Measure", "PW & EAA Funding Awarded", "PW & EAA Funding Available", "Target %", 
                      "Actual % Awarded", "% Target Achieved")

AURO_FINAL <- rbind(AURO_Export_FDI, AURO_AM_INNOVATE, AURO_RESILIENCY, AURO_90pct, AURO_90pcttotal, AURO_CEDS)
AURO_FINAL <- cbind(measures, AURO_FINAL)
AURO_FINAL <- AURO_FINAL %>% mutate(Numerator = dollar(Numerator), Denominator = dollar(Denominator),
                                    Target = percent(Target), Actual = percent(Actual), PctAchieved = percent(PctAchieved))
AURO_FINAL$Numerator[6] <- str_replace_all(AURO_FINAL$Numerator[6], "\\$|,", "")
AURO_FINAL$Denominator[6] <- str_replace_all(AURO_FINAL$Denominator[6], "\\$|,", "")
names(AURO_FINAL) <- c("Performance Measure", "PW & EAA Funding Awarded", "PW & EAA Funding Available", "Target %", 
                       "Actual % Awarded", "% Target Achieved")

CRO_FINAL <- rbind(CRO_Export_FDI, CRO_AM_INNOVATE, CRO_RESILIENCY, CRO_90pct, CRO_90pcttotal, CRO_CEDS)
CRO_FINAL <- cbind(measures, CRO_FINAL)
CRO_FINAL <- CRO_FINAL %>% mutate(Numerator = dollar(Numerator), Denominator = dollar(Denominator),
                                    Target = percent(Target), Actual = percent(Actual), PctAchieved = percent(PctAchieved))
CRO_FINAL$Numerator[6] <- str_replace_all(CRO_FINAL$Numerator[6], "\\$|,", "")
CRO_FINAL$Denominator[6] <- str_replace_all(CRO_FINAL$Denominator[6], "\\$|,", "")
names(CRO_FINAL) <- c("Performance Measure", "PW & EAA Funding Awarded", "PW & EAA Funding Available", "Target %", 
                      "Actual % Awarded", "% Target Achieved")

DRO_FINAL <- rbind(DRO_Export_FDI, DRO_AM_INNOVATE, DRO_RESILIENCY, DRO_90pct, DRO_90pcttotal, DRO_CEDS)
DRO_FINAL <- cbind(measures, DRO_FINAL)
DRO_FINAL <- DRO_FINAL %>% mutate(Numerator = dollar(Numerator), Denominator = dollar(Denominator),
                                    Target = percent(Target), Actual = percent(Actual), PctAchieved = percent(PctAchieved))
DRO_FINAL$Numerator[6] <- str_replace_all(DRO_FINAL$Numerator[6], "\\$|,", "")
DRO_FINAL$Denominator[6] <- str_replace_all(DRO_FINAL$Denominator[6], "\\$|,", "")
names(DRO_FINAL) <- c("Performance Measure", "PW & EAA Funding Awarded", "PW & EAA Funding Available", "Target %", 
                      "Actual % Awarded", "% Target Achieved")

PRO_FINAL <- rbind(PRO_Export_FDI, PRO_AM_INNOVATE, PRO_RESILIENCY, PRO_90pct, PRO_90pcttotal, PRO_CEDS)
PRO_FINAL <- cbind(measures, PRO_FINAL)
PRO_FINAL <- PRO_FINAL %>% mutate(Numerator = dollar(Numerator), Denominator = dollar(Denominator),
                                    Target = percent(Target), Actual = percent(Actual), PctAchieved = percent(PctAchieved))
PRO_FINAL$Numerator[6] <- str_replace_all(PRO_FINAL$Numerator[6], "\\$|,", "")
PRO_FINAL$Denominator[6] <- str_replace_all(PRO_FINAL$Denominator[6], "\\$|,", "")
names(PRO_FINAL) <- c("Performance Measure", "PW & EAA Funding Awarded", "PW & EAA Funding Available", "Target %", 
                      "Actual % Awarded", "% Target Achieved")

SRO_FINAL <- rbind(SRO_Export_FDI, SRO_AM_INNOVATE, SRO_RESILIENCY, SRO_90pct, SRO_90pcttotal, SRO_CEDS)
SRO_FINAL <- cbind(measures, SRO_FINAL)
SRO_FINAL <- SRO_FINAL %>% mutate(Numerator = dollar(Numerator), Denominator = dollar(Denominator),
                                    Target = percent(Target), Actual = percent(Actual), PctAchieved = percent(PctAchieved))
SRO_FINAL$Numerator[6] <- str_replace_all(SRO_FINAL$Numerator[6], "\\$|,", "")
SRO_FINAL$Denominator[6] <- str_replace_all(SRO_FINAL$Denominator[6], "\\$|,", "")
names(SRO_FINAL) <- c("Performance Measure", "PW & EAA Funding Awarded", "PW & EAA Funding Available", "Target %", 
                      "Actual % Awarded", "% Target Achieved")


output_directory <- str_c("G:/PNP/Performance Measurement/Performance Review Board PRB/FY ", year)
setwd(output_directory)
write_csv(ATRO_FINAL, "ATRO_FINAL.csv")
write_csv(AURO_FINAL, "AURO_FINAL.csv")
write_csv(CRO_FINAL, "CRO_FINAL.csv")
write_csv(DRO_FINAL, "DRO_FINAL.csv")
write_csv(PRO_FINAL, "PRO_FINAL.csv")
write_csv(SRO_FINAL, "SRO_FINAL.csv")
