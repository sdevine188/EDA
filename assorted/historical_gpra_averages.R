library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(scales)

options(scipen=999)

# load shiny app data
setwd("G:/PNP/Performance Measurement/rshinyapp/grants/data")
shiny_data_filename <- list.files()[str_detect(list.files(), "shiny_app_data_20")]
shiny <- read_csv(shiny_data_filename, col_types = list(DUNS = col_character(), Control.No. = col_character(), Project.No. = col_character(), Proj.ZIP = col_character(), 
                                                        Appl.ZIP = col_character(), Initiatives = col_character(), Coapp.Appl.ZIP.4 = col_character(), IRS = col_character(),
                                                        Coapp.DUNS = col_character(), Coapp.IRS = col_character()))
# setwd

# convert jobs/pi actuals to numeric
shiny$Jobs.Created.at.3.years <- as.numeric(shiny$Jobs.Created.at.3.years)
shiny$Jobs.Retained.at.3.years <- as.numeric(shiny$Jobs.Retained.at.3.years)
shiny$Priv.Inv.at.3.years <- as.numeric(shiny$Priv.Inv.at.3.years)

shiny$Jobs.Created.at.6.years <- as.numeric(shiny$Jobs.Created.at.6.years)
shiny$Jobs.Retained.at.6.years <- as.numeric(shiny$Jobs.Retained.at.6.years)
shiny$Priv.Inv.at.6.years <- as.numeric(shiny$Priv.Inv.at.6.years)

shiny$Jobs.Created.at.9.years <- as.numeric(shiny$Jobs.Created.at.9.years)
shiny$Jobs.Retained.at.9.years <- as.numeric(shiny$Jobs.Retained.at.9.years)
shiny$Priv.Inv.at.9.years <- as.numeric(shiny$Priv.Inv.at.9.years)


# need to combine jobs created/retained into jcr actuals
# but just summing them would create improper NA jcr values when either created or retained is NA and the other is a number
# so we assign a value of 0 instead of NA in cases where one is NA and the other is a number
na_check_3yr <- shiny %>% filter(Status == "Approved", FY > 1996, FY < 2017, Construction %in% c("C", "B"), 
                 is.na(Jobs.Created.at.3.years) & !is.na(Jobs.Retained.at.3.years) |
                         !is.na(Jobs.Created.at.3.years) & is.na(Jobs.Retained.at.3.years)) %>% 
        select(Control.No., EDA.Funding, Jobs.Created.at.9.years, Jobs.Retained.at.9.years)

na_check_6yr <- shiny %>% filter(Status == "Approved", FY > 1996, FY < 2017, Construction %in% c("C", "B"), 
                 is.na(Jobs.Created.at.6.years) & !is.na(Jobs.Retained.at.6.years) |
                         !is.na(Jobs.Created.at.6.years) & is.na(Jobs.Retained.at.6.years)) %>% 
        select(Control.No., EDA.Funding, Jobs.Created.at.6.years, Jobs.Retained.at.6.years)

na_check_9yr <- shiny %>% filter(Status == "Approved", FY > 1996, FY < 2017, Construction %in% c("C", "B"), 
                 is.na(Jobs.Created.at.9.years) & !is.na(Jobs.Retained.at.9.years) |
                         !is.na(Jobs.Created.at.9.years) & is.na(Jobs.Retained.at.9.years)) %>% 
        select(Control.No., EDA.Funding, Jobs.Created.at.9.years, Jobs.Retained.at.9.years)

# find historical average jobs_created_ratio, jobs_saved_ratio, pi_ratio
na_test_6yr <- shiny %>% filter(Status == "Approved", FY > 1996, FY < 2017, Construction %in% c("C", "B")) %>% 
        mutate(Jobs.Created.at.6.years = case_when(is.na(.$Jobs.Created.at.6.years) & 
                !is.na(.$Jobs.Retained.at.6.years) ~ 0, TRUE ~ .$Jobs.Created.at.6.years), 
               Jobs.Retained.at.6.years = case_when(is.na(.$Jobs.Retained.at.6.years) & 
                                 !is.na(.$Jobs.Created.at.6.years) ~ 0, TRUE ~ .$Jobs.Retained.at.6.years))
                
na_test_6yr %>% filter(Control.No. %in% na_check_6yr$Control.No.) %>% 
        select(Control.No., EDA.Funding, Jobs.Created.at.6.years, Jobs.Retained.at.6.years)


na_test_9yr <- shiny %>% filter(Status == "Approved", FY > 1996, FY < 2017, Construction %in% c("C", "B")) %>% 
                mutate(Jobs.Created.at.9.years = case_when(is.na(.$Jobs.Created.at.9.years) & 
                           !is.na(.$Jobs.Retained.at.9.years) ~ 0, TRUE ~ .$Jobs.Created.at.9.years), 
                Jobs.Retained.at.9.years = case_when(is.na(.$Jobs.Retained.at.9.years) & 
                            !is.na(.$Jobs.Created.at.9.years) ~ 0, TRUE ~ .$Jobs.Retained.at.9.years))

na_test_9yr %>% filter(Control.No. %in% na_check_9yr$Control.No.) %>% 
        select(Control.No., EDA.Funding, Jobs.Created.at.9.years, Jobs.Retained.at.9.years)

# sum jobs created/retained into jcr variable
data <- shiny %>% filter(Status == "Approved", FY > 1996, FY < 2017, Construction %in% c("C", "B")) %>% 
        mutate(Jobs.Created.at.6.years = case_when(is.na(.$Jobs.Created.at.6.years) & 
                !is.na(.$Jobs.Retained.at.6.years) ~ 0, TRUE ~ .$Jobs.Created.at.6.years), 
               Jobs.Retained.at.6.years = case_when(is.na(.$Jobs.Retained.at.6.years) & 
                !is.na(.$Jobs.Created.at.6.years) ~ 0, TRUE ~ .$Jobs.Retained.at.6.years),
               Jobs.Created.at.9.years = case_when(is.na(.$Jobs.Created.at.9.years) & 
                !is.na(.$Jobs.Retained.at.9.years) ~ 0, TRUE ~ .$Jobs.Created.at.9.years), 
               Jobs.Retained.at.9.years = case_when(is.na(.$Jobs.Retained.at.9.years) & 
                !is.na(.$Jobs.Created.at.9.years) ~ 0, TRUE ~ .$Jobs.Retained.at.9.years)) %>%
        mutate(jcr_3yr = Jobs.Created.at.3.years + Jobs.Retained.at.3.years,
        jcr_6yr = Jobs.Created.at.6.years + Jobs.Retained.at.6.years, 
        jcr_9yr = Jobs.Created.at.9.years + Jobs.Retained.at.9.years)

# inspect
data %>% filter(Control.No. %in% na_check_6yr$Control.No. | Control.No. %in% na_check_9yr$Control.No.) %>%
        select(Jobs.Created.at.3.years, Jobs.Retained.at.3.years, jcr_3yr, Jobs.Created.at.6.years, Jobs.Retained.at.6.years, 
                jcr_6yr, Jobs.Created.at.9.years, Jobs.Retained.at.9.years,
                jcr_9yr)


#################################################################


# summary stats for raw levels
data %>% summarize(avg_jcr_3yr = mean(jcr_3yr, na.rm = TRUE), sum_jcr_3yr = sum(jcr_3yr, na.rm = TRUE), na_jcr_3yr = sum(is.na(jcr_3yr)),
                   avg_jcr_6yr = mean(jcr_6yr, na.rm = TRUE), sum_jcr_6yr = sum(jcr_6yr, na.rm = TRUE), na_jcr_6yr = sum(is.na(jcr_6yr)),
                   avg_jcr_9yr = mean(jcr_9yr, na.rm = TRUE), sum_jcr_9yr = sum(jcr_9yr, na.rm = TRUE), na_jcr_9yr = sum(is.na(jcr_9yr)),
                   avg_pi_3yr = mean(as.numeric(Priv.Inv.at.3.years), na.rm = TRUE), sum_pi_3yr = sum(as.numeric(Priv.Inv.at.3.years), na.rm = TRUE), na_pi_3yr = sum(is.na(Priv.Inv.at.3.years)),
                   avg_pi_6yr = mean(as.numeric(Priv.Inv.at.6.years), na.rm = TRUE), sum_pi_6yr = sum(as.numeric(Priv.Inv.at.6.years), na.rm = TRUE), na_pi_6yr = sum(is.na(Priv.Inv.at.6.years)),
                   avg_pi_9yr = mean(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE), sum_pi_9yr = sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE), 
                   na_pi_9yr = sum(is.na(Priv.Inv.at.9.years))) %>% data.frame(.)


################################################


# plot jcr by fy
plot_fy_jcr <- data %>% group_by(FY) %>% summarize(avg_jcr_3yr = mean(jcr_3yr, na.rm = TRUE), 
                                                   avg_jcr_6yr = mean(jcr_6yr, na.rm = TRUE), 
                                                   avg_jcr_9yr = mean(jcr_9yr, na.rm = TRUE)) %>% 
        gather(gpra_yr, avg_jcr, avg_jcr_3yr : avg_jcr_9yr)

# replace NaN with NA
plot_fy_jcr$avg_jcr[is.nan(as.numeric(plot_fy_jcr$avg_jcr))] <- NA

# line chart
ggplot(plot_fy_jcr, aes(x = FY, y = avg_jcr, group = factor(gpra_yr), color = factor(gpra_yr))) + geom_line() + geom_point()

# violin plot
ggplot(plot_fy_jcr, aes(x = gpra_yr, y = avg_jcr)) + geom_violin(aes(fill = factor(gpra_yr))) + geom_boxplot(width = .2) + 
        ggtitle("Distribution of Jobs Created/Retained (GPRA Actuals), 2000-2016") +
        ylab("Average JCR") + xlab("GPRA Actuals") + scale_fill_brewer(palette = "Blues")



####################################################


# plot pi by fy
plot_fy_pi <- data %>% group_by(FY) %>% summarize(avg_pi_3yr = mean(Priv.Inv.at.3.years, na.rm = TRUE), 
                                                  avg_pi_6yr = mean(Priv.Inv.at.6.years, na.rm = TRUE), 
                                                  avg_pi_9yr = mean(Priv.Inv.at.9.years, na.rm = TRUE)) %>% gather(gpra_yr, avg_pi, avg_pi_3yr : avg_pi_9yr) %>% data.frame(.)

# replace NaN with NA
plot_fy_pi$avg_pi[is.nan(as.numeric(plot_fy_pi$avg_pi))] <- NA

# line chart
ggplot(plot_fy_pi, aes(x = FY, y = avg_pi, group = factor(gpra_yr), color = factor(gpra_yr))) + geom_line() + geom_point()

# violon plot
ggplot(plot_fy_pi, aes(x = gpra_yr, y = avg_pi)) + geom_violin(aes(fill = factor(gpra_yr))) + geom_boxplot(width = .2) + 
        ggtitle("Distribution of Private Investment (GPRA Actuals), 2000-2016") +
        ylab("Average JCR") + xlab("GPRA Actuals") + scale_fill_brewer(palette = "Blues")


#########################################


# jcr and pi ratios
# calculate jcr and pi ratios

# need to convert jcr = 0 to jcr = 1 so that funding/jcr is not infinity
jcr_ratio_3yr <- rep(NA, nrow(data))
for(i in 1:nrow(data)) {
        data_placeholder <- data
        if(!is.na(data_placeholder$jcr_3yr[i])) {
                if(data_placeholder$jcr_3yr[i] == 0) {
                        data_placeholder$jcr_3yr[i] <- 1
                } 
                
                if(data_placeholder$jcr_3yr[i] != 0 && !is.na(data_placeholder$jcr_3yr[i])) {
                        jcr_ratio_3yr[i] <- data_placeholder$EDA.Funding[i] / data_placeholder$jcr_3yr[i]
                }
        }
}

jcr_ratio_6yr <- rep(NA, nrow(data))
for(i in 1:nrow(data)) {
        data_placeholder <- data
        if(!is.na(data_placeholder$jcr_6yr[i])) {
                if(data_placeholder$jcr_6yr[i] == 0) {
                        data_placeholder$jcr_6yr[i] <- 1
                } 
                
                if(data_placeholder$jcr_6yr[i] != 0 && !is.na(data_placeholder$jcr_6yr[i])) {
                        jcr_ratio_6yr[i] <- data_placeholder$EDA.Funding[i] / data_placeholder$jcr_6yr[i]
                }
        }
}

jcr_ratio_9yr <- rep(NA, nrow(data))
for(i in 1:nrow(data)) {
        data_placeholder <- data
        if(!is.na(data_placeholder$jcr_9yr[i])) {
                if(data_placeholder$jcr_9yr[i] == 0) {
                        data_placeholder$jcr_9yr[i] <- 1
                } 
                
                if(data_placeholder$jcr_9yr[i] != 0 && !is.na(data_placeholder$jcr_9yr[i])) {
                        jcr_ratio_9yr[i] <- data_placeholder$EDA.Funding[i] / data_placeholder$jcr_9yr[i]
                }
        }
}

# pi ratios
pi_ratio_3yr <- rep(NA, nrow(data))
for(i in 1:nrow(data)) {
        data_placeholder <- data
        if(!is.na(data_placeholder$Priv.Inv.at.3.years[i])) {
                if(data_placeholder$Priv.Inv.at.3.years[i] == 0) {
                        pi_ratio_3yr[i] <- 0
                } 
                
                if(data_placeholder$Priv.Inv.at.3.years[i] != 0 && !is.na(data_placeholder$Priv.Inv.at.3.years[i])) {
                        pi_ratio_3yr[i] <- data_placeholder$Priv.Inv.at.3.years[i] / data_placeholder$EDA.Funding[i]
                }
        }
}

pi_ratio_6yr <- rep(NA, nrow(data))
for(i in 1:nrow(data)) {
        data_placeholder <- data
        if(!is.na(data_placeholder$Priv.Inv.at.6.years[i])) {
                if(data_placeholder$Priv.Inv.at.6.years[i] == 0) {
                        pi_ratio_6yr[i] <- 0
                } 
                
                if(data_placeholder$Priv.Inv.at.6.years[i] != 0 && !is.na(data_placeholder$Priv.Inv.at.6.years[i])) {
                        pi_ratio_6yr[i] <- data_placeholder$Priv.Inv.at.6.years[i] / data_placeholder$EDA.Funding[i]
                }
        }
}

pi_ratio_9yr <- rep(NA, nrow(data))
for(i in 1:nrow(data)) {
        data_placeholder <- data
        if(!is.na(data_placeholder$Priv.Inv.at.9.years[i])) {
                if(data_placeholder$Priv.Inv.at.9.years[i] == 0) {
                        pi_ratio_9yr[i] <- 0
                } 
                
                if(data_placeholder$Priv.Inv.at.9.years[i] != 0 && !is.na(data_placeholder$Priv.Inv.at.9.years[i])) {
                        pi_ratio_9yr[i] <- data_placeholder$Priv.Inv.at.9.years[i] / data_placeholder$EDA.Funding[i]
                }
        }
}

# combine data

data2 <- cbind(data, jcr_ratio_3yr, jcr_ratio_6yr, jcr_ratio_9yr, pi_ratio_3yr, pi_ratio_6yr, pi_ratio_9yr)


########################################################33


# jcr

# plot jcr ratio
ggplot(data2, aes(x = jcr_ratio_3yr)) + geom_histogram() + scale_x_continuous(labels = dollar)
ggplot(data2, aes(x = jcr_ratio_6yr)) + geom_histogram() + scale_x_continuous(labels = dollar)
ggplot(data2, aes(x = jcr_ratio_9yr)) + geom_histogram() + scale_x_continuous(labels = dollar)
data2 %>% filter(jcr_ratio_9yr < 100000) %>% ggplot(., aes(x = jcr_ratio_9yr)) + geom_histogram(binwidth = 1000) + 
        scale_x_continuous(labels = dollar, breaks = seq(0, 100000, 10000))

# discounted jcr and conservatively removing outliers (need to inflation adjust when have more time)
# 9 yr avg jcr is 13,550 (updated from 13,400) - this number was reported for fact sheet
data2 %>% filter(FY < 2014, FY > 1999, jcr_ratio_3yr > 2500) %>% summarize(sum_jcr_3yr = sum(jcr_3yr, na.rm = TRUE), dis_sum_jcr_3yr = sum_jcr_3yr * .75, funding = sum(EDA.Funding, na.rm = TRUE), jcr_ratio_3yr = funding / dis_sum_jcr_3yr)
data2 %>% filter(FY < 2011, FY > 1999, jcr_ratio_6yr > 2500) %>% summarize(sum_jcr_6yr = sum(jcr_6yr, na.rm = TRUE), dis_sum_jcr_6yr = sum_jcr_6yr * .75, funding = sum(EDA.Funding, na.rm = TRUE), jcr_ratio_6yr = funding / dis_sum_jcr_6yr)
data2 %>% filter(FY < 2008, FY > 1999, jcr_ratio_9yr > 2500) %>% summarize(sum_jcr_9yr = sum(jcr_9yr, na.rm = TRUE), dis_sum_jcr_9yr = sum_jcr_9yr * .75, funding = sum(EDA.Funding, na.rm = TRUE), jcr_ratio_9yr = funding / dis_sum_jcr_9yr)

# jcr by FY just as a check
data2 %>% group_by(FY) %>% filter(FY < 2008, FY > 1999, jcr_ratio_9yr > 2500) %>% 
        summarize(sum_jcr_9yr = sum(jcr_9yr, na.rm = TRUE), funding = sum(EDA.Funding, na.rm = TRUE), 
                  dis_sum_jcr_9yr = sum_jcr_9yr * .75, 
                  jcr_ratio_9yr = funding / dis_sum_jcr_9yr) %>% 
        summarize(avg_jcr_ratio_9yr = mean(jcr_ratio_9yr))


##############################################33


# pi

# plot pi ratio
ggplot(data2, aes(x = pi_ratio_3yr)) + geom_histogram()
ggplot(data2, aes(x = pi_ratio_6yr)) + geom_histogram()
ggplot(data2, aes(x = pi_ratio_9yr)) + geom_histogram()
data2 %>% filter(pi_ratio_9yr < 400) %>% ggplot(., aes(x = pi_ratio_9yr)) + geom_histogram(binwidth = 1)

# discounted pi and conservatively removing outliers
# 9 yr avg PI ratio is $15 - this number was reported for fact_sheet
data2 %>% filter(FY < 2008, FY > 1999, pi_ratio_3yr < 400) %>% 
        summarize(sum_pi_3yr = sum(as.numeric(Priv.Inv.at.3.years), na.rm = TRUE), dis_sum_pi_3yr = sum_pi_3yr * .75, funding = sum(EDA.Funding, na.rm = TRUE), pi_ratio_3yr = dis_sum_pi_3yr / funding)
data2 %>% filter(FY < 2008, FY > 1999, pi_ratio_6yr < 400) %>% 
        summarize(sum_pi_6yr = sum(as.numeric(Priv.Inv.at.6.years), na.rm = TRUE), dis_sum_pi_6yr = sum_pi_6yr * .75, funding = sum(EDA.Funding, na.rm = TRUE), pi_ratio_6yr = dis_sum_pi_6yr / funding)
data2 %>% filter(FY < 2008, FY > 1999, pi_ratio_9yr < 400) %>% 
        summarize(sum_pi_9yr = sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE), dis_sum_pi_9yr = sum_pi_9yr * .75, funding = sum(EDA.Funding, na.rm = TRUE), pi_ratio_9yr = dis_sum_pi_9yr / funding)

# pi by FY just as a check
data2 %>% group_by(FY) %>% filter(FY < 2008, FY > 1999, pi_ratio_9yr < 400) %>% 
        summarize(sum_pi_9yr = sum(as.numeric(Priv.Inv.at.9.years), na.rm = TRUE), funding = sum(EDA.Funding, na.rm = TRUE), 
                  dis_sum_pi_9yr = sum_pi_9yr * .75, 
                  pi_ratio_9yr = dis_sum_pi_9yr / funding) %>% 
        summarize(avg_pi_ratio_9yr = mean(pi_ratio_9yr))



