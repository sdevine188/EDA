library(dplyr)
library(stringr)
library(reshape2)
library(ggplot2)
library(scales)

setwd("G:/PNP/Performance Measurement/master_data")
datafilename <- list.files()[str_detect(list.files(), "master_data_20")]

md <- read.csv(datafilename, stringsAsFactors = FALSE) 

# angie's datacall for how much spent on infrastructure over last x years; what percentage of EDAP funding spent on infrastructure?

# query two ways, one just based on Cons.Non = C | B, which is all "infrastructure" projects, but will include Disaster funding
inf <- filter(md, Cons.Non == "C" | Cons.Non == "B", FY > 2000, FY < 2016, Status == "Approved")

# query for Cons.Non = C | B, and Appr.Code == 20 | 70 for pure PW and EAA (minus some odd-ball PW appropriations, and EAA disasters)
inf2 <- filter(md, Cons.Non == "C" | Cons.Non == "B", FY > 2000, FY < 2016, Appr.Code == 20 | Appr.Code == 70, Status == "Approved")

# all EDAP
edap <- filter(md, FY > 2000, FY < 2016, Appr.Code %in% c(11, 20, 40, 41, 43, 47, 51, 
                52, 66, 70, 89), Status == "Approved")

# all EDAP plus supplementals
edap_supp <- filter(md, FY > 2000, FY < 2016, Status == "Approved") 

# just pw and eaa as denominator
pw_eaa <- filter(md, FY > 2000, FY < 2016, database == "opcs", Status == "Approved", Appr.Code %in% c(20, 70))

# plot graphs of inf funding over time
inf_sum <- inf %>%
        group_by(FY) %>%
        summarize(
                inf_funding = sum(Best.EDA..)
                )

inf_bar <- ggplot(data = inf_sum, aes(x = as.factor(FY), y = inf_funding)) + geom_bar(color = "black", stat = "identity", fill = "blue") + 
        scale_y_continuous(name = "Construction Investments", labels = dollar, breaks = seq(0, 500000000, 100000000), limits = c(0, 500000000)) + xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments (Includes Supplemental Funding)") + theme(plot.title = element_text(lineheight = .8, face = "bold"))

inf_bar <- ggplot(data = inf_sum, aes(x = as.factor(FY), y = inf_funding)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Construction Investments (Incl. Supplementals)", labels = dollar) + xlab("Fiscal Year") +
        ggtitle("EDA Public Works Appropriations") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("$%.0fm", inf_sum$inf_funding / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)

# sent to angie: plot graphs of inf2 funding over time
inf2_sum <- inf2 %>%
        group_by(FY) %>%
        summarize(
                inf2_funding = sum(Best.EDA..)
        )

inf2_bar <- ggplot(data = inf2_sum, aes(x = as.factor(FY), y = inf2_funding)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Construction $ (excl. Supp.)", labels = dollar, limits = c(0, 310000000)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments (Excl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("$%.0fm", inf2_sum$inf2_funding / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)

# sent to angie: same inf2_bar for fy 2009 - fy 2015
inf2_sum_short <- filter(inf2_sum, FY > 2008)

inf2_short_bar <- ggplot(data = inf2_sum_short, aes(x = as.factor(FY), y = inf2_funding)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Construction $ (excl. Supp.)", labels = dollar, limits = c(0, 175000000)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments (Excl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("$%.0fm", inf2_sum_short$inf2_funding / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)


# sent to angie: plot inf2 as percentage of edap (this excludes supplemental funding) 
edap_sum <- edap %>%
        group_by(FY) %>%
        summarize(
                edap_funding = sum(Best.EDA..)
        )

inf2_pct_edap <- inf2_sum
inf2_pct_edap <- cbind(inf2_pct_edap, edap_sum[ , 2])
inf2_pct_edap$pct <- inf2_pct_edap$inf2_funding / inf2_pct_edap$edap_funding

inf2_pct_edap_bar <- ggplot(data = inf2_pct_edap, aes(x = as.factor(FY), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Constr. $ as % of EDAP $ (excl. Supp.)", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments as Percentage of EDAP Investments (Excl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", inf2_pct_edap$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)

# sent to angie: same graph for fy 2009 - fy 2015
inf2_pct_edap_short <- filter(inf2_pct_edap, FY > 2008)

inf2_pct_edap_short_bar <- ggplot(data = inf2_pct_edap_short, aes(x = as.factor(FY), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Constr. $ as % of EDAP $ (excl. Supp.)", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments as Percentage of EDAP Investments (Excl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", inf2_pct_edap_short$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)


# plot inf as percentage of edap_supp (this includes supplemental funding) 
edap_supp_sum <- edap_supp %>%
        group_by(FY) %>%
        summarize(
                edap_supp_funding = sum(Best.EDA..)
        )

inf_pct_edap_supp <- inf_sum
inf_pct_edap_supp <- cbind(inf_pct_edap_supp, edap_supp_sum[ , 2])
inf_pct_edap_supp$pct <- inf_pct_edap_supp$inf_funding / inf_pct_edap_supp$edap_supp_funding

inf_pct_edap_supp_bar <- ggplot(data = inf_pct_edap_supp, aes(x = as.factor(FY), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Construction $ as % of EDAP $ (Incl. Supplementals)", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments as Percentage of All EDAP Investments (Incl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", inf_pct_edap_supp$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)

# same graph, for just fy 2009 - fy 2015 (plot inf2 as percentage of edap (incl supp))
inf_pct_edap_supp_short <- filter(inf_pct_edap_supp, FY > 2008)

inf_pct_edap_supp_short_bar <- ggplot(data = inf_pct_edap_supp_short, aes(x = as.factor(FY), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Construction $ as % of EDAP $ (Incl. Supplementals)", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments as Percentage of All EDAP Investments (Incl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", inf_pct_edap_supp_short$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)

# plot pw approp as percentage of all edap obligations (incl supp)
pw_approp_pct_edap_supp <- pw_approp
pw_approp_pct_edap_supp <- cbind(pw_approp_pct_edap_supp, edap_supp_sum[ , 2])
pw_approp_pct_edap_supp$pct <- pw_approp_pct_edap_supp$pw_funding / pw_approp_pct_edap_supp$edap_supp_funding

pw_approp_pct_edap_supp_bar <- ggplot(data = pw_approp_pct_edap_supp, aes(x = as.factor(fy), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "PW Appropriation $ as % of EDAP Obligation $ (Incl. Supplementals)", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("PW Appropriations as Percentage of All EDAP Obligations (Incl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", pw_approp_pct_edap_supp$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)

# sent to angie: plot pw approp as pct of edap approp
edap_approp <- read.csv("edap_approp.csv", stringsAsFactors = FALSE)

pw_approp_pct_edap_approp <- pw_approp
pw_approp_pct_edap_approp <- cbind(pw_approp_pct_edap_approp, edap_approp[ , 2])
names(pw_approp_pct_edap_approp)[3] <- "edap_funding"
pw_approp_pct_edap_approp$edap_funding <- pw_approp_pct_edap_approp$edap_funding * 1000
pw_approp_pct_edap_approp$pct <- pw_approp_pct_edap_approp$pw_funding / pw_approp_pct_edap_approp$edap_funding

pw_approp_pct_edap_approp_bar <- ggplot(data = pw_approp_pct_edap_approp, aes(x = as.factor(fy), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "PW Approp. $ as % of EDAP Approp. $", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Public Works Appropriations as % of EDAP Appropriations") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", pw_approp_pct_edap_approp$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)


# sent to angie: for just fy 2009 - fy 2015
pw_approp_pct_edap_approp_short <- filter(pw_approp_pct_edap_approp, fy > 2008)

pw_approp_pct_edap_approp__short_bar <- ggplot(data = pw_approp_pct_edap_approp_short, aes(x = as.factor(fy), y = pct)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "PW Approp. $ as % of EDAP Approp. $", labels = percent, limits = c(0, 1)) + 
        xlab("Fiscal Year") +
        ggtitle("EDA Public Works Appropriations as % of EDAP Appropriations") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("%.0f%%", pw_approp_pct_edap_approp_short$pct * 100)), position = position_dodge(width = 0.9), vjust = -0.25)


# plot inf2 as percentage of just pw_eaa (excludes most non-construction programs, as well as supplemental)
pw_eaa_sum <- pw_eaa %>%
        group_by(FY) %>%
        summarize(
                pw_eaa_funding = sum(Best.EDA..)
        )

inf2_pct_pw_eaa <- inf2_sum
inf2_pct_pw_eaa <- cbind(inf2_pct_pw_eaa, pw_eaa_sum[ , 2])
inf2_pct_pw_eaa$pct <- inf2_pct_pw_eaa$inf2_funding / inf2_pct_pw_eaa$pw_eaa_funding

inf2_pct_pw_eaa_bar <- ggplot(data = inf2_pct_pw_eaa, aes(x = as.factor(FY), y = pct)) + geom_bar(color = "black", stat = "identity", fill = "blue") + 
        scale_y_continuous(name = "Construction $ / PW & EAA $ (excl. Supplementals)", labels = percent, limits = c(0, 1)) + xlab("Fiscal Year") +
        ggtitle("EDA Construction Investments as Percentage of PW and EAA (Excl. Supplementals)") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + geom_text(aes(label = sprintf("%.0f%%", inf2_pct_pw_eaa$pct * 100)), 
                                        position = position_dodge(width=0.9), vjust = -0.25)

# plot just pw
pw <- filter(md, FY > 2000, FY < 2016, database == "opcs", Status == "Approved", Appr.Code == 20)

pw_sum <- pw %>%
        group_by(FY) %>%
        summarize(
                pw_funding = sum(Best.EDA..)

pw_bar <- ggplot(data = pw_sum, aes(x = as.factor(FY), y = pw_funding)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "PW Construction Investments", labels = dollar) + xlab("Fiscal Year") +
        ggtitle("EDA PW Construction Investments") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("$%.0fm", pw_sum$pw_funding / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)

# just pw appropriations
setwd("G:/PNP/Performance Measurement/Data Calls/Construction/20151222 - Infrastructure")
pw_approp <- read.csv("pw_approp.csv", stringsAsFactors = FALSE)
pw_approp$pw_funding <- pw_approp$pw_funding * 1000

# sent to angie: fy 2001 - fy 2015
pw_approp_bar <- ggplot(data = pw_approp, aes(x = as.factor(fy), y = pw_funding)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Public Works Appropriations", labels = dollar, limits = c(0, 310000000)) + xlab("Fiscal Year") +
        ggtitle("EDA Public Works Appropriations") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("$%.0fm", pw_approp$pw_funding / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)

# sent to angie: fy 2009 - fy 2015
pw_approp_short <- filter(pw_approp, fy > 2008)
pw_approp_bar_short <- ggplot(data = pw_approp_short, aes(x = as.factor(fy), y = pw_funding)) + 
        geom_bar(color = "black", stat = "identity", fill = "blue", width = .85, position = position_dodge(width = 1.7)) + 
        scale_y_continuous(name = "Public Works Appropriations", labels = dollar, limits = c(0, 150000000)) + xlab("Fiscal Year") +
        ggtitle("EDA Public Works Appropriations") + 
        theme(plot.title = element_text(lineheight = .8, face = "bold")) + 
        geom_text(aes(label = sprintf("$%.0fm", pw_approp_short$pw_funding / 1000000)), position = position_dodge(width = 0.9), vjust = -0.25)


# this doesn't plot nicely for some reason??
# g <- ggplot(data = inf, aes(x = as.factor(FY), y = Best.EDA..)) + geom_bar(color = "black", stat = "identity") 

# angie follow-up question for percentage of eda funds given to rural areas in last 5 years
all <- filter(md, FY > 2010, FY < 2016, Status == "Approved")
all_sum <- all %>%
        summarize(
                num_awards = n(),
                pi = sum(Private.Investment, na.rm = TRUE),
                jcr = sum(Jobs.Created, na.rm = TRUE) + sum(Jobs.Saved, na.rm = TRUE)
        )


rural <- filter(md, FY > 2010, FY < 2016, Status == "Approved", grepl("rural", Entity.Code, ignore.case = TRUE))
rural_sum <- rural %>%
        summarize(
                num_awards = n(),
                pi = sum(Private.Investment, na.rm = TRUE),
                jcr = sum(Jobs.Created, na.rm = TRUE) + sum(Jobs.Saved, na.rm = TRUE)
                )
rural_count_pct <- nrow(rural) / nrow(all)
rural_funding_pct <- sum(rural$Best.EDA..) / sum(all$Best.EDA..)

library(gridExtra)
grid.table(rural_sum, rows = NULL)

# to save the plot
pdf("test_pdf.pdf", height = 6, width = 6)
grid.table(rural_sum, rows = NULL)
dev.off()

