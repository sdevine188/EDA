library(dplyr)
library(stringr)
library(readr)
library(scales)

# create output file
wt_final <- wt_keywords_all_subset %>% select(Project.No., FY, Status, Full.Applicant.Name, EDA.Funding, Program, 
                                              Appr.Desc, Project.Short.Descrip, GNS.Descr.,
                                              Est.Jobs.Created, Est.Jobs.Saved, Est.Private.Investment,
                                              Appl.City.Name, Appl.County.Name, Appl.Cong.Dist, Appl.State.Abbr,  
                                              Proj.City.Name, Proj.County.Name, Proj.Cong.Dist, Proj.State.Abbr) %>% arrange(FY)


# add totals row
Row_ID <- data.frame(Row_ID = seq(nrow(wt_final)))
wt_final <- bind_cols(Row_ID, wt_final)
wt_final[nrow(wt_final) + 1, ] <- NA
for(i in 1:ncol(wt_final)){
        if(class(wt_final[ , i])[1] == "POSIXct") { 
                wt_final[ , i] <- as.character(wt_final[ , i]) 
        } else { if(class(wt_final[ , i]) == "numeric" | class(wt_final[ , i]) == "integer") { 
                wt_final[ nrow(wt_final), i] <- sum(wt_final[ , i], na.rm = TRUE) 
        } else { 
                wt_final[ nrow(wt_final), i] <- NA 
        }
        }
}
if("FY" %in% names(wt_final)) {
        wt_final[nrow(wt_final), which(names(wt_final) == "FY")] <- "NA" 
}
wt_final[nrow(wt_final), 1] <- "Totals"


wt_final <- wt_final %>% mutate(EDA.Funding = dollar(EDA.Funding), 
                                Est.Private.Investment = dollar(Est.Private.Investment))