library(rvest)
library(XML)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(lazyeval)

# setwd
# setwd("H:/PNP/SRI Baseline Metrics")
# setwd("C:/Users/sdevine/Desktop/SRI Baseline Metrics")
setwd("G:/PNP/Performance Measurement/SRI Baseline Metrics")

# scrape county fips from census and save as txt file
fips_html <- html("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt")
fips_html %>% html_text() %>% write_file(., "fips_html.txt")

# read in txt as dataframe
fips <- read_tsv("fips_html.txt", col_names = FALSE)
names(fips) <- "all_data"

# split comma-seperated variables into their own columns
fips <- fips %>% mutate(split_data = str_split(as.character(all_data), ",")) %>% unnest(split_data)
fips$col_names <- c("state", "fips_state", "fips_county", "county", "unknown")
fips <- fips %>% spread(key = col_names, value = split_data) %>% select(-all_data)
head(fips, 10)

# remove non-states
fips <- filter(fips, !(state %in% c("AS", "GU", "MP", "PR", "UM", "VI")))
dim(fips)

# create fips_state_county
fips$fips_state_county <- str_c(fips$fips_state, fips$fips_county)

# note the script will put NA values for following counties:
# remove kalawao county fips_state_county 15005 because bls reports "Kalawao & Maui" under fips_state_county 15009 - see downloaded bls qcew files
# remove bedford city fips_state_county 51515 because it was discontinued starting in 2014, though is available from api and downloads for prior years



####################################################


# get qcew data from bls api
# pre-built bls api function: https://data.bls.gov/cew/doc/access/data_access_examples.htm#RSCRIPT
# data guide: https://www.bls.gov/cew/doctoc.htm
# data csv: https://www.bls.gov/cew/datatoc.htm
# data tables for reference: https://data.bls.gov/cew/apps/data_views/data_views.htm#tab=Tables
# layout of data slices: https://data.bls.gov/cew/doc/access/csv_data_slices.htm
# can use bls multi-screen tool for multiple year downloads, but it chokes if you select even half the counties: https://data.bls.gov/cgi-bin/dsrv?en

# api seems to only go back to 2012; csv files for earlier are here: https://www.bls.gov/cew/datatoc.htm
# save unzipped quarterly csv by area; be sure to delete MSA files listed after county files to improve speed

# setwd()
# setwd("C:/Users/sdevine/Desktop/SRI Baseline Metrics/qcew")
setwd("G:/PNP/Performance Measurement/SRI Baseline Metrics/qcew")

# define year ranges (2012 is earliest year available)
year_start <- 2014
year_end <- 2015
year_range <- seq(year_start, year_end, by = 1)

# create variables list
variable_list <- c("employment", "employment_growth_5yr_prior", "establishments", "establishments_growth_5yr_prior", "wage")

# placeholder for fips
# fips_full <- fips
# fips <- fips_full

# fips_small <- fips[1:5, ]
# fips <- fips_small

# which(fips$state == "VA")
# fips <- fips[2806:nrow(fips), ]
# fips %>% filter(grepl("bedford city", county, ignore.case = TRUE))
# county_data <- qcewGetAreaData(2014, 1, 51515)


for(year in year_range) {
        
        # create placeholder column for variables
        for(v in 1:length(variable_list)) {
                column_name <- str_c(variable_list[v], "_", year)
                value <- NA
                mutate_criteria <- setNames(list(interp(~y, y = value)), nm = eval(column_name))
                fips <- fips %>% mutate_(.dots = mutate_criteria)
        }
        
        for(i in 1:nrow(fips)) {
                
                # print current county
                county_state <- str_c(fips$county[i], ", ", fips$state[i])
                print(str_c(county_state, "; year: ", year))
                
                # skip counties not in bls data - see above for notes
                if(fips$fips_state_county[i] %in% c("15005", "51515")) {
                        next
                }
                
                # pull current year county data from csv 
                folder_name <- str_c(year, ".annual.by_area")
                file_name_beginning <- str_c(year, ".annual ", fips$fips_state_county[i])
                filename <- list.files(folder_name)[str_detect(list.files(folder_name), file_name_beginning)]
                filename <- str_c(folder_name, "/", filename)
                county_data <- suppressMessages(read_csv(filename))
                
                # get annual avg employment level for all ownership types/all industries
                employment <- county_data %>% filter(own_code == 0, industry_code == 10) %>% rowwise() %>%
                        select(area_fips, annual_avg_emplvl)
                employment_column_name <- str_c("employment_", year)
                fips[ i, employment_column_name] <- employment$annual_avg_emplvl
                
                # get annual avg establishment count for all ownership types/all industries
                establishments <- county_data %>% filter(own_code == 0, industry_code == 10) %>% 
                        select(area_fips, annual_avg_estabs_count)
                establishments_column_name <- str_c("establishments_", year)
                fips[ i, establishments_column_name] <- establishments$annual_avg_estabs_count
                
                # get annual avg wage
                wage <- county_data %>% filter(own_code == 0, industry_code == 10) %>% 
                        select(area_fips, annual_avg_wkly_wage)
                wage_column_name <- str_c("wage_", year)
                fips[ i, wage_column_name] <- wage$annual_avg_wkly_wage
                
                
                ##########################################
                
                
                # pull county growth data 5yr prior from csv 
                folder_name <- str_c((year - 5), ".annual.by_area")
                file_name_beginning <- str_c((year - 5), ".annual ", fips$fips_state_county[i])
                filename <- list.files(folder_name)[str_detect(list.files(folder_name), file_name_beginning)]
                filename <- str_c(folder_name, "/", filename)
                county_data <- suppressMessages(read_csv(filename))
                
                # get levels from 5 years ago
                employment_5yr_prior <- county_data %>% filter(own_code == 0, industry_code == 10) %>% 
                        select(area_fips, annual_avg_emplvl)
                
                establishments_5yr_prior <- county_data %>% filter(own_code == 0, industry_code == 10) %>% 
                        select(area_fips, annual_avg_estabs_count)
                
                # calculate growth from 5 year prior
                employment_growth_5yr_prior <- ((employment$annual_avg_emplvl - employment_5yr_prior$annual_avg_emplvl) / employment_5yr_prior$annual_avg_emplvl) * 100
                establishments_growth_5yr_prior <- ((establishments$annual_avg_estabs_count - establishments_5yr_prior$annual_avg_estabs_count) / establishments_5yr_prior$annual_avg_estabs_count) * 100
                
                # add variables to fips dataframe
                employment_growth_column_name <- str_c("employment_growth_5yr_prior_", year)
                fips[ i, employment_growth_column_name] <- employment_growth_5yr_prior
                
                establishments_growth_column_name <- str_c("establishments_growth_5yr_prior_", year)
                fips[ i, establishments_growth_column_name] <- establishments_growth_5yr_prior
        }
}


# should only be two counties with NA values
na_sum <- function(variable) {
        return(sum(is.na(variable)))
}
max_na <- fips %>% summarize_all(funs(na_sum)) %>% rowwise() %>% summarize(max_na = max(.)) %>% .$max_na
if(max_na == 2) {
        print("Good: all NA values are known exceptions")
} else {
       print("Error: all NA values are not known exceptions") 
}

# write to file
# create csv filename for merged data
date1 <- as.character(Sys.Date())
date2 <- str_replace_all(date1, "-", "")
merged_filename <- str_c("qcew_", date2, ".csv")
write_csv(fips, path = merged_filename)


