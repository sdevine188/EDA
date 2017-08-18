library(rvest)
library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(lazyeval)
library(RCurl)

# setwd
# setwd("H:/PNP/SRI Baseline Metrics")
# setwd("C:/Users/Stephen/Desktop/R/EDA/sri_metrics")
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

#######################################


# load bls lau county data from flat files
# measures: https://download.bls.gov/pub/time.series/la/la.measure
# measures: 3 - unemployment rate, 4 - unemployment, 5 - employment, 6 - labor force
# period: month 13 is annual average: https://download.bls.gov/pub/time.series/la/la.period
# see all flat LAU flat files: https://download.bls.gov/pub/time.series/la

bls <- getURL("https://download.bls.gov/pub/time.series/la/la.data.64.County")
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
bls_filename <- str_c("lau/bls_", date, ".csv")
write(bls, file = bls_filename)
bls <- read.csv("lau/bls_20170817.csv")
unemp_bls <- bls

# clean
unemp_bls <- mutate(unemp_bls, fips_state_county = str_sub(unemp_bls[ , 1], start = 6, end = 10))
unemp_bls <- mutate(unemp_bls, measure = str_sub(unemp_bls[ , 1], start = 20, end = 20))
unemp_bls <- mutate(unemp_bls, year = str_sub(unemp_bls[ , 1], start = 32, end = 35))
unemp_bls <- mutate(unemp_bls, month = str_sub(unemp_bls[ , 1], start = 38, end = 39))
unemp_bls <- mutate(unemp_bls, value = str_trim(str_sub(unemp_bls[ , 1], start = 41, end = 52)))
unemp_bls <- mutate(unemp_bls, fips_state = str_sub(fips_state_county, start = 1, end = 2))
unemp_bls <- mutate(unemp_bls, fips_county = str_sub(fips_state_county, start = 3, end = 5))
unemp_bls <- select(unemp_bls, fips_state_county:fips_county)

# filter
year_start <- 2014
year_end <- 2015
unemp_bls <- filter(unemp_bls, year >= year_start, year <= year_end, measure %in% c(3, 6), month == "13")

# spread year to wide form
unemp_bls <- unemp_bls %>% mutate(measure_name = case_when(.$measure == 3 ~ "unemployment_rate", 
                                                           .$measure == 6 ~ "labor_force")) %>% 
        unite(measure_year, c(measure_name, year)) %>% select(-measure) %>%
        spread(key = measure_year, value = value)

# write clean data to file
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
unemp_bls_filename <- str_c("lau/lau_", date, ".csv")
write_csv(unemp_bls, unemp_bls_filename)




