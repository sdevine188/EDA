library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(readr)
library(reshape2)

setwd("C:/Users/Stephen/Desktop/R/EDA/distress")

# get census county fips codes
fips_url <- getURL("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt")
fips <- read.csv(text = fips_url)
write_csv(fips, "census_fips_state_county.csv")

# need to manually create headers on saved csv, or else poor autauga becomes the header
fips <- read_csv("census_fips_state_county.csv")
fips <- data.frame(fips)
names(fips) <- c("state", "fips_state", "fips_county", "county")
fips$fips_state <- str_pad(fips$fips_state, 2, "left", "0")
fips$fips_county <- str_pad(fips$fips_county, 3, "left", "0")
fips <- filter(fips, !(state %in% c("AS", "GU", "MP", "PR", "UM", "VI")))
fips$fips_state_county <- str_c(fips$fips_state, fips$fips_county)

# check against statsamerica counties
sa <- read_csv("unemployment/statsamerica_24month_unemployment_20170130.csv")
sa <- data.frame(sa)
sa$fips_state_county <- str_pad(sa$FIPS, 5, "left", "0")
head(sa)
dim(sa)
head(fips)
dim(fips)

length(unique(fips$fips_state))
length(unique(fips$fips_state_county))

# inspect missing fips
fips %>% filter(!(fips$fips_state_county %in% sa$fips_state_county)) %>% select(county, state, fips_state_county)

sa %>% filter(!(sa$fips_state_county %in% fips$fips_state_county)) %>% select(County, State, fips_state_county)

# choroplethr packages' county_choroplethr defaults to 3143 counties, which matches census
# statsamerica seems to be missing four counties, and including two counties that census/choroplethr do not track
# sticking with census/choroplethr to avoid mapping holes


####################################################


# hit census api for acs 5yr 2010-2014 county per cap income 
pc_inc <- getURL("http://api.census.gov/data/2015/acs5/profile?get=DP03_0088E,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
pc_inc_filename = str_c("income/pc_inc_5yr_acs_", date, ".csv")
write(pc_inc, file = pc_inc_filename)
pc_inc <- read.csv("income/pc_inc_5yr_acs_20170130.csv")


# clean data and rename columns
pc_inc <- as.data.frame(lapply(pc_inc, function(x) str_replace(x, "\\[", "")))
pc_inc <- as.data.frame(lapply(pc_inc, function(x) str_replace(x, "\\]", "")))
pc_inc <- select(pc_inc, -X)
names(pc_inc) <- c("pc_inc", "county_state", "fips_state", "fips_county")
pc_inc$fips_state <- str_pad(pc_inc$fips_state, 2, pad = "0")
pc_inc <- mutate(pc_inc, fips_state_county = str_c(fips_state, fips_county))

# check county overlap
# note pc_inc still includes PR and two counties that SA/BLS have, but census/choroplethr do not
dim(pc_inc)
fips %>% filter(!(fips$fips_state_county %in% pc_inc$fips_state_county)) %>% select(county, state, fips_state_county)
pc_inc %>% filter(!(pc_inc$fips_state_county %in% fips$fips_state_county)) %>% select(county_state, fips_state_county)
# pc_inc is missing three of the four counties that sa and bls are missing

# convert pc_inc variables from factors to characters to allow rbind of new counties
str(pc_inc)
pc_inc$pc_inc <- as.numeric(as.character(pc_inc$pc_inc))
pc_inc$county_state <- as.character(pc_inc$county_state)
pc_inc$fips_county <- str_pad(as.character(pc_inc$fips_county), 2, "left", 0)

# create and merge records, interpolating state average pc_inc
head(pc_inc)
state_avg_pc_inc <- pc_inc %>% filter(fips_state %in% c("02", "46", "51")) %>% group_by(fips_state) %>% 
        summarize(avg_pc_inc = mean(pc_inc, na.rm = TRUE))
c_02270 <- c(state_avg_pc_inc$avg_pc_inc[1], "Wade Hampton Census Area, Alaska", "02", "270", "02270")
c_46113 <- c(state_avg_pc_inc$avg_pc_inc[2], "Shannon County, South Dakota", "46", "113", "46113")
c_51515 <- c(state_avg_pc_inc$avg_pc_inc[3], "Bedford city", "51", "515", "51515")

pc_inc <- rbind(pc_inc, c_02270, c_46113, c_51515)

# remove PR and two non-census counties, and recheck county overlap
fips %>% filter(!(fips$fips_state_county %in% pc_inc$fips_state_county)) %>% select(county, state, fips_state_county)
pc_inc %>% filter(!(pc_inc$fips_state_county %in% fips$fips_state_county)) %>% select(county_state, fips_state_county)

pc_inc <- pc_inc %>% filter(fips_state_county %in% fips$fips_state_county)

dim(pc_inc)
dim(fips)
length(unique(pc_inc$fips_state_county))


###########################################


# hit census api call for acs 5yr 2010-2014 national per capita income in 2013 dollars, write to csv, and import
pc_inc_nat <- getURL("http://api.census.gov/data/2015/acs5/profile?get=DP03_0088E,NAME&for=us:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
pc_inc_nat_filename <- str_c("income/pc_inc_nat_5yr_acs_", date, ".csv")
write(pc_inc_nat, file = pc_inc_nat_filename)
pc_inc_nat <- read.csv("income/pc_inc_nat_5yr_acs_20170130.csv")

# clean data and rename columns
pc_inc_nat <- as.data.frame(lapply(pc_inc_nat, function(x) str_replace(x, "\\[", "")))
pc_inc_nat <- as.data.frame(lapply(pc_inc_nat, function(x) str_replace(x, "\\]", "")))
pc_inc_nat <- select(pc_inc_nat, 1)
pc_inc_nat <- pc_inc_nat[ , 1][1]
pc_inc_nat <- as.numeric(as.character(pc_inc_nat)) # matches statsamerica


######################################################################


# load bls lau county data from flat files
bls <- getURL("https://download.bls.gov/pub/time.series/la/la.data.64.County")
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
bls_filename <- str_c("unemployment/bls_", date, ".csv")
write(bls, file = bls_filename)
bls <- read.csv("unemployment/bls_20170130.csv")
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
unemp_bls <- filter(unemp_bls, year > 2012, measure != 3, month != "13", fips_state %in% unique(fips$fips_state))

# write clean data to file
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
unemp_bls_filename <- str_c("unemployment/unemp_bls_", date, ".csv")
write_csv(unemp_bls, unemp_bls_filename)
unemp_bls <- read_csv("unemployment/unemp_bls_20170130.csv")

last_24_months <- function(x){
        last_24_months = x[1:24, ]
        last_24_months
}

bls_24month <- unemp_bls %>%
        group_by(fips_state_county, measure) %>%
        arrange(desc(year), desc(month)) %>%
        do(last_24_months(.)) %>% ungroup(.)

# calulate national 24 month unemployment rate
# statsamerica does this by summing the county 24 month unemployed and dividing by the sum of county 24 month labor force
unemp_nat <- bls_24month
# confirm unemp_nat has same 3141 count of counties as stats america - all states, minus the territories
length(unique(unemp_nat$fips_state_county))
unemp_nat_sum <- unemp_nat %>%
        group_by(measure) %>%
        summarize(
                total_value = sum(as.numeric(value))
        )
unemp_nat_sum <- data.frame(unemp_nat_sum)
unemp_nat_value <- unemp_nat_sum$total_value[1] / unemp_nat_sum$total_value[3]
# this matches the statsamerica 24 month national unemployment


##########################################################


# create combined file with county unemployment and pc_inc, plus national measures, with flag for whether distressed
# as well the amount by which they exceed the threshold
str(unemp_nat)
str(fips)
str(sa)
str(pc_inc)
str(pc_inc_nat)

# use pc_inc as the base for counties
fips_info <- select(fips, state, fips_state_county)
counties <- pc_inc
counties <- left_join(counties, fips_info, by = "fips_state_county")


#########################################################3


# need to dcast unemp_nat to get measures in columns
unemp_sum <- unemp_nat %>%
        group_by(fips_state_county, measure) %>%
        summarize(
                value_sum = sum(as.numeric(value))
        )
unemp_sum_cast <- dcast(unemp_sum, fips_state_county ~ measure, value.var = "value_sum")
names(unemp_sum_cast) <- c("fips_state_county", "unemployed", "employed", "laborforce")


#####################################################


# check unemp_sum_cast for number of counties
# bls counties match stats america with 3141, don't match census/choroplethr
dim(unemp_sum_cast)
str(unemp_sum_cast)
head(unemp_sum_cast)
length(unique(unemp_sum_cast$fips_state_county))
fips %>% filter(!(fips$fips_state_county %in% unemp_sum_cast$fips_state_county)) %>% select(county, state, fips_state_county)
unemp_sum_cast %>% filter(!(unemp_sum_cast$fips_state_county %in% fips$fips_state_county)) %>% distinct(fips_state_county)

# create and merge records, interpolating state average value
state_avg_value <- unemp_sum_cast %>% mutate(fips_state = str_sub(fips_state_county, 1, 2)) %>% 
        filter(fips_state %in% c("02", "15", "46", "51")) %>% group_by(fips_state) %>% 
        summarize(avg_unemployed = mean(unemployed, na.rm = TRUE), avg_employed = mean(employed, na.rm = TRUE),
                                avg_laborforce = mean(laborforce, na.rm = TRUE))

c_02270 <- c("02270", state_avg_value$avg_unemployed[state_avg_value$fips_state == "02"], 
             state_avg_value$avg_employed[state_avg_value$fips_state == "02"],
             state_avg_value$avg_laborforce[state_avg_value$fips_state == "02"])
c_15005 <- c("15005", state_avg_value$avg_unemployed[state_avg_value$fips_state == "15"], 
             state_avg_value$avg_employed[state_avg_value$fips_state == "15"],
             state_avg_value$avg_laborforce[state_avg_value$fips_state == "15"])
c_46113 <- c("46113", state_avg_value$avg_unemployed[state_avg_value$fips_state == "46"], 
             state_avg_value$avg_employed[state_avg_value$fips_state == "46"],
             state_avg_value$avg_laborforce[state_avg_value$fips_state == "46"])
c_51515 <- c("51515", state_avg_value$avg_unemployed[state_avg_value$fips_state == "51"], 
             state_avg_value$avg_employed[state_avg_value$fips_state == "51"],
             state_avg_value$avg_laborforce[state_avg_value$fips_state == "51"])

unemp_sum_cast <- rbind(unemp_sum_cast, c_02270, c_15005, c_46113, c_51515)

# remove two non-census counties, and recheck county overlap
fips %>% filter(!(fips$fips_state_county %in% pc_inc$fips_state_county)) %>% select(county, state, fips_state_county)
unemp_sum_cast %>% filter(!(unemp_sum_cast$fips_state_county %in% fips$fips_state_county)) %>% select(fips_state_county)

unemp_sum_cast <- unemp_sum_cast %>% filter(fips_state_county %in% fips$fips_state_county)

dim(unemp_sum_cast)
dim(fips)
length(unique(unemp_sum_cast$fips_state_county))

# convert values to numeric
unemp_sum_cast$unemployed <- as.numeric(unemp_sum_cast$unemployed)
unemp_sum_cast$employed <- as.numeric(unemp_sum_cast$employed)
unemp_sum_cast$laborforce <- as.numeric(unemp_sum_cast$laborforce)


####################################################


# combine unemp_sum_cast with counties
counties <- left_join(counties, unemp_sum_cast, by = "fips_state_county")

# calculate unemployment rate and add national pc_in and national unemployment rate
counties <- counties %>%
        mutate(unemp_rate = (unemployed / laborforce) * 100) %>%
        mutate(unemp_rate_nat = unemp_nat_value * 100) %>%
        mutate(pc_inc_nat = pc_inc_nat)

counties$pc_inc <- as.numeric(as.character(counties$pc_inc))

# create flag for distressed on pc_inc and unemployment criteria
# criteria is local unemp rate at least 1 percent higher than national avg
# local pc_inc less than 80% of national avg
# https://www.eda.gov/how-to-apply/files/Eligibility-Requirements-and-Criteria.pdf
counties$pc_inc_distress <- sapply(1:nrow(counties), function(x) 
        ifelse((counties$pc_inc[x] / counties$pc_inc_nat[x]) * 100 < 80, 1, 0))

counties$pc_inc_threshold <- sapply(1:nrow(counties), function(x) 
        ((counties$pc_inc[x] / counties$pc_inc_nat[x]) * 100) - 80)
       
counties$unemp_distress <- sapply(1:nrow(counties), function(x) 
        ifelse((counties$unemp_rate[x] - counties$unemp_rate_nat[x]) > 1, 1, 0))

counties$unemp_threshold <- sapply(1:nrow(counties), function(x) (counties$unemp_rate[x] - counties$unemp_rate_nat[x]))

# write output file
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
counties_filename <- str_c("counties_", date, ".csv")
write_csv(counties, counties_filename)

counties <- read_csv("counties_20170131.csv")


