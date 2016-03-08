library(RCurl)
library(XML)
library(stringr)
library(dplyr)
library(readr)
library(reshape2)

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
# see distress_scratchpad2 ~ line 300, these two counties are in fips, but not unemp_nat or statsamerica
fips <- fips[-c(549, 2917), ]

length(unique(fips$fips_state))
length(unique(fips$fips_state_county))



# hit census api call for all counties per capita income in 2013 dollars, write to csv, and import
# http://www.census.gov/data/developers/data-sets/acs-survey-3-year-data.html
# note that acs 3yr only covers geographies with 20k population or more, so only 1910 counties of 3141 total
# pc_inc <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0088E,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
# write(pc_inc, file = "income/pc_inc.csv")
# pc_inc <- read.csv("income/pc_inc.csv")

# hit census api for acs 5yr 2010-2014 county per cap income 
pc_inc <- getURL("http://api.census.gov/data/2014/acs5/profile?get=DP03_0088E,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
write(pc_inc, file = "income/pc_inc_5yr_acs.csv")
pc_inc <- read.csv("income/pc_inc_5yr_acs.csv")

# clean data and rename columns
pc_inc <- as.data.frame(lapply(pc_inc, function(x) str_replace(x, "\\[", "")))
pc_inc <- as.data.frame(lapply(pc_inc, function(x) str_replace(x, "\\]", "")))
pc_inc <- select(pc_inc, -X)
names(pc_inc) <- c("pc_inc", "county_state", "fips_state", "fips_county")
pc_inc$fips_state <- str_pad(pc_inc$fips_state, 2, pad = "0")
pc_inc <- mutate(pc_inc, fips_state_county = str_c(fips_state, fips_county))
pc_inc <- filter(pc_inc, fips_state_county %in% unique(fips$fips_state_county))

# hit census api call for national per capita income in 2013 dollars, write to csv, and import
# pc_inc_nat <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0088E,NAME&for=us:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
# write(pc_inc_nat, file = "income/pc_inc_nat.csv")
# pc_inc_nat <- read.csv("income/pc_inc_nat.csv")

# hit census api call for acs 5yr 2010-2014 national per capita income in 2013 dollars, write to csv, and import
pc_inc_nat <- getURL("http://api.census.gov/data/2014/acs5/profile?get=DP03_0088E,NAME&for=us:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
write(pc_inc_nat, file = "income/pc_inc_nat_5yr_acs.csv")
pc_inc_nat <- read.csv("income/pc_inc_nat_5yr_acs.csv")

# clean data and rename columns
pc_inc_nat <- as.data.frame(lapply(pc_inc_nat, function(x) str_replace(x, "\\[", "")))
pc_inc_nat <- as.data.frame(lapply(pc_inc_nat, function(x) str_replace(x, "\\]", "")))
pc_inc_nat <- select(pc_inc_nat, 1)
pc_inc_nat <- pc_inc_nat[ , 1][1]
pc_inc_nat <- as.numeric(as.character(pc_inc_nat)) # matches statsamerica

# load bls lau county data from flat files
# bls <- getURL("http://download.bls.gov/pub/time.series/la/la.data.64.County")
# write(bls, file = "unemployment/bls.csv")
bls <- read.csv("unemployment/bls.csv")
unemp_bls <- bls
unemp_bls <- mutate(unemp_bls, fips_state_county = str_sub(unemp_bls[ , 1], start = 6, end = 10))
unemp_bls <- mutate(unemp_bls, measure = str_sub(unemp_bls[ , 1], start = 20, end = 20))
unemp_bls <- mutate(unemp_bls, year = str_sub(unemp_bls[ , 1], start = 32, end = 35))
unemp_bls <- mutate(unemp_bls, month = str_sub(unemp_bls[ , 1], start = 38, end = 39))
unemp_bls <- mutate(unemp_bls, value = str_trim(str_sub(unemp_bls[ , 1], start = 41, end = 52)))
unemp_bls <- mutate(unemp_bls, fips_state = str_sub(fips_state_county, start = 1, end = 2))
unemp_bls <- mutate(unemp_bls, fips_county = str_sub(fips_state_county, start = 3, end = 5))
unemp_bls <- select(unemp_bls, fips_state_county:fips_county)
unemp_bls <- filter(unemp_bls, year > 2012, measure != 3, month != "13", fips_state %in% unique(fips$fips_state))

write_csv(unemp_bls, "unemployment/unemp_bls.csv")
unemp_bls <- read_csv("unemployment/unemp_bls.csv")

last_24_months <- function(x){
        last_24_months = x[1:24, ]
        last_24_months
}

bls_24month <- unemp_bls %>%
        group_by(fips_state_county, measure) %>%
        arrange(desc(year), desc(month)) %>%
        do(last_24_months(.))

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

# test to confirm that unemp_nat, fips, and statsamerica all have the same counties
sa <- read_csv("unemployment/statsamerica_24month_unemployment.csv")
sa <- data.frame(sa)
length(unique(fips$fips_state_county))
length(unique(unemp_nat$fips_state_county))
length(unique(sa$FIPS))
x <- arrange(fips, as.numeric(fips_state_county)) %>%
        select(fips_state_county) 
x <- unique(x)
y <- arrange(unemp_nat, as.numeric(fips_state_county)) %>%
        select(fips_state_county)
y <- unique(y)
identical(x$fips_state_county, y$fips_state_county)
z <- arrange(sa, as.numeric(FIPS)) %>%
        select(FIPS)
z <- unique(z)
identical(x$fips_state_county, z$FIPS)
which(!(fips$fips_state_county %in% unemp_nat$fips_state_county))
fips_unique <- unique(fips$fips_state_county)
unemp_unique <- unique(unemp_nat$fips_state_county)
fips[c(549, 2917),]
# fips has 3143 unique state_county, unemp_nat has only 3141, statsamerica also has 3141
# statsamerica and unemp_nat both lack kalawao county, hi; unemp_nat and statsamerica lack bedford city, VA, (not bedford county va)
sa <- read_csv("unemployment/statsamerica_24month_unemployment.csv")
sa <- data.frame(sa)
sa$FIPS <- str_pad(sa$FIPS, 5, "left", "0")
length(unique(sa$FIPS))
which(!(unemp_nat$fips_state_county %in% sa$FIPS))
which(!(sa$FIPS %in% unemp_nat$fips_state_county))
filter(unemp_nat, fips_state_county == "51515")
filter(sa, FIPS == "51515")

# create combined file with county unemployment and pc_inc, plus national measures, with flag for whether distressed
# as well the amount by which they exceed the threshold
str(unemp_nat)
str(fips)
str(sa)
str(pc_inc)
str(pc_inc_nat)


fips_info <- select(fips, state, fips_state_county)
counties <- pc_inc
counties <- left_join(counties, fips_info, by = "fips_state_county")

# need to dcast unemp_nat to get measures in columns
unemp_sum <- unemp_nat %>%
        group_by(fips_state_county, measure) %>%
        summarize(
                value_sum = sum(as.numeric(value))
        )
unemp_sum_cast <- dcast(unemp_sum, fips_state_county ~ measure, value.var = "value_sum")
names(unemp_sum_cast) <- c("fips_state_county", "unemployed", "employed", "laborforce")

# combine unemp_sum_cast with counties
counties <- left_join(counties, unemp_sum_cast, by = "fips_state_county")

# calculate unemployment rate and add national pc_in and nationaol unemployment rate
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

write_csv(counties, "counties.csv")
counties <- read_csv("counties.csv")




