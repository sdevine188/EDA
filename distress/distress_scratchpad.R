library(RCurl)
library(XML)
library(stringr)
library(dplyr)

# get census county fips codes
fips_url <- getURL("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt")
fips <- read.csv(text = fips_url)
names(fips) <- c("state", "fips_state", "fips_county", "county", "unk")
fips$fips_state <- str_pad(fips$fips_state, 2, side = "left", pad = "0")
fips$fips_county <- str_pad(fips$fips_county, 3, "left", "0")
fips$fips <- str_c(fips$fips_state, fips$fips_county, sep = "")

# hit census api call for all counties per capita income in 2013 dollars, write to csv, and import
# http://www.census.gov/data/developers/data-sets/acs-survey-3-year-data.html
pc_inc <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0088E,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
write(pc_inc, file = "pc_inc.csv")
pc_inc <- read.csv("pc_inc.csv")

# clean data and rename columns
pc_inc <- as.data.frame(lapply(pc_inc, function(x) str_replace(x, "\\[", "")))
pc_inc <- as.data.frame(lapply(pc_inc, function(x) str_replace(x, "\\]", "")))
pc_inc <- select(pc_inc, -X)
names(pc_inc) <- c("pc_inc", "county_state", "fips_state", "fips_county")
pc_inc$fips_state <- str_pad(pc_inc$fips_state, 2, pad = "0")
pc_inc <- mutate(pc_inc, fips_state_county = str_c(fips_state, fips_county))

# hit census api call for national per capita income in 2013 dollars, write to csv, and import
pc_inc_nat <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0088E,NAME&for=us:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
write(pc_inc_nat, file = "pc_inc_nat.csv")
pc_inc_nat <- read.csv("pc_inc_nat.csv")

# clean data and rename columns
pc_inc_nat <- as.data.frame(lapply(pc_inc_nat, function(x) str_replace(x, "\\[", "")))
pc_inc_nat <- as.data.frame(lapply(pc_inc_nat, function(x) str_replace(x, "\\]", "")))
pc_inc_nat <- select(pc_inc_nat, 1)
names(pc_inc_nat) <- c("pc_inc_nat")

# import statsamerica's 24-month average county unemployment data from BLS
# http://www.statsamerica.org/distress/bulk_download.aspx
# note than statsamerica seems to calculate the last 24 month national average unemployment rate as a labor force-weighted average of all counties
sum(as.numeric(unemp$unemployed)) / sum(as.numeric(unemp$labor_force))
# statsamerica uses bls data, even though eda regulations say ACS data is primary - probably because of "last 24-month" requirement
# bls website seems to have only county annual averages, or last 14 months: http://www.bls.gov/lau/#cntyaa 
# but bls annual avg does not match statsamerica, which uses bls data, but some other measure - maybe last 8 quarters
unemp <- read.csv("unemployment/unemployment.csv")
names(unemp) <- c("county", "state", "fips_state_county", "year", "month", "labor_force", "employed", "unemployed", "unemp_rate")


# bls api
# http://www.bls.gov/developers/api_r.htm
# http://www.bls.gov/developers/api_FAQs.htm#signatures3
# series id format: http://www.bls.gov/help/hlpforma.htm
# flat files and codes: http://download.bls.gov/pub/time.series/la/
library(devtools)
install_github("mikeasilva/blsAPI")
library(blsAPI)
library(jsonlite)

# test from website
series <- list('seriesid'=c('LAUCN040010000000005','LAUCN040010000000006')) 
response <- blsAPI(series)
data <- fromJSON(response)
str(data$Results$series$data)
head(data$Results$series$data)

# api is buggy, just use the url to pull json
# hartford unemployment
data <- fromJSON("http://api.bls.gov/publicAPI/v1/timeseries/data/LAUCT372800000000003")
head(data$Results$series$data)

# best method
# import bls unemployment data from flat FTP:http://www.bls.gov/lau/lausad.htm#flat , http://download.bls.gov/pub/time.series/la/
# measures: 3-unemployment rate, 4-unemployment, 5-employment, 6-laborforce
bls <- getURL("http://download.bls.gov/pub/time.series/la/la.data.64.County")
write(bls, file = "unemployment/bls.csv")
bls <- read.csv("unemployment/bls.csv")
unemp_bls <- bls
unemp_bls <- mutate(unemp_bls, fips_state_county = str_sub(unemp_bls[ , 1], start = 6, end = 10))
unemp_bls <- mutate(unemp_bls, measure = str_sub(unemp_bls[ , 1], start = 20, end = 20))
unemp_bls <- mutate(unemp_bls, year = str_sub(unemp_bls[ , 1], start = 32, end = 35))
unemp_bls <- mutate(unemp_bls, month = str_sub(unemp_bls[ , 1], start = 38, end = 39))
unemp_bls <- mutate(unemp_bls, unemp_rate = str_sub(unemp_bls[ , 1], start = 50, end = 52))
unemp_bls <- mutate(unemp_bls, fips_state = str_sub(fips_state_county, start = 1, end = 2))
unemp_bls <- mutate(unemp_bls, fips_county = str_sub(fips_state_county, start = 3, end = 5))
unemp_bls <- select(unemp_bls, fips_state_county:fips_county)

# import census crosswalk data file for CBSA to county - not the best crosswalk though for "urbanized area" - see urban crosswalk below
# http://www.census.gov/population/metro/data/def.html
# clean data
urban <- read.csv("urban/urban.csv")
urban$FIPS.State.Code <- str_pad(urban$FIPS.State.Code, 2, pad = "0")
urban$FIPS.County.Code <- str_pad(urban$FIPS.County.Code, 3, pad = "0")
urban <- mutate(urban, fips_state_county = str_c(FIPS.State.Code, FIPS.County.Code))

# urban area to county crosswalk
# based on census 2010 urban area to county relationship file
# https://www.census.gov/geo/maps-data/data/ua_rel_download.html
crosswalk <- getURL("http://www2.census.gov/geo/docs/maps-data/data/rel/ua_county_rel_10.txt")
write(crosswalk, file = "urban/urban_county_crosswalk.csv")
uc_cross <- read.csv("urban/urban_county_crosswalk.csv")

# if needed, HUD Zipcode to county/tract/CD file
# http://www.huduser.org/portal/datasets/usps_crosswalk.html

# census, tract to zip to county relationship file
# http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt
relationship <- getURL("http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_tract_rel_10.txt")
write(relationship, file = "region/relationship.csv")
relationship <- read.csv("region/relationship.csv")

# import and clean RIS data
ris <- read.csv("ris/ris.csv")

# don't use this, it mixes and matches data sources
# import census ACS employment data by block group
# http://www.census.gov/people/laborforce/about/acs_employ.html
block <- getURL("http://www.census.gov/people/laborforce/about/EmploymentStatusforBlockGroups2006-20105-YearACS.csv")
write(block, file = "unemployment/block.csv")
block <- read.csv("unemployment/block.csv")


# census acs 5yr api for per capita income down to tract level
# can only get tract one state at a time
# http://www.census.gov/data/developers/data-sets/acs-survey-5-year-data.html#notes
tract_pcinc5 <- getURL("http://api.census.gov/data/2013/acs5?get=NAME,B19301_001E&for=tract:*&in=state:01&key=905cf5cb3674a223a81618f2365a799a6330bed4")
tract_pcinc5 <- read.csv(text = tract_pcinc5)

# http://www.census.gov/data/developers/data-sets/acs-survey-5-year-data.html#notes
# ACS 5 yr 2013 percent unemployed by tract
tract_unemp <- getURL("http://api.census.gov/data/2013/acs5/profile?get=DP03_0009PE&for=tract:*&in=state:01&key=905cf5cb3674a223a81618f2365a799a6330bed4")
tract_unemp1 <- read.csv(text = tract_unemp)

# census acs 3yr for unemployment data down to county level, or MSA, CSA, urban area, congressional district, but not tract level
# http://www.census.gov/data/developers/data-sets/acs-survey-3-year-data.html
# on variables documentation, the variable name ending in just "E" is a count,  ending in "PE" is a percentage
data <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0003E,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
data2 <- read.csv(text = data)

data3 <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0003PE,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
data4 <- read.csv(text = data3)

data5 <- getURL("http://api.census.gov/data/2013/acs3/profile?get=DP03_0009PE,NAME&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
data6 <- read.csv(text = data5)


# test of how stats america calculates pc_inc for CSA.  is it population-weighted? yes it is

head(pc_inc)
filter(pc_inc, county_state == "Middlesex County, Connecticut")
filter(pc_inc, county_state == "Hartford County, Connecticut")
filter(pc_inc, county_state == "New London County, Connecticut")
filter(pc_inc, county_state == "Tolland County, Connecticut")

middlesex_pop <- 165741
hartford_pop <- 897426
newlondon_pop <- 274239
tolland_pop <- 152097

middlesex <- c(40657, 165741)
hartford <- c(34307, 897426)
newlondon <- c(33248, 274239)
tolland <- c(33547, 152097)

ct <- data.frame(middlesex, hartford, newlondon, tolland)
ct <- as.data.frame(t(ct))
names(ct) <- c("pc_inc", "pop")

ct$avg <- ct$pc_inc * ct$pop
sum(ct$avg) / sum(ct$pop)


# test of census api
DP05_0001E
http://api.census.gov/data/2013/acs3/profile?get=DP05_0001E&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4

# http://www.census.gov/data/developers/data-sets/acs-survey-5-year-data.html#notes
# ACS 5 yr 2013 percent unemployed by tract
tract_unemp <- getURL("http://api.census.gov/data/2013/acs5/profile?get=DP03_0009PE&for=tract:*&in=state:01&key=905cf5cb3674a223a81618f2365a799a6330bed4")
tract_unemp1 <- read.csv(text = tract_unemp)

http://api.census.gov/data/2013/acs5/profile?get=DP03_0009PE&for=state:*&key=
county_unemp <- getURL("http://api.census.gov/data/2013/acs5/profile?get=DP03_0009PE&for=county:*&key=905cf5cb3674a223a81618f2365a799a6330bed4")
county_unemp1 <- read.csv(text = county_unemp)

# test of how stats america calculates unemp for CSA.  is it labor force-weighted? no, it is sum(unemployed) / sum(labor force) for CSA counties
# but, the statsamerica app does not match this statistic as calculated from statsamerica csv
# use the bls flatfile data and then the stat matches the statsamerica app
# see "lau_test.csv"
head(unemp)
a <- filter(unemp, county == "Middlesex", state == "CT")
b <- filter(unemp, county == "Hartford", state == "CT")
c <- filter(unemp, county == "New London", state == "CT")
d <- filter(unemp, county == "Tolland", state == "CT")
ct_ue <- rbind(a, b, c, d)
ct_ue <- mutate(ct_ue, avg = labor_force * unemp_rate)
sum(ct_ue$avg) / sum(ct_ue$labor_force)
# doesn't match exactly w statsamerica... i get 6.92, statsamerica has 6.85
# try weighting by unemployed... which i get 6.96... should be same as weighting by labor force, so maybe difference is just rounding error??
ct_ue <- mutate(ct_ue, avg2 = unemployed * unemp_rate)
sum(ct_ue$avg2) / sum(ct_ue$unemployed)

# another test on greenville, NC
e <- filter(unemp, county == "Beaufort", state == "NC")
f <- filter(unemp, county == "Pitt", state == "NC")
nc_ue <- rbind(e, f)
nc_ue <- mutate(nc_ue, avg = labor_force * unemp_rate)
sum(nc_ue$avg) / sum(nc_ue$labor_force)
# i get 6.96, statsamerica gets 6.84
# trying weighting by unemployed - get 6.99
nc_ue <- mutate(nc_ue, avg2 = unemployed * unemp_rate)
sum(nc_ue$avg2) / sum(nc_ue$unemployed)
# trying summing labor force and summing unemployment and dividing unemployment by labor force
sum(nc_ue$unemployed) / sum(nc_ue$labor_force)




