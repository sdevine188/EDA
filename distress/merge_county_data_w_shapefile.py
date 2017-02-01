import pandas as pd
import json

% cd "c:\\users\\stephen\\desktop\\r\\eda\\distress\\county_maps\\mygeodata (2)"

with open('cb_2014_us_county_20m.json') as data_file:    
    data = json.load(data_file)

# read in counties data with distress criteria
% cd "c:\\users\\stephen\\desktop\\r\\eda\\distress"

counties = pd.read_csv("counties.csv", converters={"fips_state_county": str})

# create list of us fips_state_county from counties
counties_us = counties.fips_state_county.tolist()

# create for loop to drop counties_json which are not in counties
# reduce from 3220 in counties_json down to 3141 in counties
# first create an index

counties_json = data["features"]
counties_json1 = []

for county in counties_json:
    fips_state_county = county["properties"]["STATEFP"] + county["properties"]["COUNTYFP"]
    print(fips_state_county)
    if fips_state_county in counties_us:
        county["properties"]["pc_inc"] = counties.loc[counties.fips_state_county == fips_state_county].pc_inc.iloc[0].astype("str") 
        county["properties"]["pc_inc_nat"] = counties.loc[counties.fips_state_county == fips_state_county].pc_inc_nat.iloc[0].astype("str") 
        county["properties"]["pc_inc_distress"] = counties.loc[counties.fips_state_county == fips_state_county].pc_inc_distress.iloc[0].astype("str") 
        county["properties"]["pc_inc_threshold"] = counties.loc[counties.fips_state_county == fips_state_county].pc_inc_threshold.iloc[0].astype("str") 
        county["properties"]["unemp_rate"] = counties.loc[counties.fips_state_county == fips_state_county].unemp_rate.iloc[0].astype("str") 
        county["properties"]["unemp_rate_nat"] = counties.loc[counties.fips_state_county == fips_state_county].unemp_rate_nat.iloc[0].astype("str")           
        county["properties"]["unemp_distress"] = counties.loc[counties.fips_state_county == fips_state_county].unemp_distress.iloc[0].astype("str") 
        county["properties"]["unemp_threshold"] = counties.loc[counties.fips_state_county == fips_state_county].unemp_threshold.iloc[0].astype("str") 
        county["properties"]["county_state"] = counties.loc[counties.fips_state_county == fips_state_county].county_state.iloc[0]        
        counties_json1.append(county)
        
# recombine the subsetted counties_json1 dict with the 
counties_json2 = dict(features = counties_json1, crs = dict(crs = data["crs"]), type = "FeatureColleciton")

% cd "c:\\users\\stephen\\desktop\\r\\eda\\distress\\county_maps\\mygeodata (2)"

with open("counties_json2.json", 'w') as outfile:
    json.dump(counties_json2, outfile)

