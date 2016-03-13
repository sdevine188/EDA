import pandas as pd
import json
from pprint import pprint

% cd "c:\\users\\stephen\\desktop\\r\\eda\\distress\\county_maps\\mygeodata (2)"
% ls



with open('cb_2014_us_county_20m.json') as data_file:    
    data = json.load(data_file)

type(data)
len(data)
data.keys()
type(data["type"]) # list
type(data["crs"]) # dict
len(data["crs"])
data["type"]
data["crs"]
type(data["features"]) # this is a list
len(data["features"]) # 3220, this includes territories
type(data["features"][1]) # this is a dict
data["features"][1].keys()
data["features"][1]
data["features"][1]["properties"]
type(data["features"][1]["properties"]) # this is a dict
data["features"][1]["properties"].keys()
data["features"][1]["properties"]["STATEFP"]

# add a new key-value pair to the properties dict
data["features"][1]["properties"]["test"] = "hello"

# read in counties data with distress criteria
% cd "c:\\users\\stephen\\desktop\\r\\eda\\distress"
% ls

counties = pd.read_csv("counties.csv", converters={"fips_state_county": str})
counties.head()
counties.columns
counties.pc_inc[1]

# create list of us fips_state_county from counties
counties_us = counties.fips_state_county.tolist()
type(counties_us)
len(counties_us)
counties_us[0]

# create for loop to drop counties_json which are not in counties
# reduce from 3220 in counties_json down to 3141 in counties
# first create an index

counties_json = data["features"]
type(counties_json)
len(counties_json)
type(counties_json[1])
counties_json[1]["properties"]["STATEFP"]

counties_json1 = []
len(counties_json1)

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
        counties_json1.append(county)
        
fips_state_county = "18087"
counties.loc[counties.fips_state_county == fips_state_county].pc_inc.iloc[0].astype("str")  
type(counties_json[1]["properties"]["pc_inc"].to_string())
counties_json[1]["properties"]["pc_inc"] = counties.loc[counties.fips_state_county == fips_state_county].pc_inc.iloc[0]
# recombine the subsetted counties_json1 dict with the 
data["type"]
type(data["type"])
data["crs"]
type(json_crs)
json_crs["crs"]
type(counties_json1) # list
type(data)
data.keys()
counties_json2.keys()


counties_json2 = dict(features = counties_json1, crs = dict(crs = data["crs"]), type = "FeatureColleciton")
type(counties_json2)

import json
with open("counties_json2.json", 'w') as outfile:
    json.dump(counties_json2, outfile)

% pwd
with open("counties_json2.json") as data_file:    
    data2 = json.load(data_file)
data2["features"][1]


sum(counties.unemp_distress)


criteria = ["Unemployment", "Per capita income"]
