library(stringr)
library(dplyr)
library(readr)
library(tidyr)
library(lazyeval)
library(jsonlite)

# setwd
# setwd("H:/PNP/SRI Baseline Metrics")
# setwd("C:/Users/Stephen/Desktop/R/EDA/sri_metrics")
setwd("G:/PNP/Performance Measurement/SRI Baseline Metrics")

# acs 5 year has all counties
# https://www.census.gov/data/developers/data-sets/acs-5year.html
# acs 5yr variables: http://api.census.gov/data/2015/acs5/profile/variables.html
# acs 5yr profile api call: http://api.census.gov/data/2015/acs5/profile.html
# note it appears that 2013 is earliest year for api data

# acs calculates labor force participation rate as population over 16 / pop. in laborforce - see link below 
# https://www.census.gov/people/laborforce/about/acs_employ.html

# api template
# http://api.census.gov/data/2015/acs5/profile?get=DP03_0002E,NAME&for=county:*&in=state:*&key=905cf5cb3674a223a81618f2365a799a6330bed4

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

# fips_full <- fips
# fips <- fips_full

################################################


# save api call template
api_template <- "http://api.census.gov/data/YEAR/acs5/profile?get=VARIABLE_NAME,NAME&for=county:*&in=state:*&key=905cf5cb3674a223a81618f2365a799a6330bed4"

# set year ranges (as of 8/2017 the latest year is 2015)
year_start <- 2015
year_end <- 2014
year_range <- seq(year_end, year_start, by = 1)

for(year in year_range) {
        
        # get laborforce data
        laborforce_api_call <- str_replace(api_template, "YEAR", as.character(year))
        laborforce_api_call <- str_replace(laborforce_api_call, "VARIABLE_NAME", "DP03_0002E")
        laborforce <- data.frame(fromJSON(laborforce_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        laborforce <- laborforce[-1, ]
        names(laborforce) <- c("labor_force", "county", "fips_state", "fips_county")
        laborforce <- laborforce %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_laborforce <- fips$fips_state_county[!(fips$fips_state_county %in% laborforce$fips_state_county)]
        missing_from_laborforce <- fips %>% filter(fips_state_county %in% missing_from_laborforce)
        laborforce_additions <- data.frame(labor_force = rep(NA, nrow(missing_from_laborforce)), 
                                           county = missing_from_laborforce$county,
                                           fips_state = missing_from_laborforce$fips_state,
                                           fips_county = missing_from_laborforce$fips_county,
                                           fips_state_county = missing_from_laborforce$fips_state_county)
        laborforce <- rbind(laborforce, laborforce_additions)

        # join laborforce with fips
        laborforce_join <- laborforce %>% select(fips_state_county, labor_force)

        ######################################
        
        # get population data
        population_api_call <- str_replace(api_template, "YEAR", as.character(year))
        population_api_call <- str_replace(population_api_call, "VARIABLE_NAME", "DP03_0001PE")
        population <- data.frame(fromJSON(population_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        population <- population[-1, ]
        names(population) <- c("population", "county", "fips_state", "fips_county")
        population <- population %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_population <- fips$fips_state_county[!(fips$fips_state_county %in% population$fips_state_county)]
        missing_from_population <- fips %>% filter(fips_state_county %in% missing_from_population)
        population_additions <- data.frame(population = rep(NA, nrow(missing_from_population)), 
                                           county = missing_from_population$county,
                                           fips_state = missing_from_population$fips_state,
                                           fips_county = missing_from_population$fips_county,
                                           fips_state_county = missing_from_population$fips_state_county)
        population <- rbind(population, population_additions)
        
        # join laborforce with fips
        population_join <- population %>% select(fips_state_county, population)
        

        ############################
        
        
        # join labor_force and population to create lf_participation_rate
        lf_participation_rate_join <- left_join(laborforce_join, population_join, by = c("fips_state_county" = "fips_state_county"))
        
        # convert labor_force and population to numeric
        lf_participation_rate_join$labor_force <- as.numeric(as.character(lf_participation_rate_join$labor_force))
        lf_participation_rate_join$population <- as.numeric(as.character(lf_participation_rate_join$population))
        
        # calculate labor force participation rate
        lf_participation_rate_join <- lf_participation_rate_join %>% mutate(lf_participation_rate = (labor_force / population)*100)
        
        # dynamically rename variables to include given year
        current_column_name <- "lf_participation_rate"
        new_column_name <- str_c("lf_participation_rate_", year)

        mutate_criteria <- setNames(list(interp(~x, 
                                        x = as.name(current_column_name))), 
                                        nm = eval(new_column_name))
        
        lf_participation_rate_join <- lf_participation_rate_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-c(lf_participation_rate, labor_force, population))

        # add lf_participation_rate to fips
        fips <- left_join(fips, lf_participation_rate_join, by = c("fips_state_county" = "fips_state_county"))

        
        ################################################################
        
        
        # pull education_less_9
        education_less_9_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_less_9_api_call <- str_replace(education_less_9_api_call, "VARIABLE_NAME", "DP02_0059PE")
        education_less_9 <- data.frame(fromJSON(education_less_9_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_less_9 <- education_less_9[-1, ]
        names(education_less_9) <- c("education_less_9", "county", "fips_state", "fips_county")
        education_less_9 <- education_less_9 %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_less_9$fips_state_county[!(education_less_9$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_less_9 %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_less_9 <- education_less_9 %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_less_9 <- fips$fips_state_county[!(fips$fips_state_county %in% education_less_9$fips_state_county)]
        missing_from_education_less_9 <- fips %>% filter(fips_state_county %in% missing_from_education_less_9)
        education_less_9_additions <- data.frame(education_less_9 = rep(NA, nrow(missing_from_education_less_9)), 
                                           county = missing_from_education_less_9$county,
                                           fips_state = missing_from_education_less_9$fips_state,
                                           fips_county = missing_from_education_less_9$fips_county,
                                           fips_state_county = missing_from_education_less_9$fips_state_county)
        education_less_9 <- rbind(education_less_9, education_less_9_additions)
        
        # join education_less_9 with fips
        education_less_9_join <- education_less_9 %>% select(fips_state_county, education_less_9)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_less_9"
        new_column_name <- str_c("education_less_9_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_less_9_join <- education_less_9_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_less_9)
        
        # add education_less_9 to fips
        fips <- left_join(fips, education_less_9_join, by = c("fips_state_county" = "fips_state_county"))
        
        
        ###############################
        
        
        # pull education_9_to_12
        education_9_to_12_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_9_to_12_api_call <- str_replace(education_9_to_12_api_call, "VARIABLE_NAME", "DP02_0060PE")
        education_9_to_12 <- data.frame(fromJSON(education_9_to_12_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_9_to_12 <- education_9_to_12[-1, ]
        names(education_9_to_12) <- c("education_9_to_12", "county", "fips_state", "fips_county")
        education_9_to_12 <- education_9_to_12 %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_9_to_12$fips_state_county[!(education_9_to_12$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_9_to_12 %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_9_to_12 <- education_9_to_12 %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_9_to_12 <- fips$fips_state_county[!(fips$fips_state_county %in% education_9_to_12$fips_state_county)]
        missing_from_education_9_to_12 <- fips %>% filter(fips_state_county %in% missing_from_education_9_to_12)
        education_9_to_12_additions <- data.frame(education_9_to_12 = rep(NA, nrow(missing_from_education_9_to_12)), 
                                                 county = missing_from_education_9_to_12$county,
                                                 fips_state = missing_from_education_9_to_12$fips_state,
                                                 fips_county = missing_from_education_9_to_12$fips_county,
                                                 fips_state_county = missing_from_education_9_to_12$fips_state_county)
        education_9_to_12 <- rbind(education_9_to_12, education_9_to_12_additions)
        
        # join education_9_to_12 with fips
        education_9_to_12_join <- education_9_to_12 %>% select(fips_state_county, education_9_to_12)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_9_to_12"
        new_column_name <- str_c("education_9_to_12_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_9_to_12_join <- education_9_to_12_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_9_to_12)
        
        # add education_9_to_12 to fips
        fips <- left_join(fips, education_9_to_12_join, by = c("fips_state_county" = "fips_state_county"))
        
        
        ################################################
        
        
        # pull education_hs_grad
        education_hs_grad_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_hs_grad_api_call <- str_replace(education_hs_grad_api_call, "VARIABLE_NAME", "DP02_0061PE")
        education_hs_grad <- data.frame(fromJSON(education_hs_grad_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_hs_grad <- education_hs_grad[-1, ]
        names(education_hs_grad) <- c("education_hs_grad", "county", "fips_state", "fips_county")
        education_hs_grad <- education_hs_grad %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_hs_grad$fips_state_county[!(education_hs_grad$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_hs_grad %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_hs_grad <- education_hs_grad %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_hs_grad <- fips$fips_state_county[!(fips$fips_state_county %in% education_hs_grad$fips_state_county)]
        missing_from_education_hs_grad <- fips %>% filter(fips_state_county %in% missing_from_education_hs_grad)
        education_hs_grad_additions <- data.frame(education_hs_grad = rep(NA, nrow(missing_from_education_hs_grad)), 
                                                  county = missing_from_education_hs_grad$county,
                                                  fips_state = missing_from_education_hs_grad$fips_state,
                                                  fips_county = missing_from_education_hs_grad$fips_county,
                                                  fips_state_county = missing_from_education_hs_grad$fips_state_county)
        education_hs_grad <- rbind(education_hs_grad, education_hs_grad_additions)
        
        # join education_hs_grad with fips
        education_hs_grad_join <- education_hs_grad %>% select(fips_state_county, education_hs_grad)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_hs_grad"
        new_column_name <- str_c("education_hs_grad_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_hs_grad_join <- education_hs_grad_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_hs_grad)
        
        # add education_hs_grad to fips
        fips <- left_join(fips, education_hs_grad_join, by = c("fips_state_county" = "fips_state_county"))
        
        
        ######################################################
        
        # pull education_some_college
        education_some_college_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_some_college_api_call <- str_replace(education_some_college_api_call, "VARIABLE_NAME", "DP02_0062PE")
        education_some_college <- data.frame(fromJSON(education_some_college_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_some_college <- education_some_college[-1, ]
        names(education_some_college) <- c("education_some_college", "county", "fips_state", "fips_county")
        education_some_college <- education_some_college %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_some_college$fips_state_county[!(education_some_college$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_some_college %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_some_college <- education_some_college %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_some_college <- fips$fips_state_county[!(fips$fips_state_county %in% education_some_college$fips_state_county)]
        missing_from_education_some_college <- fips %>% filter(fips_state_county %in% missing_from_education_some_college)
        education_some_college_additions <- data.frame(education_some_college = rep(NA, nrow(missing_from_education_some_college)), 
                                                  county = missing_from_education_some_college$county,
                                                  fips_state = missing_from_education_some_college$fips_state,
                                                  fips_county = missing_from_education_some_college$fips_county,
                                                  fips_state_county = missing_from_education_some_college$fips_state_county)
        education_some_college <- rbind(education_some_college, education_some_college_additions)
        
        # join education_some_college with fips
        education_some_college_join <- education_some_college %>% select(fips_state_county, education_some_college)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_some_college"
        new_column_name <- str_c("education_some_college_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_some_college_join <- education_some_college_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_some_college)
        
        # add education_some_college to fips
        fips <- left_join(fips, education_some_college_join, by = c("fips_state_county" = "fips_state_county"))
        
        
        ###############################################
        
        
        # pull education_associates
        education_associates_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_associates_api_call <- str_replace(education_associates_api_call, "VARIABLE_NAME", "DP02_0063PE")
        education_associates <- data.frame(fromJSON(education_associates_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_associates <- education_associates[-1, ]
        names(education_associates) <- c("education_associates", "county", "fips_state", "fips_county")
        education_associates <- education_associates %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_associates$fips_state_county[!(education_associates$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_associates %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_associates <- education_associates %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_associates <- fips$fips_state_county[!(fips$fips_state_county %in% education_associates$fips_state_county)]
        missing_from_education_associates <- fips %>% filter(fips_state_county %in% missing_from_education_associates)
        education_associates_additions <- data.frame(education_associates = rep(NA, nrow(missing_from_education_associates)), 
                                                  county = missing_from_education_associates$county,
                                                  fips_state = missing_from_education_associates$fips_state,
                                                  fips_county = missing_from_education_associates$fips_county,
                                                  fips_state_county = missing_from_education_associates$fips_state_county)
        education_associates <- rbind(education_associates, education_associates_additions)
        
        # join education_associates with fips
        education_associates_join <- education_associates %>% select(fips_state_county, education_associates)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_associates"
        new_column_name <- str_c("education_associates_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_associates_join <- education_associates_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_associates)
        
        # add education_associates to fips
        fips <- left_join(fips, education_associates_join, by = c("fips_state_county" = "fips_state_county"))
        
        
        ###################################################
        
        
        # pull education_bachelors
        education_bachelors_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_bachelors_api_call <- str_replace(education_bachelors_api_call, "VARIABLE_NAME", "DP02_0064PE")
        education_bachelors <- data.frame(fromJSON(education_bachelors_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_bachelors <- education_bachelors[-1, ]
        names(education_bachelors) <- c("education_bachelors", "county", "fips_state", "fips_county")
        education_bachelors <- education_bachelors %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_bachelors$fips_state_county[!(education_bachelors$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_bachelors %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_bachelors <- education_bachelors %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_bachelors <- fips$fips_state_county[!(fips$fips_state_county %in% education_bachelors$fips_state_county)]
        missing_from_education_bachelors <- fips %>% filter(fips_state_county %in% missing_from_education_bachelors)
        education_bachelors_additions <- data.frame(education_bachelors = rep(NA, nrow(missing_from_education_bachelors)), 
                                                  county = missing_from_education_bachelors$county,
                                                  fips_state = missing_from_education_bachelors$fips_state,
                                                  fips_county = missing_from_education_bachelors$fips_county,
                                                  fips_state_county = missing_from_education_bachelors$fips_state_county)
        education_bachelors <- rbind(education_bachelors, education_bachelors_additions)
        
        # join education_bachelors with fips
        education_bachelors_join <- education_bachelors %>% select(fips_state_county, education_bachelors)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_bachelors"
        new_column_name <- str_c("education_bachelors_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_bachelors_join <- education_bachelors_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_bachelors)
        
        # add education_bachelors to fips
        fips <- left_join(fips, education_bachelors_join, by = c("fips_state_county" = "fips_state_county"))
        
        
        ###################################################
        
        
        # pull education_graduate
        education_graduate_api_call <- str_replace(api_template, "YEAR", as.character(year))
        education_graduate_api_call <- str_replace(education_graduate_api_call, "VARIABLE_NAME", "DP02_0065PE")
        education_graduate <- data.frame(fromJSON(education_graduate_api_call))
        
        # name columns and drop row 1 because it read column names as row1
        education_graduate <- education_graduate[-1, ]
        names(education_graduate) <- c("education_graduate", "county", "fips_state", "fips_county")
        education_graduate <- education_graduate %>% mutate(fips_state_county = str_c(fips_state, fips_county))
        
        # drop counties not in 3143 counties in fips
        dropped_fips_state_county <- education_graduate$fips_state_county[!(education_graduate$fips_state_county %in% fips$fips_state_county)]
        dropped_counties <- education_graduate %>% filter(fips_state_county %in% dropped_fips_state_county) %>% .$county
        dropped_counties <- str_c(dropped_counties, collapse = "; ")
        print(str_c("counties dropped from ", year, ": ", dropped_counties))
        education_graduate <- education_graduate %>% filter(!(fips_state_county %in% dropped_fips_state_county))
        
        # need to add placeholder rows for two counties in census fips but not in 5yr acs, likely because they are newly created
        missing_from_education_graduate <- fips$fips_state_county[!(fips$fips_state_county %in% education_graduate$fips_state_county)]
        missing_from_education_graduate <- fips %>% filter(fips_state_county %in% missing_from_education_graduate)
        education_graduate_additions <- data.frame(education_graduate = rep(NA, nrow(missing_from_education_graduate)), 
                                                  county = missing_from_education_graduate$county,
                                                  fips_state = missing_from_education_graduate$fips_state,
                                                  fips_county = missing_from_education_graduate$fips_county,
                                                  fips_state_county = missing_from_education_graduate$fips_state_county)
        education_graduate <- rbind(education_graduate, education_graduate_additions)
        
        # join education_graduate with fips
        education_graduate_join <- education_graduate %>% select(fips_state_county, education_graduate)
        
        # dynamically rename variables to include given year
        current_column_name <- "education_graduate"
        new_column_name <- str_c("education_graduate_", year)
        
        mutate_criteria <- setNames(list(interp(~x, 
                                                x = as.name(current_column_name))), 
                                    nm = eval(new_column_name))
        
        education_graduate_join <- education_graduate_join %>% mutate_(.dots = mutate_criteria) %>% 
                select(-education_graduate)
        
        # add education_graduate to fips
        fips <- left_join(fips, education_graduate_join, by = c("fips_state_county" = "fips_state_county"))
}

# inspect
glimpse(fips)

# write clean data to file
date <- as.character(Sys.Date())
date <- str_replace_all(date, "-", "")
acs_filename <- str_c("acs/acs_", date, ".csv")
write_csv(fips, acs_filename)






