library(choroplethr)
library(readr)
library(choroplethrMaps)
library(ggplot2)
library(dplyr)
library(datasets)
library(maps)
library(stringr)
library(scales)
library(RColorBrewer)


# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/General Information/20170125/county_distress_map")

# load data on county distress 
counties <- read_csv("counties_20170131.csv")

# create region and value variables, which county_choropleth function uses
counties$region <- counties$fips_state_county
# remove leading zero on fips_state_county
for(i in 1:nrow(counties)) {
        if(str_sub(counties$region[i], 1, 1) == "0") {
                counties$region[i] <- str_sub(counties$region[i], 2, nchar(counties$region[i]))
        } 
}
counties$region <- as.numeric(counties$fips_state_county)


counties$value <- counties$pc_inc_distress

counties <- data.frame(counties)

# create choropleth
county_choropleth(counties, title = "2016 EDA Distress Map - Per Capita Money Income")


######################################################

# choropleth in ggplot
# create county and state polygon files

# load lat/long map data for county and state boundaries
county_map <- map_data("county")
state_map <- map_data("state")

# attach unemployment data and county fips code data
data(unemp)
data(county.fips)

# create states dataframe with state names and abbreviations
states <- data.frame(state.abb, state.name)
states$state.name <- tolower(state.name)

# merge states with county_map to create county_df
county_df <- merge(county_map, states, by.x = "region", by.y = "state.name")

# create a polyname variable in county_df to use for merge with county.fips
county_df$polyname <- str_c(county_df$region, county_df$subregion, sep = ",")

# merge county_df with county.fips based on polyname
county_df <- merge(county_df, county.fips, by = "polyname")


#####################################################


# merge in data of interest

# merge county_df with pc_inc_distress based on fips
# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/General Information/20170125/county_distress_map")

# load data on county distress 
counties <- read_csv("counties_20170131.csv")
counties <- data.frame(counties)

# create eda_distress flag for either criteria met
counties$eda_distress <- ifelse(counties$pc_inc_distress == 1 | counties$unemp_distress == 1, 1, 0)

# create region and value variables, which county_choropleth function uses
counties$region <- counties$fips_state_county
# remove leading zero on fips_state_county
for(i in 1:nrow(counties)) {
        if(str_sub(counties$region[i], 1, 1) == "0") {
                counties$region[i] <- str_sub(counties$region[i], 2, nchar(counties$region[i]))
        } 
}
counties$region <- as.numeric(counties$fips_state_county)

pc_inc_distress_df <- counties %>% select(region, pc_inc_distress)
names(pc_inc_distress_df)[1] <- "fips"

choropleth <- left_join(county_df, pc_inc_distress_df, by = "fips")

# choropleth <- left_join(choropleth, pc_inc_distress_df, by = "fips")


# order choropleth
choropleth <- choropleth[order(choropleth$order), ]

# select color palette using brewer.pal
# then use colorRampPalette to build a function "pal" which divides the palette by a given number of factors
# display.brewer.all()
colors <- brewer.pal(9, "Blues")
pal <- colorRampPalette(colors)
# pal is a function, which takes a number as it's argument eg. pal(14)
# to generalize: pal(length(unique(choropleth$rate_d1)))

# create levels for pc_inc_distress for use in legend names
choropleth <- choropleth %>% mutate(pc_inc_distress_fct = case_when(.$pc_inc_distress == "0" ~ "Not Distressed", .$pc_inc_distress == "1" ~ "Distressed"))
choropleth$pc_inc_distress_fct <- factor(choropleth$pc_inc_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$pc_inc_distress_fct2 <- factor(choropleth$pc_inc_distress_fct, levels = levels, ordered = TRUE)


####################################################


# create choropleth of all counties in US
# will take a minute to load 
choropleth_input <- choropleth
state_map_input <- state_map

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = pc_inc_distress_fct2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_manual(values = c("#3333ff", "#ff3300")) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
       axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold")) + 
        labs(x = "", y = "", title = "Eligibility Based on EDA Distress Criteria", fill = "Distress Indicator") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


##########################################################


# create choropleth for hawaii

# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/General Information/20170125/county_distress_map")

# load pre-created shape file with counties distress criteria already merged
choropleth <- read_csv("hawaii_shape.csv")
choropleth <- data.frame(choropleth)

# create eda_distress flag for either criteria met
choropleth$eda_distress <- ifelse(choropleth$pc_inc_distress == 1 | choropleth$unemp_distress == 1, 1, 0)

# order choropleth
choropleth <- choropleth[order(choropleth$order), ]

# create levels for pc_inc_distress for use in legend names
choropleth <- choropleth %>% mutate(pc_inc_distress_fct = case_when(.$pc_inc_distress == "0" ~ "Not Distressed", .$pc_inc_distress == "1" ~ "Distressed"))
choropleth$pc_inc_distress_fct <- factor(choropleth$pc_inc_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$pc_inc_distress_fct2 <- factor(choropleth$pc_inc_distress_fct, levels = levels, ordered = TRUE)

# create choropleth input
choropleth_input <- choropleth %>% filter(state == "HI")

# create choropleth 
ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = pc_inc_distress_fct2), colour = alpha("white", 1/2), size = 0.2) + 
        scale_fill_manual(values = c("#3333ff", "#ff3300")) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
         axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold")) + 
        labs(x = "", y = "", title = "Eligibility Based on EDA Distress Criteria", fill = "Distress Indicator") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


############################################################


# create choropleth for alaska

# setwd
setwd("G:/PNP/Performance Measurement/Data Calls/General Information/20170125/county_distress_map")

# load pre-created shape file with counties distress criteria already merged
choropleth <- read_csv("alaska_shape.csv")
choropleth <- data.frame(choropleth)

# create eda_distress flag for either criteria met
choropleth$eda_distress <- ifelse(choropleth$pc_inc_distress == 1 | choropleth$unemp_distress == 1, 1, 0)

# order choropleth
choropleth <- choropleth[order(choropleth$order), ]

# create levels for pc_inc_distress for use in legend names
choropleth <- choropleth %>% mutate(pc_inc_distress_fct = case_when(.$pc_inc_distress == "0" ~ "Not Distressed", .$pc_inc_distress == "1" ~ "Distressed"))
choropleth$pc_inc_distress_fct <- factor(choropleth$pc_inc_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$pc_inc_distress_fct2 <- factor(choropleth$pc_inc_distress_fct, levels = levels, ordered = TRUE)

# create choropleth input
choropleth_input <- choropleth %>% filter(state == "AK")

# create choropleth 
ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = pc_inc_distress_fct2), colour = alpha("white", 1/2), size = 0.2) + 
        scale_fill_manual(values = c("#3333ff", "#ff3300")) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                                                                                 panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
                                                                                 axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold")) + 
        labs(x = "", y = "", title = "Eligibility Based on EDA Distress Criteria", fill = "Distress Indicator") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


############################################################
