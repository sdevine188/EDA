library(readr)
library(ggplot2)
library(dplyr)
library(datasets)
library(maps)
library(stringr)
library(scales)
library(RColorBrewer)


# load us_county_shapefiles from tiger line file instead of base-R shapefiles

# setwd
setwd("C:/Users/Stephen/Desktop/R/EDA/distress")

# load lat/long map data for state boundaries - excluding alaska and hawaii
state_map <- read_csv("states_shapefiles_dataframe.csv")

# load us_county_shapefiles
county_df <- read_csv("us_counties_shapefiles_dataframe.csv")

# create fips variable for merge with counties data
county_df <- county_df %>% mutate(fips_state_county = str_c(fips_state, fips_county)) %>% data.frame(.)

#################################################


# create us choropleth
choropleth <- county_df

# exclude alaska and hawaii
head(choropleth)
choropleth <- choropleth %>% filter(!(fips_state %in% c("02", "15")))
state_map_input <- state_map %>% filter(id %in% unique(choropleth$fips_state)) 

# check that choropleth and state_map has same states
# length will differ due to an NA
length(unique(choropleth$state))
unique(choropleth$fips_state)
length(unique(choropleth$fips_state_county))

length(unique(state_map_input$id))
unique(state_map_input$id)

unique(choropleth$fips_state)[which(!(unique(choropleth$fips_state) %in% unique(state_map_input$id)))]
unique(state_map_input$id)[which(!(unique(state_map_input$id) %in% unique(choropleth$fips_state)))]

# order choropleth and state_map
choropleth <- choropleth[order(choropleth$order), ]
state_map_input <- state_map_input[order(state_map_input$order), ]

# test fill in data for 46102
# choropleth %>% filter(id == "46102") %>% select(id, eda_distress)
# choropleth <- choropleth %>% mutate(eda_distress = case_when(.$id == "46102" ~ 1, TRUE ~ .$eda_distress))
# choropleth %>% filter(id == "46102") %>% select(id, eda_distress)

# select color palette using brewer.pal
# then use colorRampPalette to build a function "pal" which divides the palette by a given number of factors
# display.brewer.all()
colors <- brewer.pal(9, "Blues")
pal <- colorRampPalette(colors)
# pal is a function, which takes a number as it's argument eg. pal(14)
# to generalize: pal(length(unique(choropleth$rate_d1)))

# create levels for eda_distress for use in legend names
choropleth <- choropleth %>% mutate(eda_distress_fct = case_when(.$eda_distress == "0" ~ "Not Distressed", .$eda_distress == "1" ~ "Distressed"))
choropleth$eda_distress_fct <- factor(choropleth$eda_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$eda_distress_fct2 <- factor(choropleth$eda_distress_fct, levels = levels, ordered = TRUE)


####################################################


# create choropleth of all counties in US
# will take a minute to load 
choropleth_input <- choropleth

ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = eda_distress_fct2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_manual(values = c("#3333ff", "#ff3300")) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
       panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
       axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold")) + 
        labs(x = "", y = "", title = "Eligibility Based on EDA Distress Criteria", fill = "Distress Indicator") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


##########################################################


# create choropleth for hawaii
choropleth <- county_df %>% filter(fips_state == "15") %>% data.frame(.)
state_map_input <- state_map %>% filter(id %in% unique(choropleth$fips_state)) 

# order choropleth and state_map_input
choropleth <- choropleth[order(choropleth$order), ]
state_map_input <- state_map_input[order(state_map_input$order), ]

# create levels for eda_distress for use in legend names
choropleth <- choropleth %>% mutate(eda_distress_fct = case_when(.$eda_distress == "0" ~ "Not Distressed", .$eda_distress == "1" ~ "Distressed"))
choropleth$eda_distress_fct <- factor(choropleth$eda_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$eda_distress_fct2 <- factor(choropleth$eda_distress_fct, levels = levels, ordered = TRUE)

# create choropleth input
choropleth_input <- choropleth

# create choropleth 
ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = eda_distress_fct2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_manual(values = c("#3333ff", "#ff3300")) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
         axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold")) + 
        labs(x = "", y = "", title = "Eligibility Based on EDA Distress Criteria", fill = "Distress Indicator") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


############################################################


# create choropleth for alaska
choropleth <- county_df %>% filter(fips_state == "02") %>% data.frame(.)
state_map_input <- state_map %>% filter(id %in% unique(choropleth$fips_state)) 

# order choropleth and state_map_input
choropleth <- choropleth[order(choropleth$order), ]
state_map_input <- state_map_input[order(state_map_input$order), ]

# create levels for eda_distress for use in legend names
choropleth <- choropleth %>% mutate(eda_distress_fct = case_when(.$eda_distress == "0" ~ "Not Distressed", .$eda_distress == "1" ~ "Distressed"))
choropleth$eda_distress_fct <- factor(choropleth$eda_distress_fct)
levels <- c("Not Distressed", "Distressed")
choropleth$eda_distress_fct2 <- factor(choropleth$eda_distress_fct, levels = levels, ordered = TRUE)

# create choropleth input
choropleth_input <- choropleth

# create choropleth 
ggplot(data = choropleth_input, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill = eda_distress_fct2), colour = alpha("white", 1/2), size = 0.2) + 
        geom_polygon(data = state_map_input, colour = "black", fill = NA) +
        scale_fill_manual(values = c("#3333ff", "#ff3300")) + theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                                                                                 panel.grid.minor = element_blank(), panel.border = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), 
                                                                                 axis.ticks.x = element_blank(), axis.text.x = element_blank(), plot.title = element_text(size = 20, face = "bold")) + 
        labs(x = "", y = "", title = "Eligibility Based on EDA Distress Criteria", fill = "Distress Indicator") + coord_fixed() + coord_map(project = "conic", lat0 = 30)


############################################################
