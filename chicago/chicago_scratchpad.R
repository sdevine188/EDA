data <- loadWorkbook(get("file"))
df <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
df <- df[-1, ]
for(i in 1:ncol(df)){
        names(df)[i] <- df[ 1, i]
}
df <- df[-1, ]

file <- list_files[1]
head(df)
file
sheet <- gsub(".xls", "", file)
sheet



counties <- loadWorkbook("counties.xlsx")
counties <- readWorksheet(counties, sheet = "Sheet1", header = TRUE)
head(counties)


names(df)
df$county_state <- 0
df$county_state <- tolower(gsub(" County", "", df$Name))
df$county_state <- tolower(paste(df$county_state, df$Postal, sep = ""))
rows <- which(df$county_state %in% counties$county_state)


df$county_state[rows]
counties$county_state

which(counties$county_state %in% df$county_state[rows])
il_rows <- which(df$Postal == "IL")
df$Name[il_rows]
in_rows <- which(df$Postal == "IN")
df$county_state[in_rows]

head(frame1)
frame1 <- df[rows, ]
frame2 <- frame1[ , c(1:5, 23, 32)]
names(frame1)
frame2 <- frame1[ , c(5, 23)]
names(frame2)

file1 <- list_files[1]
list_files1 <- list_files
ifelse(list_files1[2] == list_files[1], "match", "no match")
if(list_files1[2] == list_files[1]){
        print("match")
} else {
        print("no match")
}

head(df1)
frame <- data.frame(fill = c(1))
frame$fill <- rep(1, nrow(df1))
frame$fill <- c(1, 2, 3, 4, 5)
frame2 <- data.frame(fill = c(2, 3, 4))
frame <- rbind(frame, frame2)
frame
df$fill <- rep(1, nrows(data))
frame3 <- data.frame(fill = c(1:nrow(df1)))
frame3

list_files[2]

for(i in 1:length(list_files)){
        file <- list_files[i]
        print(file)
}

print(head(frame2))


### second data import

head(df)
tail(df)
str(df)
df_test <- df
rownames(df_test) <- NULL
head(df_test)

file <- list_files[1]
print(file)

data <- loadWorkbook(get("file"))
sheet <- gsub(".xlsx", "", file)
data <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
data <- data[-c(1:5), ]
data$fips <- paste(data$Col2, data$Col3, sep = "")
data <- data[ , 7:11]
year <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
names(data)[1] <- paste(year[i], "lf", sep = "")
names(data)[2] <- paste(year[i], "emp", sep = "")
names(data)[3] <- paste(year[i], "uemp", sep = "")
names(data)[4] <- paste(year[i], "ue_rate", sep = "")
df_test <- merge(data, df1, by = "fips")



head(data)
nrow(data)
head(df1)
nrow(df1)
head(df_test)
nrow(df_test)

## read in national ue data
df_test <- df2
head(df_test)
library(reshape2)


file <- list_files[9]
data <- loadWorkbook(get("file"))
sheet <- gsub(".xlsx", "", file)
data <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
data <- data[which(data$Year %in% c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")), ]
rownames(data) <- NULL
data$fill <- rep(1, 8)
data <- dcast(data, fill ~ Year, value.var = "Annual")
data <- data[ , -1]
names(data) <- c("2006_nat_ue", "2007_nat_ue", "2008_nat_ue", "2009_nat_ue", "2010_nat_ue", "2011_nat_ue", "2012_nat_ue", 
                 "2013_nat_ue")
for(i in 1:ncol(data)){
        data[ , i]
}

data_test <- data
data_test1 <- data.frame(c(rep(data_test[ , 1], 21)), (rep(data_test[ , 2], 21)), (rep(data_test[ , 3], 21)), 
                         (rep(data_test[ , 4], 21)), (rep(data_test[ , 5], 21)), (rep(data_test[ , 6], 21)), 
                         (rep(data_test[ , 7], 21)), (rep(data_test[ , 8], 21)))


names(data_test1) <- c("2006_nat_ue", "2007_nat_ue", "2008_nat_ue", "2009_nat_ue", "2010_nat_ue", "2011_nat_ue", "2012_nat_ue", 
                       "2013_nat_ue")
data_test2 <- cbind(data_test1, df1)


data_test1
head(data_test1)
nrow(data_test1)
names(data)[1] <- "2006"
head(data)
names(data)
str(data)
head(data_test)
head(data_test3)
str(data_test3)
nrow(df1)

head(data_test2)

## read in income data
file <- list_files[1]
data <- read.csv(get("file"))
head(data)
str(data)
names(data)
data <- data[ , -c(2:6, 8:44)]
rows <- which(data$GeoFIPS %in% df2$fips)
data <- data[rows, ]
unique(data$GeoFIPS)

head(data1)
head(data2)
data1 <- data[ , 1:4]
names(data1) <- c("GeoFIPS", "Description", "2006", "2007")
data1 <- melt(data1, id.vars = c("GeoFIPS", "Description"))
data1$new <- paste(data1$Description, data1$variable, sep = "_")
data1 <- data1[ , -c(2, 3)]

data2 <- dcast(data1, GeoFIPS ~ new)

##data2 <- reshape(data1, timevar = "variable")

names <- names(data2)
cols <- grep("thousands", names)
data2 <- data2[ , -cols]
nrow(data2)
inc <- data.frame()
inc <- income


head(inc)
names(inc)
names <- names(inc)
pcip_names_col <- grep("Per capita", names)
pop_names_col <- grep("Population", names)
pcip_names <- names(inc)[pcip_names_col]
pcip_new_names <- gsub("Per capita personal income (dollars)", "pcip_", pcip_names)
names(inc)[pcip_names]

## try simpler
names(data) <- ""

## read in and merge national income data
file <- list_files[4]
data <- read.csv(get("file"))
data <- data[c(1, 43) , -10]


data1 <- melt(data, id.vars = "Description")
data1$new <- paste(data1$Description, data1$variable, sep = "_")
data1$fill <- 1
data1 <- data1[ , -c(1, 2)]
data1 <- dcast(data1, fill ~ new)
names(data1) <- c("2006_natpop", "2007_natpop", "2008_natpop", "2009_natpop", "2010_natpop", "2011_natpop", "2012_natpop", 
                  "2013_natpop", "2006_natinc", "2007_natinc", "2008_natinc", "2009_natinc", "2010_natinc", "2011_natinc",
                  "2012_natinc", "2013_natinc")

df_test <- df2


str(data1)
head(data)
head(data2)
head(data1)

## create real income variables
data <- df5
head(data)
data$real_pcnatinc_2006 <- (data$pcnatinc_2006 / data$cpi_2006) * data$cpi_2013
data$sum <- 0
data$sum <- as.numeric(as.character(data$2011_cpi) + as.numeric(as.character(data$2011_pop)
str(data)

data$new <- data$cpi_2012 + as.numeric(as.character(data$pop_2013))

data1 <- data
names(data1)[90:95] <- c("t1", "t2", "t3", "t4", "t5", "t6")
head(data1)
data1$t6 <- data1$t4 + data1$t5
data1$t6 <- data1$2007_cpi + data1$2008_cpi

## create separate data frame with tri-state population-weighted averages and national averages
ue_rate
median
pcpi
poverty

data <- df6

## convert character variables into numeric
data$pop_2006 <- as.numeric(as.character(data$pop_2006))
data$pop_2007 <- as.numeric(as.character(data$pop_2007))
data$pop_2008 <- as.numeric(as.character(data$pop_2008))
data$pop_2009 <- as.numeric(as.character(data$pop_2009))
data$pop_2010 <- as.numeric(as.character(data$pop_2010))
data$pop_2011 <- as.numeric(as.character(data$pop_2011))
data$pop_2012 <- as.numeric(as.character(data$pop_2012))
data$pop_2013 <- as.numeric(as.character(data$pop_2013))

data$ue_rate_2006 <- as.numeric(as.character(data$ue_rate_2006))
data$ue_rate_2007 <- as.numeric(as.character(data$ue_rate_2007))
data$ue_rate_2008 <- as.numeric(as.character(data$ue_rate_2008))
data$ue_rate_2009 <- as.numeric(as.character(data$ue_rate_2009))
data$ue_rate_2010 <- as.numeric(as.character(data$ue_rate_2010))
data$ue_rate_2011 <- as.numeric(as.character(data$ue_rate_2011))
data$ue_rate_2012 <- as.numeric(as.character(data$ue_rate_2012))
data$ue_rate_2013 <- as.numeric(as.character(data$ue_rate_2013))

data$poverty_2006 <- as.numeric(as.character(data$poverty_2006))
data$poverty_2007 <- as.numeric(as.character(data$poverty_2007))
data$poverty_2008 <- as.numeric(as.character(data$poverty_2008))
data$poverty_2009 <- as.numeric(as.character(data$poverty_2009))
data$poverty_2010 <- as.numeric(as.character(data$poverty_2010))
data$poverty_2011 <- as.numeric(as.character(data$poverty_2011))
data$poverty_2012 <- as.numeric(as.character(data$poverty_2012))
data$poverty_2013 <- as.numeric(as.character(data$poverty_2013))

data$median_2006 <- as.numeric(as.character(data$median_2006))
data$median_2007 <- as.numeric(as.character(data$median_2007))
data$median_2008 <- as.numeric(as.character(data$median_2008))
data$median_2009 <- as.numeric(as.character(data$median_2009))
data$median_2010 <- as.numeric(as.character(data$median_2010))
data$median_2011 <- as.numeric(as.character(data$median_2011))
data$median_2012 <- as.numeric(as.character(data$median_2012))
data$median_2013 <- as.numeric(as.character(data$median_2013))

data$pcpi_2006 <- as.numeric(as.character(data$pcpi_2006))
data$pcpi_2007 <- as.numeric(as.character(data$pcpi_2007))
data$pcpi_2008 <- as.numeric(as.character(data$pcpi_2008))
data$pcpi_2009 <- as.numeric(as.character(data$pcpi_2009))
data$pcpi_2010 <- as.numeric(as.character(data$pcpi_2010))
data$pcpi_2011 <- as.numeric(as.character(data$pcpi_2011))
data$pcpi_2012 <- as.numeric(as.character(data$pcpi_2012))
data$pcpi_2013 <- as.numeric(as.character(data$pcpi_2013))

## create weighted values

data$wt_uerate_2006 <- data$ue_rate_2006 * data$pop_2006
data$wt_uerate_2007 <- data$ue_rate_2007 * data$pop_2007
data$wt_uerate_2008 <- data$ue_rate_2008 * data$pop_2008
data$wt_uerate_2009 <- data$ue_rate_2009 * data$pop_2009
data$wt_uerate_2010 <- data$ue_rate_2010 * data$pop_2010
data$wt_uerate_2011 <- data$ue_rate_2011 * data$pop_2011
data$wt_uerate_2012 <- data$ue_rate_2012 * data$pop_2012
data$wt_uerate_2013 <- data$ue_rate_2013 * data$pop_2013

data$wt_median_2006 <- data$median_2006 * data$pop_2006
data$wt_median_2007 <- data$median_2007 * data$pop_2007
data$wt_median_2008 <- data$median_2008 * data$pop_2008
data$wt_median_2009 <- data$median_2009 * data$pop_2009
data$wt_median_2010 <- data$median_2010 * data$pop_2010
data$wt_median_2011 <- data$median_2011 * data$pop_2011
data$wt_median_2012 <- data$median_2012 * data$pop_2012
data$wt_median_2013 <- data$median_2013 * data$pop_2013

data$wt_pcpi_2006 <- data$pcpi_2006 * data$pop_2006
data$wt_pcpi_2007 <- data$pcpi_2007 * data$pop_2007
data$wt_pcpi_2008 <- data$pcpi_2008 * data$pop_2008
data$wt_pcpi_2009 <- data$pcpi_2009 * data$pop_2009
data$wt_pcpi_2010 <- data$pcpi_2010 * data$pop_2010
data$wt_pcpi_2011 <- data$pcpi_2011 * data$pop_2011
data$wt_pcpi_2012 <- data$pcpi_2012 * data$pop_2012
data$wt_pcpi_2013 <- data$pcpi_2013 * data$pop_2013

data$wt_poverty_2006 <- data$poverty_2006 * data$pop_2006
data$wt_poverty_2007 <- data$poverty_2007 * data$pop_2007
data$wt_poverty_2008 <- data$poverty_2008 * data$pop_2008
data$wt_poverty_2009 <- data$poverty_2009 * data$pop_2009
data$wt_poverty_2010 <- data$poverty_2010 * data$pop_2010
data$wt_poverty_2011 <- data$poverty_2011 * data$pop_2011
data$wt_poverty_2012 <- data$poverty_2012 * data$pop_2012
data$wt_poverty_2013 <- data$poverty_2013 * data$pop_2013

## create separate dataframe with weighted local aggregates and national values
data <- df7

agg <- data.frame(fill = c(1))

agg$poverty_2006 <- sum(data$wt_poverty_2006) / sum(data$pop_2006)
agg$poverty_2007 <- sum(data$wt_poverty_2007) / sum(data$pop_2007)
agg$poverty_2008 <- sum(data$wt_poverty_2008) / sum(data$pop_2008)
agg$poverty_2009 <- sum(data$wt_poverty_2009) / sum(data$pop_2009)
agg$poverty_2010 <- sum(data$wt_poverty_2010) / sum(data$pop_2010)
agg$poverty_2011 <- sum(data$wt_poverty_2011) / sum(data$pop_2011)
agg$poverty_2012 <- sum(data$wt_poverty_2012) / sum(data$pop_2012)
agg$poverty_2013 <- sum(data$wt_poverty_2013) / sum(data$pop_2013)

agg$uerate_2006 <- sum(data$wt_uerate_2006) / sum(data$pop_2006)
agg$uerate_2007 <- sum(data$wt_uerate_2007) / sum(data$pop_2006)
agg$uerate_2008 <- sum(data$wt_uerate_2008) / sum(data$pop_2006)
agg$uerate_2009 <- sum(data$wt_uerate_2009) / sum(data$pop_2006)
agg$uerate_2010 <- sum(data$wt_uerate_2010) / sum(data$pop_2006)
agg$uerate_2011 <- sum(data$wt_uerate_2011) / sum(data$pop_2006)
agg$uerate_2012 <- sum(data$wt_uerate_2012) / sum(data$pop_2006)
agg$uerate_2013 <- sum(data$wt_uerate_2013) / sum(data$pop_2006)

agg$median_2006 <- sum(data$wt_median_2006) / sum(data$pop_2006)
agg$median_2007 <- sum(data$wt_median_2007) / sum(data$pop_2006)
agg$median_2008 <- sum(data$wt_median_2008) / sum(data$pop_2006)
agg$median_2009 <- sum(data$wt_median_2009) / sum(data$pop_2006)
agg$median_2010 <- sum(data$wt_median_2010) / sum(data$pop_2006)
agg$median_2011 <- sum(data$wt_median_2011) / sum(data$pop_2006)
agg$median_2012 <- sum(data$wt_median_2012) / sum(data$pop_2006)
agg$median_2013 <- sum(data$wt_median_2013) / sum(data$pop_2006)

agg$pcpi_2006 <- sum(data$wt_pcpi_2006) / sum(data$pop_2006)
agg$pcpi_2007 <- sum(data$wt_pcpi_2007) / sum(data$pop_2006)
agg$pcpi_2008 <- sum(data$wt_pcpi_2008) / sum(data$pop_2006)
agg$pcpi_2009 <- sum(data$wt_pcpi_2009) / sum(data$pop_2006)
agg$pcpi_2010 <- sum(data$wt_pcpi_2010) / sum(data$pop_2006)
agg$pcpi_2011 <- sum(data$wt_pcpi_2011) / sum(data$pop_2006)
agg$pcpi_2012 <- sum(data$wt_pcpi_2012) / sum(data$pop_2006)
agg$pcpi_2013 <- sum(data$wt_pcpi_2013) / sum(data$pop_2006)

agg2 <- data.frame(fill = c(1))

agg2$median_2006 <- data$real_natmedian_2006[1]
agg2$median_2007 <- data$real_natmedian_2007[1]
agg2$median_2008 <- data$real_natmedian_2008[1]
agg2$median_2009 <- data$real_natmedian_2009[1]
agg2$median_2010 <- data$real_natmedian_2010[1]
agg2$median_2011 <- data$real_natmedian_2011[1]
agg2$median_2012 <- data$real_natmedian_2012[1]
agg2$median_2013 <- data$real_natmedian_2013[1]

agg2$poverty_2006 <- data$nat_poverty_2006[1]
agg2$poverty_2007 <- data$nat_poverty_2007[1]
agg2$poverty_2008 <- data$nat_poverty_2008[1]
agg2$poverty_2009 <- data$nat_poverty_2009[1]
agg2$poverty_2010 <- data$nat_poverty_2010[1]
agg2$poverty_2011 <- data$nat_poverty_2011[1]
agg2$poverty_2012 <- data$nat_poverty_2012[1]
agg2$poverty_2013 <- data$nat_poverty_2013[1]

agg2$uerate_2006 <- data$nat_ue_2006[1]
agg2$uerate_2007 <- data$nat_ue_2007[1]
agg2$uerate_2008 <- data$nat_ue_2008[1]
agg2$uerate_2009 <- data$nat_ue_2009[1]
agg2$uerate_2010 <- data$nat_ue_2010[1]
agg2$uerate_2011 <- data$nat_ue_2011[1]
agg2$uerate_2012 <- data$nat_ue_2012[1]
agg2$uerate_2013 <- data$nat_ue_2013[1]

agg2$pcpi_2006 <- data$real_pcnatinc_2006[1]
agg2$pcpi_2007 <- data$real_pcnatinc_2007[1]
agg2$pcpi_2008 <- data$real_pcnatinc_2008[1]
agg2$pcpi_2009 <- data$real_pcnatinc_2009[1]
agg2$pcpi_2010 <- data$real_pcnatinc_2010[1]
agg2$pcpi_2011 <- data$real_pcnatinc_2011[1]
agg2$pcpi_2012 <- data$real_pcnatinc_2012[1]
agg2$pcpi_2013 <- data$real_pcnatinc_2013[1]

agg3 <- rbind(agg, agg2)
agg3[ , 1] <- c("local", "national")
names(agg3)[1] <- "location"

## exploratory analysis
head(df7)
df7$real_median_2006
names(df7)
median_eda <- df7[ , c(87, 127, 134, 135, 142)]
write.csv(median_eda, "median_eda.csv")
df7$cpi_2006
df7$cpi_2013

magg3 <- melt(agg3, id.vars = ("location"))
head(magg3)
str(magg3)
magg3$location <- as.factor(magg3$location)

library(ggplot2)
pcip_agg <- magg3[grep("pcpi", magg3$variable), ]
ggplot(pcip_agg, aes(x = variable, y = value, fill = location)) + geom_bar(stat = "identity", position = position_dodge())
## graph is easier to prettify in excel
lq <- data.frame()

ggplot()


ggplot(crime, aes(x=murder, y=burglary, size=population, label=state),guide=FALSE)+
        geom_point(colour="white", fill="red", shape=21)+ scale_size_area(max_size = 15)+
        scale_x_continuous(name="Murders per 1,000 population", limits=c(0,12))+
        scale_y_continuous(name="Burglaries per 1,000 population", limits=c(0,1250))+
        geom_text(size=4)+
        theme_bw()