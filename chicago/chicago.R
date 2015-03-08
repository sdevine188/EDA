library(XLConnect)
library(stringr)

## set beginning working directory
setwd("C:/Users/Steve/Desktop/R/chicago")

counties <- loadWorkbook("counties.xlsx")
counties <- readWorksheet(counties, sheet = "Sheet1", header = TRUE)

## import poverty_median data for tri-state counties, and append to base data frame
setwd("C:/Users/Steve/Desktop/R/chicago/poverty_median")
list_files <- list.files()
df <- data.frame(fill = c(1:21))

for(i in 1:length(list_files)){
        file <- list_files[i]
        print(file)
        data <- loadWorkbook(get("file"))
        sheet <- gsub(".xls", "", file)
        data <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
        data <- data[-1, ]
        for(x in 1:ncol(data)){
                names(data)[x] <- data[ 1, x]
        }
        data <- data[-1, ]
        data$county_state <- 0
        data$county_state <- tolower(gsub(" County", "", data$Name))
        data$county_state <- tolower(paste(data$county_state, data$Postal, sep = ""))
        rows <- which(data$county_state %in% counties$county_state)
        data <- data[rows, ]
        if(list_files[i] == list_files[1]){
                data <- data[ , c(1:4, 8, 23, 32)]
                names(data) <- c("state_fips", "county_fips", "state_abb", "county_name", "poverty_2006", 
                                 "median_2006", "county_state")
                df <- cbind(df, data)
        } else {
                data <- data[ , c(8, 23)]
                year <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
                names(data)[1] <- paste("poverty", year[i], sep = "_")
                names(data)[2] <- paste("median", year[i], sep = "_")
                df <- cbind(df, data)
        }
}

rownames(df) <- NULL
df$fips <- paste(df$state_fips, df$county_fips, sep = "")
df <- df[ , -1]

df1 <- df

## import poverty_median data for nation, and append to base data frame
setwd("C:/Users/Steve/Desktop/R/chicago/poverty_median")
list_files <- list.files()
df <- data.frame(fill = c(1))

for(i in 1:length(list_files)){
        file <- list_files[i]
        print(file)
        data <- loadWorkbook(get("file"))
        sheet <- gsub(".xls", "", file)
        data <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
        data <- data[-1, ]
        for(x in 1:ncol(data)){
                names(data)[x] <- data[ 1, x]
        }
        data <- data[-1, ]
        data <- data[ 1, c(8, 23)]
        year <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
        names(data)[1] <- paste("nat_poverty", year[i], sep = "_")
        names(data)[2] <- paste("nat_median", year[i], sep = "_")
        df <- cbind(df, data)
}

df <- df[ , -1]
df_test <- data.frame(fill = c(1:21))

for(i in 1:ncol(df)){
         value <- df[1, i]
         rep_value <- rep(value, 21)
         df_test[ , i+1] <- rep_value 
}

df_test <- df_test[ , -1]
names(df_test) <- names(df)
df1 <- cbind(df1, df)

## import and merge lau data
setwd("C:/Users/Steve/Desktop/R/chicago/unemployment")
list_files <- list.files()

for(i in 1:8){
        file <- list_files[i]
        print(file)
        data <- loadWorkbook(get("file"))
        sheet <- gsub(".xlsx", "", file)
        data <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
        data <- data[-c(1:5), ]
        data$fips <- paste(data$Col2, data$Col3, sep = "")
        data <- data[ , 7:11]
        year <- c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")
        names(data)[1] <- paste("lf", year[i], sep = "_")
        names(data)[2] <- paste("emp", year[i], sep = "_")
        names(data)[3] <- paste("uemp", year[i],  sep = "_")
        names(data)[4] <- paste("ue_rate", year[i], sep = "_")
        df1 <- merge(data, df1, by = "fips")
}

## import and cbind national unemployment data
file <- list_files[9]
data <- loadWorkbook(get("file"))
sheet <- gsub(".xlsx", "", file)
data <- readWorksheet(data, sheet = get("sheet"), header = TRUE)
data <- data[which(data$Year %in% c("2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013")), ]
data$fill <- rep(1, 8)
data <- dcast(data, fill ~ Year, value.var = "Annual")
data <- data[ , -1]
data <- data.frame(c(rep(data[ , 1], 21)), (rep(data[ , 2], 21)), (rep(data[ , 3], 21)), 
                   (rep(data[ , 4], 21)), (rep(data[ , 5], 21)), (rep(data[ , 6], 21)), 
                   (rep(data[ , 7], 21)), (rep(data[ , 8], 21)))
names(data) <- c("nat_ue_2006", "nat_ue_2007", "nat_ue_2008", "nat_ue_2009", "nat_ue_2010", "nat_ue_2011", 
                 "nat_ue_2012", "nat_ue_2013")
df1 <- cbind(data, df1)

df2 <- df1

## import and merge local income data
setwd("C:/Users/Steve/Desktop/R/chicago/income")
list_files <- list.files()
income <- data.frame()

for(i in 1:3){
        file <- list_files[i]
        print(file)
        data <- read.csv(get("file"))
        data <- data[ , -c(2:6, 8:44)]
        rows <- which(data$GeoFIPS %in% df2$fips)
        data <- data[rows, ]
        data <- melt(data, id.vars = c("GeoFIPS", "Description"))
        data$new <- paste(data$Description, data$variable, sep = "_")
        data <- data[ , -c(2, 3)]
        data <- dcast(data, GeoFIPS ~ new)
        names <- names(data)
        cols <- grep("thousands", names)
        data <- data[ , -cols]
        names(data) <- c("fips", "pcpi_2006", "pcpi_2007","pcpi_2008", "pcpi_2009","pcpi_2010", "pcpi_2011",
                         "pcpi_2012", "pcpi_2013", "pop_2006", "pop_2007", "pop_2008", "pop_2009", "pop_2010", "pop_2011",
                         "pop_2012", "pop_2013")    
        if(file == list_files[1]){
                income <- data
        } else {
                income <- rbind(income, data)
        }
}

df2 <- merge(df2, income, by = "fips")
df3 <- df2

## import and merge national income data
setwd("C:/Users/Steve/Desktop/R/chicago/income")
file <- list_files[5]
data <- read.csv(get("file"))
data <- data[c(1, 43) , -10]
write.csv(data, "nat_inc_raw.csv")

## manually used the nat_inc_raw.csv to calculate the clean nat_inc.csv, which has pcnatinc = natinc / natpop
setwd("C:/Users/Steve/Desktop/R/chicago/originals_overwritten")
nat_inc <- read.csv("nat_inc.csv")
names(nat_inc) <- c("pcnatinc_2006", "pcnatinc_2007", "pcnatinc_2008", "pcnatinc_2009", "pcnatinc_2010", "pcnatinc_2011", 
                    "pcnatinc_2012", "pcnatinc_2013")

df3 <- cbind(df3, nat_inc) 
df4 <- df3

## import and merge cpi data
setwd("C:/Users/Steve/Desktop/R/chicago/cpi")

cpi <- read.csv("bls_cpi_clean.csv")
names(cpi) <- c("cpi_2006", "cpi_2007", "cpi_2008", "cpi_2009", "cpi_2010", "cpi_2011", 
                "cpi_2012", "cpi_2013")
df4 <- cbind(df4, cpi)

df5 <- df4

## create real income variables

df5$real_pcnatinc_2006 <- (df5$pcnatinc_2006 / df5$cpi_2006) * df5$cpi_2013
df5$real_pcnatinc_2007 <- (df5$pcnatinc_2007 / df5$cpi_2007) * df5$cpi_2013
df5$real_pcnatinc_2008 <- (df5$pcnatinc_2008 / df5$cpi_2008) * df5$cpi_2013
df5$real_pcnatinc_2009 <- (df5$pcnatinc_2009 / df5$cpi_2009) * df5$cpi_2013
df5$real_pcnatinc_2010 <- (df5$pcnatinc_2010 / df5$cpi_2010) * df5$cpi_2013
df5$real_pcnatinc_2011 <- (df5$pcnatinc_2011 / df5$cpi_2011) * df5$cpi_2013
df5$real_pcnatinc_2012 <- (df5$pcnatinc_2012 / df5$cpi_2012) * df5$cpi_2013
df5$real_pcnatinc_2013 <- (df5$pcnatinc_2013 / df5$cpi_2013) * df5$cpi_2013

df5$real_pcpi_2006 <- (as.numeric(as.character(df5$pcpi_2006)) / df5$cpi_2006) * df5$cpi_2013
df5$real_pcpi_2007 <- (as.numeric(as.character(df5$pcpi_2007)) / df5$cpi_2007) * df5$cpi_2013
df5$real_pcpi_2008 <- (as.numeric(as.character(df5$pcpi_2008)) / df5$cpi_2008) * df5$cpi_2013
df5$real_pcpi_2009 <- (as.numeric(as.character(df5$pcpi_2009)) / df5$cpi_2009) * df5$cpi_2013
df5$real_pcpi_2010 <- (as.numeric(as.character(df5$pcpi_2010)) / df5$cpi_2010) * df5$cpi_2013
df5$real_pcpi_2011 <- (as.numeric(as.character(df5$pcpi_2011)) / df5$cpi_2011) * df5$cpi_2013
df5$real_pcpi_2012 <- (as.numeric(as.character(df5$pcpi_2012)) / df5$cpi_2012) * df5$cpi_2013
df5$real_pcpi_2013 <- (as.numeric(as.character(df5$pcpi_2013)) / df5$cpi_2013) * df5$cpi_2013

df5$real_median_2006 <- (as.numeric(as.character(df5$median_2006)) / df5$cpi_2006) * df5$cpi_2013
df5$real_median_2007 <- (as.numeric(as.character(df5$median_2007)) / df5$cpi_2007) * df5$cpi_2013
df5$real_median_2008 <- (as.numeric(as.character(df5$median_2008)) / df5$cpi_2008) * df5$cpi_2013
df5$real_median_2009 <- (as.numeric(as.character(df5$median_2009)) / df5$cpi_2009) * df5$cpi_2013
df5$real_median_2010 <- (as.numeric(as.character(df5$median_2010)) / df5$cpi_2010) * df5$cpi_2013
df5$real_median_2011 <- (as.numeric(as.character(df5$median_2011)) / df5$cpi_2011) * df5$cpi_2013
df5$real_median_2012 <- (as.numeric(as.character(df5$median_2012)) / df5$cpi_2012) * df5$cpi_2013
df5$real_median_2013 <- (as.numeric(as.character(df5$median_2013)) / df5$cpi_2013) * df5$cpi_2013

df5$real_natmedian_2006 <- (as.numeric(as.character(df5$nat_median_2006)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2007 <- (as.numeric(as.character(df5$nat_median_2007)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2008 <- (as.numeric(as.character(df5$nat_median_2008)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2009 <- (as.numeric(as.character(df5$nat_median_2009)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2010 <- (as.numeric(as.character(df5$nat_median_2010)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2011 <- (as.numeric(as.character(df5$nat_median_2011)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2012 <- (as.numeric(as.character(df5$nat_median_2012)) / df5$cpi_2006) * df5$cpi_2013
df5$real_natmedian_2013 <- (as.numeric(as.character(df5$nat_median_2013)) / df5$cpi_2006) * df5$cpi_2013

df6 <- df5

## prepare to create separate data frame with tri-state population-weighted averages and national averages

## convert character variables into numeric
df6$pop_2006 <- as.numeric(as.character(df6$pop_2006))
df6$pop_2007 <- as.numeric(as.character(df6$pop_2007))
df6$pop_2008 <- as.numeric(as.character(df6$pop_2008))
df6$pop_2009 <- as.numeric(as.character(df6$pop_2009))
df6$pop_2010 <- as.numeric(as.character(df6$pop_2010))
df6$pop_2011 <- as.numeric(as.character(df6$pop_2011))
df6$pop_2012 <- as.numeric(as.character(df6$pop_2012))
df6$pop_2013 <- as.numeric(as.character(df6$pop_2013))

df6$ue_rate_2006 <- as.numeric(as.character(df6$ue_rate_2006))
df6$ue_rate_2007 <- as.numeric(as.character(df6$ue_rate_2007))
df6$ue_rate_2008 <- as.numeric(as.character(df6$ue_rate_2008))
df6$ue_rate_2009 <- as.numeric(as.character(df6$ue_rate_2009))
df6$ue_rate_2010 <- as.numeric(as.character(df6$ue_rate_2010))
df6$ue_rate_2011 <- as.numeric(as.character(df6$ue_rate_2011))
df6$ue_rate_2012 <- as.numeric(as.character(df6$ue_rate_2012))
df6$ue_rate_2013 <- as.numeric(as.character(df6$ue_rate_2013))

df6$poverty_2006 <- as.numeric(as.character(df6$poverty_2006))
df6$poverty_2007 <- as.numeric(as.character(df6$poverty_2007))
df6$poverty_2008 <- as.numeric(as.character(df6$poverty_2008))
df6$poverty_2009 <- as.numeric(as.character(df6$poverty_2009))
df6$poverty_2010 <- as.numeric(as.character(df6$poverty_2010))
df6$poverty_2011 <- as.numeric(as.character(df6$poverty_2011))
df6$poverty_2012 <- as.numeric(as.character(df6$poverty_2012))
df6$poverty_2013 <- as.numeric(as.character(df6$poverty_2013))

df6$median_2006 <- as.numeric(as.character(df6$median_2006))
df6$median_2007 <- as.numeric(as.character(df6$median_2007))
df6$median_2008 <- as.numeric(as.character(df6$median_2008))
df6$median_2009 <- as.numeric(as.character(df6$median_2009))
df6$median_2010 <- as.numeric(as.character(df6$median_2010))
df6$median_2011 <- as.numeric(as.character(df6$median_2011))
df6$median_2012 <- as.numeric(as.character(df6$median_2012))
df6$median_2013 <- as.numeric(as.character(df6$median_2013))

df6$pcpi_2006 <- as.numeric(as.character(df6$pcpi_2006))
df6$pcpi_2007 <- as.numeric(as.character(df6$pcpi_2007))
df6$pcpi_2008 <- as.numeric(as.character(df6$pcpi_2008))
df6$pcpi_2009 <- as.numeric(as.character(df6$pcpi_2009))
df6$pcpi_2010 <- as.numeric(as.character(df6$pcpi_2010))
df6$pcpi_2011 <- as.numeric(as.character(df6$pcpi_2011))
df6$pcpi_2012 <- as.numeric(as.character(df6$pcpi_2012))
df6$pcpi_2013 <- as.numeric(as.character(df6$pcpi_2013))

## create weighted values

df6$wt_uerate_2006 <- df6$ue_rate_2006 * df6$pop_2006
df6$wt_uerate_2007 <- df6$ue_rate_2007 * df6$pop_2007
df6$wt_uerate_2008 <- df6$ue_rate_2008 * df6$pop_2008
df6$wt_uerate_2009 <- df6$ue_rate_2009 * df6$pop_2009
df6$wt_uerate_2010 <- df6$ue_rate_2010 * df6$pop_2010
df6$wt_uerate_2011 <- df6$ue_rate_2011 * df6$pop_2011
df6$wt_uerate_2012 <- df6$ue_rate_2012 * df6$pop_2012
df6$wt_uerate_2013 <- df6$ue_rate_2013 * df6$pop_2013

df6$wt_median_2006 <- df6$real_median_2006 * df6$pop_2006
df6$wt_median_2007 <- df6$real_median_2007 * df6$pop_2007
df6$wt_median_2008 <- df6$real_median_2008 * df6$pop_2008
df6$wt_median_2009 <- df6$real_median_2009 * df6$pop_2009
df6$wt_median_2010 <- df6$real_median_2010 * df6$pop_2010
df6$wt_median_2011 <- df6$real_median_2011 * df6$pop_2011
df6$wt_median_2012 <- df6$real_median_2012 * df6$pop_2012
df6$wt_median_2013 <- df6$real_median_2013 * df6$pop_2013

df6$wt_pcpi_2006 <- df6$real_pcpi_2006 * df6$pop_2006
df6$wt_pcpi_2007 <- df6$real_pcpi_2007 * df6$pop_2007
df6$wt_pcpi_2008 <- df6$real_pcpi_2008 * df6$pop_2008
df6$wt_pcpi_2009 <- df6$real_pcpi_2009 * df6$pop_2009
df6$wt_pcpi_2010 <- df6$real_pcpi_2010 * df6$pop_2010
df6$wt_pcpi_2011 <- df6$real_pcpi_2011 * df6$pop_2011
df6$wt_pcpi_2012 <- df6$real_pcpi_2012 * df6$pop_2012
df6$wt_pcpi_2013 <- df6$real_pcpi_2013 * df6$pop_2013

df6$wt_poverty_2006 <- df6$poverty_2006 * df6$pop_2006
df6$wt_poverty_2007 <- df6$poverty_2007 * df6$pop_2007
df6$wt_poverty_2008 <- df6$poverty_2008 * df6$pop_2008
df6$wt_poverty_2009 <- df6$poverty_2009 * df6$pop_2009
df6$wt_poverty_2010 <- df6$poverty_2010 * df6$pop_2010
df6$wt_poverty_2011 <- df6$poverty_2011 * df6$pop_2011
df6$wt_poverty_2012 <- df6$poverty_2012 * df6$pop_2012
df6$wt_poverty_2013 <- df6$poverty_2013 * df6$pop_2013

df7 <- df6

## create separate data frame with tri-state population-weighted averages and national averages

agg <- df7.frame(fill = c(1))

agg$poverty_2006 <- sum(df7$wt_poverty_2006) / sum(df7$pop_2006)
agg$poverty_2007 <- sum(df7$wt_poverty_2007) / sum(df7$pop_2007)
agg$poverty_2008 <- sum(df7$wt_poverty_2008) / sum(df7$pop_2008)
agg$poverty_2009 <- sum(df7$wt_poverty_2009) / sum(df7$pop_2009)
agg$poverty_2010 <- sum(df7$wt_poverty_2010) / sum(df7$pop_2010)
agg$poverty_2011 <- sum(df7$wt_poverty_2011) / sum(df7$pop_2011)
agg$poverty_2012 <- sum(df7$wt_poverty_2012) / sum(df7$pop_2012)
agg$poverty_2013 <- sum(df7$wt_poverty_2013) / sum(df7$pop_2013)

agg$uerate_2006 <- sum(df7$wt_uerate_2006) / sum(df7$pop_2006)
agg$uerate_2007 <- sum(df7$wt_uerate_2007) / sum(df7$pop_2006)
agg$uerate_2008 <- sum(df7$wt_uerate_2008) / sum(df7$pop_2006)
agg$uerate_2009 <- sum(df7$wt_uerate_2009) / sum(df7$pop_2006)
agg$uerate_2010 <- sum(df7$wt_uerate_2010) / sum(df7$pop_2006)
agg$uerate_2011 <- sum(df7$wt_uerate_2011) / sum(df7$pop_2006)
agg$uerate_2012 <- sum(df7$wt_uerate_2012) / sum(df7$pop_2006)
agg$uerate_2013 <- sum(df7$wt_uerate_2013) / sum(df7$pop_2006)

agg$median_2006 <- sum(df7$wt_median_2006) / sum(df7$pop_2006)
agg$median_2007 <- sum(df7$wt_median_2007) / sum(df7$pop_2006)
agg$median_2008 <- sum(df7$wt_median_2008) / sum(df7$pop_2006)
agg$median_2009 <- sum(df7$wt_median_2009) / sum(df7$pop_2006)
agg$median_2010 <- sum(df7$wt_median_2010) / sum(df7$pop_2006)
agg$median_2011 <- sum(df7$wt_median_2011) / sum(df7$pop_2006)
agg$median_2012 <- sum(df7$wt_median_2012) / sum(df7$pop_2006)
agg$median_2013 <- sum(df7$wt_median_2013) / sum(df7$pop_2006)

agg$pcpi_2006 <- sum(df7$wt_pcpi_2006) / sum(df7$pop_2006)
agg$pcpi_2007 <- sum(df7$wt_pcpi_2007) / sum(df7$pop_2006)
agg$pcpi_2008 <- sum(df7$wt_pcpi_2008) / sum(df7$pop_2006)
agg$pcpi_2009 <- sum(df7$wt_pcpi_2009) / sum(df7$pop_2006)
agg$pcpi_2010 <- sum(df7$wt_pcpi_2010) / sum(df7$pop_2006)
agg$pcpi_2011 <- sum(df7$wt_pcpi_2011) / sum(df7$pop_2006)
agg$pcpi_2012 <- sum(df7$wt_pcpi_2012) / sum(df7$pop_2006)
agg$pcpi_2013 <- sum(df7$wt_pcpi_2013) / sum(df7$pop_2006)

agg2 <- df7.frame(fill = c(1))

agg2$median_2006 <- df7$real_natmedian_2006[1]
agg2$median_2007 <- df7$real_natmedian_2007[1]
agg2$median_2008 <- df7$real_natmedian_2008[1]
agg2$median_2009 <- df7$real_natmedian_2009[1]
agg2$median_2010 <- df7$real_natmedian_2010[1]
agg2$median_2011 <- df7$real_natmedian_2011[1]
agg2$median_2012 <- df7$real_natmedian_2012[1]
agg2$median_2013 <- df7$real_natmedian_2013[1]

agg2$poverty_2006 <- df7$nat_poverty_2006[1]
agg2$poverty_2007 <- df7$nat_poverty_2007[1]
agg2$poverty_2008 <- df7$nat_poverty_2008[1]
agg2$poverty_2009 <- df7$nat_poverty_2009[1]
agg2$poverty_2010 <- df7$nat_poverty_2010[1]
agg2$poverty_2011 <- df7$nat_poverty_2011[1]
agg2$poverty_2012 <- df7$nat_poverty_2012[1]
agg2$poverty_2013 <- df7$nat_poverty_2013[1]

agg2$uerate_2006 <- df7$nat_ue_2006[1]
agg2$uerate_2007 <- df7$nat_ue_2007[1]
agg2$uerate_2008 <- df7$nat_ue_2008[1]
agg2$uerate_2009 <- df7$nat_ue_2009[1]
agg2$uerate_2010 <- df7$nat_ue_2010[1]
agg2$uerate_2011 <- df7$nat_ue_2011[1]
agg2$uerate_2012 <- df7$nat_ue_2012[1]
agg2$uerate_2013 <- df7$nat_ue_2013[1]

agg2$pcpi_2006 <- df7$real_pcnatinc_2006[1]
agg2$pcpi_2007 <- df7$real_pcnatinc_2007[1]
agg2$pcpi_2008 <- df7$real_pcnatinc_2008[1]
agg2$pcpi_2009 <- df7$real_pcnatinc_2009[1]
agg2$pcpi_2010 <- df7$real_pcnatinc_2010[1]
agg2$pcpi_2011 <- df7$real_pcnatinc_2011[1]
agg2$pcpi_2012 <- df7$real_pcnatinc_2012[1]
agg2$pcpi_2013 <- df7$real_pcnatinc_2013[1]

agg3 <- rbind(agg, agg2)
agg3[ , 1] <- c("local", "national")
names(agg3)[1] <- "location"

agg4 <- agg3

## write df7 to csv to send in email
setwd("C:/Users/Steve/Desktop/R/chicago")
write.csv(df7, "raw_data.csv")

## create average annual growth in pcpi and median since 2009
agg4$pcpi_growth <- (((agg4$pcpi_2010 - agg4$pcpi_2009) / agg4$pcpi_2009) + ((agg4$pcpi_2011 - agg4$pcpi_2010) / agg4$pcpi_2010) + 
        ((agg4$pcpi_2012 - agg4$pcpi_2011) / agg4$pcpi_2011) + ((agg4$pcpi_2013 - agg4$pcpi_2012) / agg4$pcpi_2012)) / 4

agg4$median_growth <- (((agg4$median_2010 - agg4$median_2009) / agg4$median_2009) + 
                               ((agg4$median_2011 - agg4$median_2010) / agg4$median_2010) + 
        ((agg4$median_2012 - agg4$median_2011) / agg4$median_2011) + ((agg4$median_2013 - agg4$median_2012) / agg4$median_2012)) / 4

## write agg4 to csv for excel
setwd("C:/Users/Steve/Desktop/R/chicago/ea")
write.csv(agg4, "agg4.csv")
