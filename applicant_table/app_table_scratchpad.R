library(stringdist)
library(dplyr)
getwd()
list.files()
app <- read.csv("applicant_table_clusters_20150430.csv")
head(app, 50)

test <- filter(app, CLUSTER == 17)
head(test)
stringdist(a, b, method='jw', p=0.1)

stringdist("trace", "tracers", method = "jw", p = .1)
stringdist("pacerman", "tracers", method = "jw", p = .1)
test

test_scores <- test
for(i in 1:nrow(test)){
        test$APPL_NAME[i]
}