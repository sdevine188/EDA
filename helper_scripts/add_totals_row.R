iris2 <- iris

# find "iris2" and replace with <your_data_name>
Row_ID <- data.frame(Row_ID = seq(nrow(iris2)))
iris2 <- bind_cols(Row_ID, iris2)
iris2[nrow(iris2) + 1, ] <- NA
for(i in 1:ncol(iris2)){
        if(class(iris2[ , i])[1] == "POSIXct") { 
                iris2[ , i] <- as.character(iris2[ , i]) 
        } else { if(class(iris2[ , i]) == "numeric" | class(iris2[ , i]) == "integer") { 
                iris2[ nrow(iris2), i] <- sum(iris2[ , i], na.rm = TRUE) 
                } else { 
                        iris2[ nrow(iris2), i] <- NA 
                }
        }
}
if("FY" %in% names(iris2)) {
        iris2[nrow(iris2), which(names(iris2) == "FY")] <- "NA" 
}
iris2[nrow(iris2), 1] <- "Totals"


# check to confirm
tail(iris2)
