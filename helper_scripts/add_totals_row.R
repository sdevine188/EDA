# add totals to bottom of data for numeric or integer columns
iris2 <- iris

# find "iris2" and replace with <your_data_name>
Row_ID <- data.frame(Row_ID = seq(nrow(iris2)))
iris2 <- bind_cols(Row_ID, iris2)
iris2[nrow(iris2) + 1, ] <- unlist(sapply(iris2, function(x) { if(class(x) == "numeric" | class(x) == "integer") { 
        sum(x, na.rm = TRUE) } else { NA } } ))
iris2[nrow(iris2), 1] <- "Totals"
if("FY" %in% names(iris2)) {
        iris2[nrow(iris2), which(names(iris2) == "FY")] <- "NA" 
}

# check to confirm
tail(iris2)


