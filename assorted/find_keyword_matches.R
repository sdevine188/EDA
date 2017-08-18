# visually check keyword-only hits
dataframe <- iris
keyword_string <- "setosa|versicolor"

find_keyword_matches <- function(dataframe, keyword_string, keep_columns = c("Control.No.")){
        keyword_matches <- data.frame(matrix(nrow = nrow(dataframe), ncol = ncol(dataframe)))
        names(keyword_matches) <- names(dataframe)
        drop_columns <- c()
        for(col in 1:ncol(dataframe)){
                column_values <- c()
                if(names(dataframe)[col] %in% keep_columns) {
                        column_values <- dataframe[ , col]
                        keyword_matches[ , col] <- column_values
                        next
                }
                if(!(names(dataframe)[col] %in% keep_columns)) {
                        for(row in 1:nrow(dataframe)){
                                if(grepl(keyword_string, dataframe[row, col], ignore.case = TRUE)){
                                        column_values <- c(column_values, as.character(dataframe[row, col]))
                                } else {
                                        column_values <- c(column_values, as.character(""))
                                }
                        }
                        if(length(which(column_values == "")) == nrow(dataframe)) {
                                drop_columns <- c(drop_columns, col)
                        }
                        keyword_matches[ , col] <- column_values
                }
        }
        keyword_matches <- keyword_matches[ , -drop_columns]
        return(keyword_matches)
}

species_keywords_only <- find_keyword_matches(dataframe, keyword_string, keep_columns = "Sepal.Length")
write_csv(species_keywords_only, "species_keywords_only.csv")


