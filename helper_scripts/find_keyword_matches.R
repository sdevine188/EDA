# visually check keyword-only hits
species_keywords <- iris %>% select(Species)
keyword_string <- "setosa|versicolor"

find_keyword_matches <- function(dataframe, keyword_string){
        keyword_matches <- data.frame(matrix(nrow = nrow(dataframe), ncol = ncol(dataframe)))
        names(keyword_matches) <- names(dataframe)
        for(col in 1:ncol(dataframe)){
                column_values <- c()
                for(row in 1:nrow(dataframe)){
                        if(grepl(keyword_string, dataframe[row, col], ignore.case = TRUE)){
                                column_values <- c(column_values, as.character(dataframe[row, col]))
                        } else {
                                column_values <- c(column_values, NA)
                        }   
                }
                keyword_matches[ , col] <- column_values
        }
        return(keyword_matches)
}

species_keywords_only <- find_keyword_matches(species_keywords, keyword_string)
write_csv(species_keywords_only, "species_keywords_only.csv")


