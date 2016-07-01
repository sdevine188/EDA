# see keyword frequencies
species_keywords <- iris %>% select(Species)
keyword_string <- "setosa|versicolor"

find_keyword_freq <- function(dataframe, keyword_string){
        keyword_df <- data.frame("keyword" = sapply(str_split(keyword_string, "\\|"), "["), "count" = NA)
        
        for(i in 1:nrow(keyword_df)){
                keyword <- as.character(keyword_df$keyword[i])
                keyword_count <- 0
                for(col in 1:ncol(dataframe)){
                        keyword_locations <- str_locate_all(dataframe[ , col], keyword)
                        for(row in 1:length(keyword_locations)){
                                keyword_count <- keyword_count + nrow(keyword_locations[[row]])
                        }
                }
                keyword_df$count[i] <- keyword_count
        }
        return(keyword_df)
}

species_keywords_freq <- find_keyword_freq(species_keywords, keyword_string)
