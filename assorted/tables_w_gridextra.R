library(gridExtra)
library(grid)
library(gtable)
library(dplyr)
library(stringr)
library(scales)

# great tutorials on formatting certain cells
# http://stackoverflow.com/questions/18414001/gridextra-colour-different-rows-with-tablegrob
# http://stackoverflow.com/questions/32173928/r-tablegrob-change-format-of-row
# https://github.com/baptiste/gridextra/wiki/tableGrob#aesthetic-formatting

# create table output
# create table of funding by year (just example msi table, but not from same agency_table example)
msi_table <- msi %>% group_by(FY) %>% summarize(count = n(), amount = sum(EDA.Funding, na.rm = TRUE)) %>% data.frame(.)
msi_totals <- msi_table %>% summarize(total_count = sum(count, na.rm = TRUE), total_amount = sum(amount, na.rm = TRUE))
msi_totals <- cbind("Totals", msi_totals)
names(msi_totals) <- names(msi_table)
msi_table <- rbind(msi_table, msi_totals)
msi_table$amount <- paste("$", comma(msi_table$amount), sep = "")
names(msi_table) <- c("FY", "Total Count", "Total Amount")

# create main table
agency_table_output <- tableGrob(agency_table, rows = NULL)
title <- textGrob(paste("Count and amount", "\n", "of IMCP community applications and awards, by agency", "\n"), gp = gpar(fontsize = 15, fontface = "bold"))
padding <- unit(0.5, "line")
agency_table_output <- gtable_add_rows(agency_table_output, heights = grobHeight(title) + padding, pos = 0)
agency_table_output <- gtable_add_grob(agency_table_output, list(title), t = 1, l = 1, r = ncol(agency_table_output))

# create totals row table
totals_table_output <- tableGrob(agency_table[nrow(agency_table), ], rows = NULL, 
                                 cols = as.character(agency_table[nrow(agency_table), 1:ncol(agency_table)]))

# combine main table with totals row
agency_table_output <- rbind(agency_table_output[-nrow(agency_table_output), ], totals_table_output[-nrow(totals_table_output), ])
grid.newpage()
grid.draw(agency_table_output)

##########################

# add footnote to ggplot
footnote <- "Test footnote"
g <- ggplot(iris, aes(x = Species, y = Sepal.Length)) + geom_bar(stat = "identity")
g2 <- arrangeGrob(g, bottom = textGrob(footnote, x = 0, 
                hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))
grid.newpage()
grid.draw(g2)



##########################


# save table to file
# https://www.stat.berkeley.edu/classes/s133/saving.html

# save table 
grid.newpage()
pdf("test.pdf", width=6, height=4, paper = "special") 
grid.draw(state_table_output)
dev.off()

# best for microsoft word
grid.newpage()
win.metafile("mygraph.wmf")
grid.draw(state_table_output)
dev.off()





##########################
# original notes
##########################

setwd("G:/PNP/Performance Measurement/master_data")
datafilename <- list.files()[str_detect(list.files(), "master_data_20")]
md <- read.csv(datafilename, stringsAsFactors = FALSE) 

# data call for angie, asking for environmentally sustainable development awards
env <- filter(md, grepl("CC|CD|CF|CP", Initiatives), FY > 2009, Status == "Approved")
env_sum_year <- env %>%
        group_by(FY) %>%
        summarize(
                "Count of Awards" = n(),
                "Funding (mil)" = dollar(round(sum(Best.EDA..) / 1000000, 0)),
                "PI (mil)" = dollar(round(sum(Private.Investment, na.rm = TRUE) / 1000000, 0)),
                JCR = comma(sum(Jobs.Created, na.rm = TRUE) + (sum(Jobs.Saved, na.rm = TRUE)))
                )

env_sum <- env %>%
        summarize(
                "Count of Awards" = n(),
                "Funding (mil)" = dollar(round(sum(Best.EDA..) / 1000000, 0)),
                "PI (mil)" = dollar(round(sum(Private.Investment, na.rm = TRUE) / 1000000, 0)),
                JCR = comma(sum(Jobs.Created, na.rm = TRUE) + sum(Jobs.Saved, na.rm = TRUE))
        )

# create tables

# create quick tables
grid.table(env_sum, rows = NULL)
grid.table(env_sum_year, rows = NULL)

# create pretty tables

# if you have text fields you want to wrap to limit width
key <- c("I feel close to other parents with children the same age", "I am comfortable asking for advice about parenting", "I take time out to take care of my own health and well???being")
key_wraped <- strwrap(key, width = 30, simplify = FALSE) # modify 30 to your needs
key_new <- sapply(key_wraped, paste, collapse = "\n")

env_sum_table <- tableGrob(env_sum, rows = NULL)
title <- textGrob(paste("EDA Environmentally", "\n", "Sustainable Development"), gp = gpar(fontsize = 15, fontface = "bold"))
padding <- unit(0.5, "line")
env_sum_table <- gtable_add_rows(
        env_sum_table, heights = grobHeight(title) + padding, pos = 0)
env_sum_table <- gtable_add_grob(
        env_sum_table, list(title),
        t = 1, l = 1, r = ncol(env_sum_table))
grid.newpage()
grid.draw(env_sum_table)


# working example of the wrap text fields function
key <- c("I feel close to other parents with children the same age", "I am comfortable asking for advice about parenting", "I take time out to take care of my own health and well???being")
key_wraped <- strwrap(key, width = 30, simplify = FALSE) # modify 30 to your needs
key_new <- sapply(key_wraped, paste, collapse = "\n")

x <- c(1, 2, 3)
y <- c(5, 6, 7)
df <- data.frame(key_new, x, y)
df_table <- tableGrob(df)
title <- textGrob(paste("EDA Environmentally", "\n", "Sustainable Development"), gp = gpar(fontsize = 15, fontface = "bold"))
padding <- unit(0.5, "line")
df_table <- gtable_add_rows(
        df_table, heights = grobHeight(title) + padding, pos = 0)
df_table <- gtable_add_grob(
        df_table, list(title),
        t = 1, l = 1, r = ncol(df_table))
grid.newpage()
grid.draw(df_table)
