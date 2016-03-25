library(gridExtra)
library(grid)
library(gtable)
library(dplyr)
library(stringr)
library(scales)

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

env_sum_table <- tableGrob(env_sum)
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
