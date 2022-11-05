rm(list=ls())
library(tidyverse)
library(SbcRepTables)
library(parallel)

surveys <- read_rds("./data/surveys.rds")
surveys_mtd <- read_rds("./data/surveys_mtd.rds")
column_groups <- c("ciudad", "cadena","genero", "nse", "actividad")


# Create list of data frames
data_df <-
    surveys %>%
    sbc_report_list( surveys_mtd,
                     column_groups)

# Create HTML from data
surveys_html_sheet  <-
    data_df %>%
    sbc_html_heatmap(plasmaRows = c(""),
                     groupSeparator = T
    )

# View on RStudio
surveys_html_sheet %>% sbc_viewer()

# Or copy to excel
surveys_html_sheet %>% clipr::write_clip()




