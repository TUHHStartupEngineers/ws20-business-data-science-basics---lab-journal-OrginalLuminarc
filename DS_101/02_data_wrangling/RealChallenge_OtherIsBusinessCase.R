library(tidyverse)
library(data.table)
library(vroom)

col_types <- list(
  id = col_character(),
  type = col_skip(),
  number = col_character(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)
setDT(patent_tbl)

col_types_assignee <- list(
  id = col_character(),
  type = col_character(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_assignee,
  na         = c("", "NA", "NULL")
)

setDT(assignee_tbl)

col_types_patent_assignee <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_skip()
)

patent_assignee_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_patent_assignee,
  na         = c("", "NA", "NULL")
)
setDT(patent_assignee_tbl)

col_types_uspc <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "DS_101/02_data_wrangling/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_uspc,
  na         = c("", "NA", "NULL")
)
setDT(uspc_tbl)

#Question 1
setnames(assignee_tbl, "id", "assignee_id")

pat_ass_merged <- merge(x = patent_assignee_tbl , y = assignee_tbl , 
                        by    = "assignee_id")

patents_corporations <- pat_ass_merged[!is.na(organization) & type == 2, .N, by = organization][order(-N)]
patents_corporations  %>% head(10) %>% write_rds("DS_101/02_data_wrangling/Q1")

#Question 2
patent_tbl[,year := lubridate::year(date)]
patent_tbl_clean <- patent_tbl[, .(patent_id = id,year)]

table2 <- merge(x = pat_ass_merged , y = patent_tbl_clean , by = "patent_id")

patents_2019 <- table2[!is.na(organization) & year == 2019 & type == 2, .N, by = organization][order(-N)]
patents_2019 %>% head(10) %>% write_rds("DS_101/02_data_wrangling/Q2")

#Question 3
table3 <- merge(x = pat_ass_merged , y = uspc_tbl , by = "patent_id") %>%
  unique()

patents_per_corp <- pat_ass_merged[!is.na(organization), .N, by = organization][order(-N)] 

table3_expanded <- merge(x = table3 , y = patents_per_corp , by = "organization")

min_patents <- patents_per_corp[10, N]

sector <- table3_expanded[N >= min_patents, .N , by = mainclass_id][order(-N)]
sector %>% head(5) %>% write_rds("DS_101/02_data_wrangling/Q3")
