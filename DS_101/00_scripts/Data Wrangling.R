library(tidyverse)
library(readxl)
library(stringr)

bikes_tbl      <- read_excel(path = "DS_101//00_data/01_bike_sales/01_raw_data/bikes.xlsx") %>%
  separate("category",c("category_1","category_2","category_3"), sep = " - ") %>%
  set_names(names(.) %>% str_replace_all("\\.", "_")) 

orderlines_tbl <- read_rds("DS_101//00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds") %>%
  select(-price)

bikeshops <- read_excel(path = "DS_101//00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

bike_orderlines_tbl <- left_join(orderlines_tbl, bikes_tbl, by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops, by = c("customer_id" = "bikeshop.id"))

order_dates_tbl <- bike_orderlines_tbl %>% select(1:3)
order_items_tbl  <- bike_orderlines_tbl %>% select(1:2,4:8)

joined <- order_dates_tbl %>%
  left_join(order_times_tbl)