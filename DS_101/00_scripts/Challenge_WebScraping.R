library(tidyverse)
library(rvest)
library(stringr)
library(jsonlite)
library(glue)

home_url <- "https://www.radon-bikes.de"

html_home         <- read_html(home_url)

bike_category_tbl <- html_home %>%
  
  html_nodes(css = ".megamenu__item > a") %>%
  html_attr("href") %>%
  discard(.p = ~stringr::str_detect(.x,"wear|RADON LIFE|SERVICE & SUPPORT|DE")) %>%
  enframe(name = "position", value = "subdirectory") %>%
  mutate(url = glue("{home_url}{subdirectory}bikegrid/"))

bike_category_url <- bike_category_tbl$url[1]

get_bike_data <- function(url) {
  html_bike_category <- read_html(url)
  
  bike_url_tbl  <- html_bike_category %>%
    html_nodes(css = ".m-bikegrid__info > a") %>%
    html_attr("href") %>%
    enframe(name = "position", value = "url")
  
  bike_name_tbl <- html_bike_category %>%
    html_nodes(css =".a-heading--small") %>%
    html_text() %>%
    str_trim() %>%
    enframe(name = "position", value = "Model Name")
  
  bike_price_tbl <- html_bike_category %>%
    html_nodes(css =".m-bikegrid__price.currency_eur") %>%# > m-bikegrid__price") %>%
    html_nodes(css = ".m-bikegrid__price--active") %>%
    html_text() %>%
    str_extract(pattern = "[0-9]+") %>% 
    as.numeric()%>%
    enframe(name = "position", value = "price(\200)") %>%
    left_join(bike_name_tbl) %>%
    left_join(bike_url_tbl)
}