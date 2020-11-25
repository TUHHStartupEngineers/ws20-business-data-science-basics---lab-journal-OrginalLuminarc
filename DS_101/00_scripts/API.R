library(tidyverse)
library(httr)
library(jsonlite)

resp <- GET("https://pokeapi.co/api/v2/type/15")
list =  resp %>%
  .$content %>%
  rawToChar() %>%
  fromJSON()
ice_pokemon <- list[["pokemon"]] 
ice_pokemon %>% head(n = 10)