library(tidyverse)
library(lubridate)
library(scales)
library(maps)
world <- map_data("world")
covid_data_csv <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")# %>%
  
covid_data_tbl <- covid_data_csv %>%
  group_by(countriesAndTerritories) %>%
  summarise(deaths = sum(deaths),
            population = mean(popData2019)) %>%
  ungroup() %>% 
  mutate(mort_rate = deaths / population) %>%
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories
  ))


covid_world <-left_join(world, covid_data_tbl, by = c("region" = "countriesAndTerritories"))

covid_world %>%
  
  ggplot(aes(x = long, y = lat, fill = mort_rate)) +
  
  geom_map(aes(map_id = region, colour = "grey"), map = world) +
  
  scale_colour_manual(values=c('grey'),
                      guide = FALSE) +

  scale_fill_continuous(name = "Mortality Rate",
                        low = "#fc8d59", high = "#7f0000",
                        limits = c(0,0.0012),
                        breaks = c(0.00,0.0003,0.0006,0.0009,0.0012),
                        labels = c("0.00 %", "0.03 %", "0.06 %", "0.09 %", "0.12 %")) +
  
  theme(axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank()) +

  
  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "More than 1.2 Million COVID-19 deaths worldwide",
    caption = "Date: 30/11/2020",
    x = "",
    y = ""
  )
