library(tidyverse)
library(lubridate)
library(scales)
library(ggrepel)

covid_data_tbl <- read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv") %>%
  filter(year == 2020 &countriesAndTerritories %in% c("Spain", "France", "Germany", "United_Kingdom", "United_States_of_America")) %>%
  mutate(date = make_date(year, month, day)) %>%
  mutate(month = month(date)) %>%
  mutate(month_txt = month(date, label = TRUE, abbr = FALSE)) %>%
  arrange(countriesAndTerritories, date) %>%
  group_by(countriesAndTerritories) %>%
  mutate(cum_cases = cumsum(cases)) %>%
  ungroup()

max_table <- covid_data_tbl %>%
  filter(countriesAndTerritories == "United_States_of_America") %>%
  slice_max(cum_cases)

covid_data_tbl %>%
  
  ggplot(aes( x = date, y = cum_cases, color = countriesAndTerritories)) +
  
  geom_line(size = 1, linetype = 1) + 
  
  scale_y_continuous(breaks = c(0,2.5e6,5e6,7.5e6,10e6, 12.5e6),
                     labels = scales::dollar_format(scale = 1e-6, 
                                                    prefix = "",
                                                    suffix = "M")) +
  
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + 
  
  theme_grey() +
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold")) +
  
  scale_color_discrete(name = "Country",
                       labels = c("France", "Germany", "Spain", "UK", "USA")) +
  
  guides(col = guide_legend(nrow = 2)) +
  
  labs(
    title = "COVID-19 confirmed cases worldwide",
    x = "Year 2020",
    y = "Cummulative Cases"
  ) +
  
  geom_label_repel(aes(x=date, y=cum_cases, label=cum_cases),
             data = max_table,
             hjust = 1.5,
             fill = "#F763E0",
             color= "white")