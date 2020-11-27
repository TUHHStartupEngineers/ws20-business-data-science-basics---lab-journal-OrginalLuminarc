library(tidyverse)
library(data.table)
url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
covid_data_dt <- fread(url)
#data("airquality")
#air_ql_tbl <- setDT(airquality)
#air_ql_tbl[!is.na(Ozone), .(Solar.R, Wind, Temp)]
#covid_data_dt[, sum(deaths > 1000)]

#mtcars <- as.data.table(mtcars)
#mtcars[,mileage_type := ifelse(mpg>20, "high", "low")]
#mtcars[,.N]
#mtcars[,mean(mpg), by = gear]

# Create a new data.table
covid_data_EUR_dt <- covid_data_dt[ continentExp == "Europe", 
                                    lapply(.SD, function(x) {
                                      x %>% 
                                        mean() %>% 
                                        round(1)
                                    }
                                    ), 
                                    by = .(countriesAndTerritories), 
                                    .SDcols = c("cases", "deaths")
]

# Set key
setkey(covid_data_EUR_dt, countriesAndTerritories)
key(covid_data_EUR_dt)

# Create two data.tables from that
cd_dt1 <- covid_data_EUR_dt[, .(countriesAndTerritories, cases)]
cd_dt2 <- covid_data_EUR_dt[1:20, .(countriesAndTerritories, deaths)]

# Join them
cd_dt1[cd_dt2]
cd_dt1[cd_dt2, on = "countriesAndTerritories", deaths := i.deaths]
