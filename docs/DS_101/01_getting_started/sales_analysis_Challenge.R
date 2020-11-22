# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("tidyverse")
library("readxl")
library("lubridate")
# 2.0 Importing Files ----
bikes <- read_xlsx("DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
order_lines <- read_xlsx("DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops <- read_xlsx("DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----
# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- order_lines %>%
  left_join(bikes, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  select(order.id, order.date, customer.id, product.id, quantity, price, name, location) %>%
  mutate(total_price = price * quantity) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_")) %>%
  separate(col = location,
           into = c("city", "state"),
           sep = ", ")


# 6.0 Business Insights ----
# 6.1 Sales by state ----
# Step 1 - Manipulate
bike_orders_state <- bike_orderlines_wrangled_tbl %>%
  transmute(total_price,state) %>%
  group_by(state) %>%
  summarise(sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
# Step 2 - Visualize
bike_orders_state %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = state, y = sales)) +
  
  # Geometries
  geom_col(fill = "#2DC6D6") + # Use geom_col for a bar plot
  geom_label(aes(label = sales_text)) + # Adding labels to the bars
  geom_smooth(method = "lm", se = FALSE) + # Adding a trendline
  
  # Formatting
  # scale_y_continuous(labels = scales::dollar) + # Change the y-axis. 
  # Again, we have to adjust it for euro values
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title    = "Revenue by state",
    x = "", # Override defaults for x and y
    y = "Revenue"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 6.2 Sales by Year and Category 2 ----
# Step 1 - Manipulate
bike_orders_state_year <- bike_orderlines_wrangled_tbl %>%
  transmute(year = year(order_date),total_price,state) %>%
  group_by(state,year) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))
# Step 2 - Visualize
bike_orders_state_year %>%
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = state)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ state) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and state",
    #subtitle = "Each product category has an upward trend",
    fill = "state" # Changes the legend name
  )

# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
