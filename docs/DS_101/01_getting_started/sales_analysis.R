# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library("tidyverse")
library("readxl")
library("lubridate")
# 2.0 Importing Files ----
bikes <- read_xlsx("C:/Users/Lukasten/Documents/Github/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
order_lines <- read_xlsx("C:/Users/Lukasten/Documents/Github/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops <- read_xlsx("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
# 3.0 Examining Data ----


# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- order_lines %>% left_join(bikes, by = c("product.id" = "bike.id")) %>% left_join(bikeshops, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_joined_tbl %>% 
  select(category) %>%
  filter(str_detect(category, "^Mountain")) %>% 
  unique()

# 6.0 Business Insights ----
# 6.1 Sales by Year ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  separate(col = category,
           into = c("category.1", "category.2","category.3"),sep = " - ") %>%
  mutate(total_price = price * quantity) %>%
  select(-...1, -gender) %>%
  
  # 5.3.2 by a pattern
  # You can use the select_helpers to define patterns. 
  # Type ?ends_with and click on Select helpers in the documentation
  select(-ends_with(".id")) %>%
  
  # 5.3.3 Actually we need the column "order.id". Let's bind it back to the data
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  
  # 5.3.4 You can reorder the data by selecting the columns in your desired order.
  # You can use select_helpers like contains() or everything()
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total_price,
         everything()) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# Step 1 - Manipulate
bike_orders_year <- bike_orderlines_wrangled_tbl %>%
  transmute(order_year = year(order_date),price,quantity,total_price) %>%
  group_by(order_year) %>%
  summarize(bike_orders = sum(quantity),
            sales = sum(total_price)) %>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
bike_orders_year %>%
  
  # Setup canvas with the columns year (x-axis) and sales (y-axis)
  ggplot(aes(x = order_year, y = sales)) +
  
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
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----
# Step 1 - Manipulate

bike_orders_year_category <- bike_orderlines_wrangled_tbl %>%
  transmute(year = year(order_date), category = category_1, total_price) %>%
  group_by(year, category) %>%
  summarize(sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
bike_orders_year_category %>%
  
  # Set up x, y, fill
  ggplot(aes(x = year, y = sales, fill = category)) +
  
  # Geometries
  geom_col() + # Run up to here to get a stacked bar plot
  
  # Facet
  facet_wrap(~ category) +
  
  # Formatting
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category" # Changes the legend name
  )


# 7.0 Writing Files ----

# 7.1 Excel ----
#install.packages("writexl")
#library("writexl")
#bike_orderlines_wrangled_tbl %>%
#  write_xlsx("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
