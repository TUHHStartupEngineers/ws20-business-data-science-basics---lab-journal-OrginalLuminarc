library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")

# Select columns and filter categories
pct_sales_by_customer_tbl <- bike_orderlines_tbl %>%
  
  select(bikeshop, category_1, category_2, quantity) %>%
  filter(category_1 %in% c("Mountain","Road")) %>% 
  
  # Group by category and summarize
  group_by(bikeshop, category_1, category_2) %>%
  summarise(total_qty = sum(quantity)) %>%
  ungroup() %>%
  
  # Add missing groups (not necessarily mandatory, but we'd get holes in the plot)
  # complete() creates NAs. We need to set those to 0.
  complete(bikeshop, nesting(category_1, category_2)) %>% 
  mutate(across(total_qty, ~replace_na(., 0))) %>%  
  
  # Group by bikeshop and calculate revenue ratio
  group_by(bikeshop) %>%
  mutate(pct = total_qty / sum(total_qty)) %>%
  ungroup() %>%
  
  # Reverse order of bikeshops
  mutate(bikeshop = as.factor(bikeshop) %>% fct_rev()) %>%
  # Just to verify
  mutate(bikeshop_num = as.numeric(bikeshop))

# Data Visualization
pct_sales_by_customer_tbl %>%
  
  ggplot(aes(category_2, bikeshop)) +
  
  # Geometries
  geom_tile(aes(fill = pct)) +
  geom_text(aes(label = scales::percent(pct, accuracy = 1L)), 
            size = 3) +
  facet_wrap(~ category_1, scales = "free_x") +
  
  # Formatting
  scale_fill_gradient(low = "white", high = "#2C3E50") +
  labs(
    title = "Heatmap of Purchasing Habits",
    x = "Bike Type (Category 2)",
    y = "Customer",
    caption = str_glue(
      "Customers that prefer Road: 
        To be discussed ...
        
        Customers that prefer Mountain: 
        To be discussed ...")
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(face = "bold.italic")
  )