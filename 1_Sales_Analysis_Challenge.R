# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS CHALLENGE ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)










# 2.0 Importing Files ----
# A good convention is to use the file name and suffix it with tbl for the data structure tibble
bikes_tbl      <- read_excel(path = "C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")









# 3.0 Examining Data ----









# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# Examine the results with glimpse()
bike_orderlines_joined_tbl









# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>%
  # 5.1 Separate state and city name
  separate(col    = location,
           into   = c("city", "state"),
           sep    = ", ") %>%
  
  # 5.2 Add the total price (price * quantity) 
  # Add a column to a tibble that uses a formula-style calculation of other columns
  mutate(total.price = price * quantity) %>%
  
  # 5.4 Rename columns because we actually wanted underscores instead of the dots
  # (one at the time vs. multiple at once)
  rename(bikeshop = name) %>%
    set_names(names(.) %>% str_replace_all("\\.", "_"))
  
  
  

  
  
  
  
  
# 6.0 Business Insights ----
# 6.1 Sales by location(state) ----

library(lubridate)

# Step 1 - Manipulate
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns
  select(state, order_date, total_price) %>%
  
  # Grouping by state and summarizing sales
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>%
  
  # Optional: Add a column that turns the numbers into a currency format 
  # (makes it in the plot optically more appealing)
  # mutate(sales_text = scales::dollar(sales)) <- Works for dollar values
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_state_tbl %>%
  
  # Setup canvas with the columns state (x-axis) and sales (y-axis)
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
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))









# 6.2 Sales by Year and location(state) ----

# Step 1 - Manipulate
sales_by_year_loc_1_tbl <- bike_orderlines_wrangled_tbl %>%
  
  # Select columns and add a year
  select(state, order_date, total_price) %>%
  mutate(year = year(order_date)) %>%
  
  # Group by and summarize year and main category
  group_by(year, state) %>%
  summarise(sales = sum(total_price)) %>%
  ungroup() %>%
  
  # Format $ Text
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " €"))

sales_by_year_loc_1_tbl  

# Step 2 - Visualize
sales_by_year_loc_1_tbl %>%
  
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
    subtitle = "Each state has an upward trend",
    fill = "State" # Changes the legend name
  ) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 7.0 Writing Files ----

# 7.1 Excel ----
install.packages("writexl")
library("writexl")
bike_orderlines_wrangled_tbl %>%
  write_xlsx("C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")

# 7.2 CSV ----
bike_orderlines_wrangled_tbl %>% 
  write_csv("C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")

# 7.3 RDS ----
bike_orderlines_wrangled_tbl %>% 
  write_rds("C:/Benazir/Study/WinterSemester_2021/BusinessDataScienceBasics/data-science/DS_101/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")






