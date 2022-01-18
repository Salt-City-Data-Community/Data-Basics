# Data Cleaning Basics with R
# Source data set: https://www.kaggle.com/dgomonov/new-york-city-airbnb-open-data
# Original source data has been modified for data cleaning tasks

# Clear any previously loaded variables in R session
rm(list = ls())

# Set random seed for outputs that can be replicated
set.seed(10)

# Load the tidyverse library, install if needed
#install.packages("tidyverse")
library(tidyverse)

raw_data <- read.csv('airbnb_nyc_2019.csv')

# Preview slice of data
raw_data[1:4,]

#Get unique value counts for each column
sapply(raw_data, function (x) length (unique (x)))

# Check classes of columns
sapply(raw_data, class)

# Checking for duplicate rows
table(duplicated(raw_data))

# Checking for duplicate rows in specific column
table(duplicated(raw_data$id))

data <- distinct(raw_data)

# Checking for spread of data and NA values
summary(data$price)

# Create point plot of all prices
ggplot(data = data) +
  aes(x = 1:nrow(data), y = price) +
  geom_point() +
  labs(title = 'Spread of NYC AirBNB Prices (2019)',
       x = "",
       y = "Price (USD)") +
  scale_y_continuous()

# Example for a subset of data is desired based on attributes:
subset_data <- data[which(data$neighbourhood=="Kensington"
                         & data$price < 300), ]

# Show total number of NA values in data
table(is.na(data))

# Check and see which columns have NA values
sapply(data, function(x) sum(is.na(x)))

# Get NA price data by selecting the respective rows
missing_prices <- which(is.na(data$price), arr.ind = TRUE)

typeof(missing_prices)

# Getting fraction of total data that has missing values in desired column
length(missing_prices) / nrow(data)

# Checking to gauge potential bias from missing data itself
data_with_prices <- data[-missing_prices,]
data_without_prices <- data[missing_prices,]

table(data$room_type)

table(data_with_prices$room_type)

table(data_without_prices$room_type)

# Decide on method to handle missing values

# Get mean, median, and mode of price to variables for possible use

mean_price <- mean(data_with_prices$price)

median_price <- median(data_with_prices$price)

# Create a function to calculate mode using the unique function and max values of each unique value result
mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

mode_price <- mode(data_with_prices$price)

# Fill in the missing values if using one of the values from above, in this case, the rounded mean

fill_na_sample_data <- data.frame(data)

fill_na_sample_data[c("price")][is.na(data[c("price")])] <- 152.72

# Impute the missing value using basic linear regression

sample_prices <- sample_n(data_with_prices, 500)

model <- lm(price ~ neighbourhood_group + 
              room_type, data = sample_prices)
summary(model)

price_predictions <- predict(model, newdata = data[missing_prices,])
data_prices_imputed <- data
data_prices_imputed[missing_prices,]$price <- price_predictions

# Drop the rows that have missing values
test_drop = data[-c(missing_prices), ] 

sapply(test_drop, function(x) sum(is.na(x)))
