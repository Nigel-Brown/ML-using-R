library(readr)
library(magrittr)
library(dplyr)
data <- read_csv(here::here("datasets","Data.csv"))
data <- data %>% janitor::clean_names()

# data cleansing ----
glimpse(data)

# show incomplete data
data[!complete.cases(data),]


# create subsets
spanish_data <- data[data$country == 'Spain',]
german_data <- data[data$country == 'Germany',]

median(data$age,na.rm = TRUE)
median(data$salary, na.rm = TRUE)
median(german_data$salary, na.rm = TRUE)
# country specific age is chosen as lower than median for all countries

data[is.na(data$age) & data$country == "Spain", "age"] <- median(spanish_data$age, na.rm = TRUE)

# country specific median salary is chosen as it's higher than across all countries

data[is.na(data$salary) & data$country == "Germany", "salary"] <- median(german_data$salary, na.rm = TRUE)


# Encode categorical data ----
data$country <- factor(data$country,
                       levels = c('France', 'Spain', 'Germany'),
                       labels = c(1,2,3))


data$purchased <- factor(data$purchased,
                       levels = c('Yes', 'No'),
                       labels = c(0,1))
glimpse(data)

# Split data into training and test sets

library(rsample)
set.seed(1234)

train_test_split <- initial_split(data = data, prop = 0.8)   
training_set <- training(train_test_split)
test_set <- test(train_test_split)

# Feature scaling ----
training_set[,2:3] <- scale(training_set[2:3])
test_set[,2:3] <- scale(test_set[[,2:3]])










 
