library(readr)
library(magrittr)
library(ggplot2)
library(rsample)


data <- read_csv(here::here('datasets', '50_Startups.csv'))
data <- data %>% janitor::clean_names()

# encode categorical data --------
data$state <- factor(data$state, levels = c("New York", "California", "Florida"), labels = c(1,2,3))
glimpse(data)

#check for missing data -----
data[!complete.cases(data),]


# split data -----
set.seed(123)

train_test_split <- initial_split(data, prop = 0.8)
training_set <- training(train_test_split)
test_set <- testing(train_test_split)


# fit muiltiple linear regression to training set -----

#regressor <- lm(profit ~ r_d_spend + administration + marketing_spend + state, training_set)
regressor <- lm(profit ~ ., training_set) # shorthand for above

summary(regressor)

# predict the test set results -----
y_pred <- predict(regressor, test_set)

y_pred


# Building the optimal model with backward elimination ----
SL <-  0.05

backward_elimination <- function(x, y, sl) {
  numVars <-  length(x)
  for (i in c(1:numVars)) {
    regressor <- lm(profit ~ ., x) # edit for each dataset
    maxVar <- max(coef(summary(regressor))[c(2:numVars), 4])
    if(maxVar > sl) {
      j = which.max(coef(summary(regressor))[c(2:numVars), 4])
      x = x[,-j]
    }
    numVars <-  numVars - 1
  }
  return(summary(regressor))
}

backward_elimination(data, SL)

library(MASS)

res.lm <- lm(profit ~ ., data) 
step <- stepAIC(res.lm, direction = "backward", trace = TRUE)
step

