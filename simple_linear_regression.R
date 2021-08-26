library(readr)
library(magrittr)
library(dplyr)
library(rsample)
library(ggplot2)


data <- read_csv(here::here("datasets","Salary_Data.csv"))

# split data ----
set.seed(123)


train_test_split <- initial_split(data, prop = 2/3)
training_set <- training(train_test_split)
test_set <- testing(train_test_split)

# fit Linear Regression to training set ----

regressor = lm(Salary ~ YearsExperience, training_set)

summary(regressor)

# predict test set results ----

y_pred <- predict(regressor, test_set)


# plot the results ----

ggplot(training_set) +
  geom_point(aes(YearsExperience, Salary), color = 'blue') +
  geom_line(aes(YearsExperience, predict(regressor, training_set)), color = 'grey') +
  labs(
    title = "Salary vs Experience (Training set)",
    x = "Years of experience"
  ) +
  theme_light()

ggplot() +
  geom_point(aes(test_set$YearsExperience, test_set$Salary), color = 'blue') +
  geom_line(aes(training_set$YearsExperience, predict(regressor, training_set)), color = 'grey') +
  labs(
    title = "Salary vs Experience (Test set)",
    x = "Years of experience"
  ) +
  theme_light()
