# xgboost.R
#
# William John Zywiec
# The George Washington University

library(xgboost)

data <- data.set$training.df

model <- xgboost(
  data = data,
  label = data.set$training.data$keff,
  eta = 0.01, # 0.3
  max_depth = 30, # 6
  nround = 10000,
  objective = 'reg:squarederror',
  eval_metric = 'mae',
  early_stopping_rounds = 50)

predictions <- predict(model, data.set$test.df)
