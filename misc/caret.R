# caret.R
#
# William John Zywiec
# The George Washington University

# load packages
library(caret)
library(ranger)
library(Cubist)

caret.train <- training.data[c(-2, -9)]
caret.train$mod <- caret.train$mod %>% as.factor() %>% as.numeric()
caret.train$ref <- caret.train$ref %>% as.factor() %>% as.numeric()

caret.test <- test.data[c(-2, -9)]
caret.test$mod <- caret.test$mod %>% as.factor() %>% as.numeric()
caret.test$ref <- caret.test$ref %>% as.factor() %>% as.numeric()

ranger.grid <- expand.grid(
  .mtry = c(2, 4, 6),
  .splitrule = c("variance", "extratrees", "maxstat"),
  .min.node.size = 1:5)

ranger.model <- train(keff ~ . ,
  data = caret.train,
  method = "ranger",
  preProcess = c("center", "scale"),
  tuneGrid = ranger.grid)

cubist.grid <- expand.grid(
  .committees = c(1, 10, 20, 30, 40, 50),
  .neighbors = c(0:10))

cubist.model <- train(keff ~ . ,
  data = caret.train,
  method = "cubist",
  preProcess = c("center", "scale"),
  tuneGrid = cubist.grid)
