# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(skimr)
library(caTools)
library(Matrix)
library(lightgbm)
library(ROCR)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('Churn_Modelling.csv')

df %>% skim()

df <- df %>% select(-RowNumber, -CustomerId, -Surname)

df$Exited %>% table() %>% prop.table() %>% round(2)


# Create LGB Dataset ----
features <- df %>% colnames() %>% setdiff("Exited")

categoricals <- df %>% 
  select(-Exited) %>% 
  mutate_if(is.factor, as.character) %>% 
  select_if(is.character) %>% 
  names()

# Splitting the df into the Train set and Test set
set.seed(123)
split <- df$Exited %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == TRUE)
test <- df %>% subset(split == FALSE)

train_sparse <- train %>% select(features) %>% as.matrix() %>% Matrix(sparse = T)
test_sparse <- test %>% select(features) %>% as.matrix() %>% Matrix(sparse = T)

lgb.train <- train_sparse %>% lgb.Dataset(label = train$Exited)


# Modeling ----

# Setting up LGBM Parameters 
lgb.grid <- list(objective = "binary",
                 metric = "auc",
                 min_sum_hessian_in_leaf = 1,
                 min_data_in_leaf = 30,
                 feature_fraction = 0.7,
                 bagging_fraction = 0.7,
                 bagging_freq = 5,
                 min_data = 100,
                 max_bin = 50,
                 lambda_l1 = 8,
                 lambda_l2 = 1.3,
                 min_data_in_bin=100,
                 min_gain_to_split = 10,
                 is_unbalance = TRUE)

# Train Final Model
lgb.model <- lgb.train(params = lgb.grid, 
                       data = lgb.train, 
                       nrounds = 1000,
                       max_depth = 6,
                       num_leaves = 30,
                       learning_rate = 0.02,
                       eval_freq = 20, 
                       categorical_feature = categoricals)

# # Feature importance
lgb.model %>% 
  lgb.importance() %>% 
  lgb.plot.importance(top_n = 5)

# Prediction
y_pred <- lgb.model %>% predict(test_sparse)


# Evaluation Metrices  ----

pred <- y_pred %>% prediction(test$Exited)
eval <- pred %>% performance("acc")
max <- which.max(slot(eval, "y.values")[[1]])

Threshold <- slot(eval, "x.values")[[1]][max]

Accuracy <- slot(eval, "y.values")[[1]][max]

AUC <- pred %>% 
  performance("auc") %>% 
  slot("y.values") %>% 
  unlist() %>% round(2)

Gini <- 2 * AUC - 1

tibble(Threshold, Accuracy, AUC, Gini)
