# Import libraries & dataset ----
library(tidyverse)
library(rstudioapi)
library(skimr)

path <- dirname(getSourceEditorContext()$path)
setwd(path)

df <- read_csv('50_Startups.csv')

df %>% skim()


# Splitting the df into the Train set and Test set ----
library(caTools)
set.seed(123)
split <- df$Profit %>% sample.split(SplitRatio = 0.8)
train <- df %>% subset(split == TRUE)
test <- df %>% subset(split == FALSE)


# Fitting XGBoost model ----
library(xgboost)
library(parsnip)
set.seed(123)
regression <- boost_tree(
  mode = "regression", #for classification - "classification"
  mtry = 30,
  learn_rate = 0.35,  
  tree_depth = 7) %>% 
  set_engine(engine = "xgboost") %>%
  fit(Profit ~ ., data = train)

# Predicting the Test set results
y_pred <- regression %>% predict(test %>% select(-Profit))


# Model evaluation ----
residuals = test$Profit - y_pred$.pred
RMSE = sqrt(mean(residuals^2))

y_test_mean = mean(test$Profit)
tss =  sum((test$Profit - y_test_mean)^2)
rss =  sum(residuals^2)
R2  =  1 - (rss/tss)

n <- test %>% nrow() 
k <- test %>% ncol() - 1
Adj_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE, R2, Adj_R2)
