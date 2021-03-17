# AutoML Regression

# Import libraries & dataset ----
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(glue)
library(highcharter)
library(plotly)
library(h2o)  

path <- dirname(getSourceEditorContext()$path)
setwd(path)

raw <- fread("Life Expantancy Data.csv")

raw %>% skim()

names(raw) <- names(raw) %>% 
  str_replace_all(" ","_") %>% 
  str_replace_all("-","_") %>% 
  str_replace_all("/","_")


# --------------------------------- Modeling ----------------------------------
h2o.init()

h2o_data <- raw %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'Life_expectancy'
features <- raw %>% select(-Life_expectancy) %>% names()


# Fitting h2o model ----
model <- h2o.automl(
  x = features,
  y = target,
  training_frame    = train,
  validation_frame  = test,
  leaderboard_frame = test,
  stopping_metric = "RMSE",
  seed = 123,
  max_runtime_secs = 360)

model@leaderboard %>% as.data.frame()
model@leader


# Predicting the Test set results ----
y_pred <- model %>% h2o.predict(newdata = test) %>% as.data.frame()
y_pred$predict


# ----------------------------- Model evaluation -----------------------------
test_set <- test %>% as.data.frame()
residuals = test_set$Life_expectancy - y_pred$predict

# Calculate RMSE (Root Mean Square Error) ----
RMSE = sqrt(mean(residuals^2))

# Calculate Adjusted R2 (R Squared) ----
y_test_mean = mean(test_set$Life_expectancy)

tss = sum((test_set$Life_expectancy - y_test_mean)^2) #total sum of squares
rss = sum(residuals^2) #residual sum of squares

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE),
       R2, Adjusted_R2)


# Plotting actual & predicted ----
my_data <- cbind(predicted = y_pred$predict,
                 observed = test_set$Life_expectancy) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Test: Adjusted R2 = {round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g %>% ggplotly()


# Check overfitting ----
y_pred_train <- model %>% h2o.predict(newdata = train) %>% as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$Life_expectancy - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$Life_expectancy)

tss = sum((train_set$Life_expectancy - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))


# Plotting actual & predicted
my_data_train <- cbind(predicted = y_pred_train$predict,
                       observed = train_set$Life_expectancy) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color = "darkred") + 
  geom_smooth(method=lm) + 
  labs(x="Predecited Power Output", 
       y="Observed Power Output",
       title=glue('Train: Adjusted R2 = {round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color="darkgreen",size=16,hjust=0.5),
        axis.text.y = element_text(size=12), 
        axis.text.x = element_text(size=12),
        axis.title.x = element_text(size=14), 
        axis.title.y = element_text(size=14))

g_train %>% ggplotly()


# Compare 
library(patchwork)
g_train + g

tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2)
