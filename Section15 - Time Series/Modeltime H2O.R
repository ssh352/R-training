# Import libraries & dataset ----
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime.h2o)
library(rstudioapi)

df <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales)

df %>% 
  group_by(id) %>% 
  plot_time_series(
    Date, Weekly_Sales,
    .facet_ncol  = 2,
    .smooth      = F,
    .interactive = T)


# Splitting ----
splits <- df %>% 
  time_series_split(assess = "3 month", cumulative = T)

recipe_spec <- recipe(Weekly_Sales ~ ., training(splits)) %>%
  step_timeseries_signature(Date) 

train <- training(splits) %>% bake(prep(recipe_spec),.)
test  <- testing(splits) %>% bake(prep(recipe_spec),.)


# Modeling ----

h2o.init()

# Specification
model_spec_h2o <- automl_reg(mode = 'regression') %>%
  set_engine(
    'h2o', nfolds = 5, seed = 123,
    max_models = 3, verbosity = NULL,
    max_runtime_secs = 5, 
    max_runtime_secs_per_model = 3,
    exclude_algos = c("DeepLearning")) 

# Training
model_fit_h2o <- model_spec_h2o %>%
  fit(Weekly_Sales ~ ., train)

# Prediction
Prediction <- model_fit_h2o %>% predict(test)


# Modeltime Workflow ----

modeltime <- model_fit_h2o %>% modeltime_table() 

# Calibrate to the testing set and visualize the forecasts
modeltime %>%
  modeltime_calibrate(test) %>%
  modeltime_forecast(
    new_data = test,
    actual_data = df,
    keep_data = T
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2, 
    .interactive = T)


# Refit to Full Dataset & Forecast Forward ----

data_prepared <- bind_rows(train, test)

future <- data_prepared %>%
  group_by(id) %>%
  future_frame(.length_out = "1 year") %>%
  ungroup()

future_prepared <- recipe_spec %>% prep() %>% bake(future)

refit <- modeltime %>%
  modeltime_refit(data_prepared)

# Visualize the final forecast
refit %>%
  modeltime_forecast(
    new_data = future_prepared,
    actual_data = data_prepared,
    keep_data = T
  ) %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol = 2,
    .interactive = T)


# Saving and Loading Models ----
path <- dirname(getSourceEditorContext()$path)

model_fit_h2o %>% 
  save_h2o_model(path = paste0(path,"/model_fit_h2o"), overwrite = T)

model <- load_h2o_model(path = paste0(path,"/model_fit_h2o"))
