# Advanced Missing Imputation Techniques

library(tidyverse)
library(dslabs)

#devtools::install_github("alastairrushworth/inspectdf") 
library(inspectdf)

raw_df <- gapminder

raw_df %>% inspect_na() %>% show_plot()


# mice method
library(mice)

raw_df_mice <- raw_df %>% mice(method='rf', seed=123)
df <- raw_df_mice %>% complete()

df %>% inspect_na()


# random forest method
library(missForest)

raw_df %>% glimpse()

raw_df_rf <- raw_df %>% 
  select_if(is.numeric) %>% 
  missForest()

df <- raw_df %>% 
  select_if(is.factor) %>% 
  cbind(raw_df_rf$ximp)

df %>% inspect_na()
