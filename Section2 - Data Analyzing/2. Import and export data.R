# Import and Export data

library(tidyverse)

data <- dslabs::gapminder


library(rstudioapi)
path <- dirname(getSourceEditorContext()$path)
setwd(path)


# Export data as 'csv' format
data %>% write_csv('gapminder.csv')

# Export data as 'xlsx' format
library(writexl) 
data %>% write_xlsx('gapminder.xlsx')


# Import data in 'csv' format 
df <- read_csv("gapminder.csv")

# Import data in 'xlsx' format 
library(readxl)
df <- read_xlsx("gapminder.xlsx")
