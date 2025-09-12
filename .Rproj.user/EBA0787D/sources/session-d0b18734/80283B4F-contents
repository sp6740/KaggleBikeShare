library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(patchwork)
library(skimr)
library(DataExplorer)
library(dplyr)

test_file <- vroom("test.csv")
train_file <- vroom("train.csv")

skimr::skim(test_file)
skimr::skim(train_file)
# dplyr::glimpse(dataset) - lists the variable type of each column (makes sure each variable is the right type)
# skimr::skim(dataset) - nice overview of the dataset
# DataExploerer::plot_intro(dataset) - visualization of glimpse()
# DataExploerer::plot_correlation(dataset) - correlation heat map between variables
# DataExplorer::plot_bar(dataset) - bar charts of all discrete variables
# DataExploerer::plot_histograms(dataset) - histograms of all numeric variables
# DataExplorer::plot_missing(dataset) - percent missing in each column

# ggplot()
# geom_point() - scatterplots
# geom_bar() - bar plots
# geom_density() - density plots
# geom_histogram() - histograms
# geom_smooth() - adds trend line
# geom_boxplot() - boxplot


train_file <- train_file %>%
  select(-registered, -casual)
train_file <- train_file %>%
  mutate(count = log(count))

my_recipe <- recipe(count ~ ., data = train_file) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(weather = as.factor(weather)) %>%
  step_time(datetime, features = "hour") %>%
  step_mutate(season = as.factor(season)) %>%
  step_rm(atemp)
prepped_recipe <- prep(my_recipe)
baked_recipie <- bake(prepped_recipe, new_data = train_file)



## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") # Regression just means quantitative response



## Combine my Recipe and Model into a Workflow and fit
bike_workflow <- workflow() %>%
  add_recipe(my_recipe) %>%
  add_model(my_linear_model) %>%
  fit(data = train_file)


## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = test_file )

lin_preds <- lin_preds %>% 
  mutate(.pred = exp(.pred))

head(baked_recipie, 5)
