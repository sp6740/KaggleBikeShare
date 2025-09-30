setwd("C:/Users/sophi/OneDrive/Documents/Stat_348/KaggleBikeShare")
library(tidyverse)
library(tidymodels)
library(vroom)
library(ggplot2)
library(patchwork)
library(DataExplorer)
library(glmnet)
library(rpart)
library(ranger)
library(bonsai)
library(lightgbm)
library(agua)

# h2o::h2o.init()

test_file <- vroom("test.csv")
train_file <- vroom("train.csv")

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


## Setup and Fit the Linear Regression Model
my_linear_model <- linear_reg() %>% #Type of model
  set_engine("lm") %>% # Engine = What R function to use
  set_mode("regression") # Regression just means quantitative response


my_recipe <- recipe(count ~ ., data = train_file) %>%
  step_mutate(weather = ifelse(weather == 4, 3, weather)) %>%
  step_mutate(across(c(weather, season, holiday), as.factor)) %>%
  step_time(datetime, features = "hour") %>%
  step_interact(terms = ~ datetime_hour:workingday) %>%
  step_interact(terms = ~ datetime_hour:temp) %>%
  step_date(datetime, features =  c("doy", "dow")) %>%
  step_mutate(
    hour_sin = sin(2 * pi * datetime_hour / 24),
    hour_cos = cos(2 * pi * datetime_hour / 24),
    doy_sin  = sin(2 * pi * datetime_doy / 365),
    doy_cos  = cos(2 * pi * datetime_doy / 365)) %>%
  step_mutate(hour = as.factor(datetime_hour)) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors())
prepped_recipe <- prep(my_recipe)
baked_recipie <- bake(prepped_recipe, new_data = train_file)

## Define the model
## max_runtime_secs = how long to let h2o.ai run
## max_models = how many models to stack
# auto_model <- auto_ml() %>%
# set_engine("h2o", max_runtime_secs=300, max_models=6) %>%
# set_mode("regression")

# my_mod <- rand_forest(mtry = tune(),
#                         min_n=tune(),
#                       trees=600) %>% #Type of model
#   set_engine("ranger") %>% # What R function to use
#   set_mode("regression")


bart_model <- bart(trees=tune()) %>% # BART figures out depth and learn_rate
  set_engine("dbarts") %>%
  set_mode("regression")

## Combine my Recipe and Model into a Workflow and fit
# automl_wf <- workflow() %>%
# add_recipe(my_recipe) %>%
# add_model(auto_model) %>%
# fit(data=train_file)


## Set Workflow
preg_wf <- workflow() %>%
add_recipe(my_recipe) %>%
add_model(bart_model)

# ## Set up grid of tuning values
mygrid <- grid_regular(trees(range = c(1, 17)), levels = 3)
# 
# 
# ## Split data for CV
folds <- vfold_cv(train_file, v = 4, repeats=1)

# ## Run the CV1
CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=mygrid,
          metrics=metric_set(rmse, mae)) #Or leave metrics NULL
# 
# ## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best(metric="rmse")
# 
final_wf <- preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=train_file)

# ## Predict
final_wf %>%
predict(new_data = test_file)

## Run all the steps on test data
preds <- predict(final_wf, new_data = test_file )

preds <- preds %>% 
  mutate(.pred = exp(.pred))


kaggle_submission <- preds %>%
  bind_cols(., test_file) %>% #Bind predictions with test data
  select(datetime, .pred) %>% #Just keep datetime and prediction variables
  rename(count=.pred) %>% #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) %>% #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./FeatureEngineering.csv", delim=",")

# sine and cosine of hour
# break hour down into a factor throughout the week
# log transformation on count