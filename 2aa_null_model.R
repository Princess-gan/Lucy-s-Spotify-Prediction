library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)

# handle common conflicts
tidymodels_prefer()

#set up parallel processing
library(doMC)
registerDoMC(cores = 10)

# set seed
set.seed(2023)

# load any required objects-----------------------------
load("initial_setup/tuning_setup.rda")

#null model eg
null_spec <- null_model() %>% 
  set_engine("parsnip") %>% 
  set_mode("regression") # or set_mode("classification")

null_workflow <- workflow() %>% 
  add_model(null_spec) %>% 
  add_recipe(song_recipe1) # yes we still need to add a recipe

null_fit <- fit_resamples(null_workflow, 
                          resamples = song_fold,
                          control = control_resamples(save_pred = TRUE))


#Collect predictions on testing data
null_test_preds <- collect_predictions(null_fit)

# Calculate performance metrics on testing data
null_test_metrics <- null_test_preds %>%
  metrics(truth = popularity, estimate = .pred)

# View performance metrics on testing data
null_test_metrics

write_csv(null_test_metrics, "best parameters/null_test_metrics.csv")
