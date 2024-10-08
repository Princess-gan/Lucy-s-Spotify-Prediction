# load packages ----
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

# rf----
rf_spec <-
  rand_forest(
    min_n = tune(),
    mtry = tune()
  ) %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("regression")


# check tuning parameters
#hardhat::extract_parameter_set_dials(rf_spec)

# setup tuning
rf_params<-hardhat::extract_parameter_set_dials(rf_spec) %>% 
  update(
    #1: growing trees using at least 1 variable (maybe would like to find a simple explanation where one variable explains lots)
    #11: depend on a lot of factors
    mtry = mtry(range = c(1, 8))
  )


# define grid
rf_grid <- grid_regular(rf_params, levels = 5)


# workflow---
rf_workflow <-
  workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(song_recipe1) 

# tuning
rf_tune <-
  rf_workflow %>% 
  tune_grid(
    resamples = song_fold,
    grid = rf_grid
  )

show_notes(rf_tune)

# write out results
save(rf_tune, rf_workflow, file = "results/fit_rf.rda")
