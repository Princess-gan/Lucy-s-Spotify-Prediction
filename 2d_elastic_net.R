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

# en----
en_spec <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Check tuning parameters
# hardhat::extract_parameter_set_dials(en_spec)

# Set tuning parameters
en_params <- hardhat::extract_parameter_set_dials(en_spec) %>%
  update(
    penalty = penalty(range = c(0, 1)),
    mixture = mixture(range = c(0, 1))
  )

# Define grid
en_grid <- grid_regular(en_params, levels = 5)

# Define workflow
en_workflow <- workflow() %>%
  add_recipe(song_recipe1) %>%
  add_model(en_spec)

#Tune the model using cross-validation
en_tune <- en_workflow %>%
  tune_grid(
    resamples = song_fold,
    grid = en_grid,
    control = control_grid(save_pred = TRUE, verbose = TRUE)
  )

show_notes(en_tune)
#Write out results
save(en_tune, en_workflow, file = "results/fit_en.rda")
