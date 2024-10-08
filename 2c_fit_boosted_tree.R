# Boosted tree tuning----------------------------------------------------------

# load packages
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)

# handle common conflicts
tidymodels_prefer()

#set up parallel processing
library(doMC)
registerDoMC(cores = 10)

#set seed
set.seed(2468)

# load any required objects-----------------------------
load("initial_setup/tuning_setup.rda")

# define model-------
bt_spec <-
  boost_tree(
    #if we don't set them up with tune(), we get a default value
    #if we set them up with tune(), we get a default range of value 
    mtry = tune(), 
    learn_rate = tune(), 
    min_n = tune()
    ) %>%
  #how important is our model to predictions, help us analyze what variables in our model are important
  set_engine('xgboost', importance = "impurity") %>%
  set_mode('regression')

#check tuning parameters: mtry, min_n, learn_rate
#it is saying: it has default setting for min_n and learn_rate, but mtry needs to be taken care of
# to check what default values for learn_rate are, type "learn_rate()" in the console
# type mtry() in the consol, get Range: [1, ?]
# hardhat::extract_parameter_set_dials(bt_spec)


#Setup tuning parameter
bt_params <- hardhat::extract_parameter_set_dials(bt_spec) %>% 
  update(
    mtry = mtry(range = c(1, 8)),
    learn_rate = learn_rate(range = c(-5, -0.2))
  )

#define grid
#levels = 5: 5 values for each level to try
# 5* 5 * 5 = 125 models fit 50 times
bt_grid <- grid_regular(bt_params, levels = 5)

#workflow-------
bt_workflow <- 
  workflow() %>% 
  add_model(bt_spec) %>% 
  add_recipe(song_recipe1) 

# tuning
bt_tune <- 
  bt_workflow %>% 
  tune_grid(
    resamples = song_fold,
    grid = bt_grid
  )
show_notes(bt_tune)

#Write out results
save(bt_tune, bt_workflow, file = "results/fit_bt.rda")

