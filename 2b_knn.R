# knn tuning----------------------------------------------------------

# load packages
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(kknn)

# handle common conflicts
tidymodels_prefer()

#set seed
set.seed(2023)

# load any required objects-----------------------------
load("initial_setup/tuning_setup.rda")

# define model-------
knn_spec <-
  nearest_neighbor(
    neighbors = tune() 
  )%>% 
  set_engine("kknn") %>% 
  set_mode("regression")

#check tuning parameters: nearest_neighbors

#set up tuning
knn_params <- hardhat::extract_parameter_set_dials(knn_spec) %>% 
  update(
    neighbors = neighbors(range = c(1, 20))
  )

#define grid
#levels = 5: 5 values for each level to try
# 5* 5 * 5 = 125 models fit 50 times
knn_grid <- grid_regular(knn_params, levels = 5)

#workflow-------
knn_workflow <- 
  workflow() %>% 
  add_model(knn_spec) %>% 
  add_recipe(carseats_recipe) 

# tuning
knn_tune <- 
  knn_workflow %>% 
  tune_grid(
    resamples = carseats_fold,
    grid = knn_grid
  )

show_notes(rf_tune)
#Write out results
save(knn_tune, knn_workflow, file = "results/fit_knn.rda")
