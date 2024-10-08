#load packages
library(tidyverse)
library(tidymodels)
library(rsample)
library(janitor)
library(kableExtra)
library(ggplot2)

#handle common conflicts
tidymodels_prefer()

#set seed
set.seed(2023)

#load data
song_data <- read_csv("data/clean data/song_data_cleaned.csv") %>%
  #mutate(artists = as.factor(artists)) %>% 
  #mutate(genres = as.factor(genres)) %>% 
  janitor::clean_names()

view(song_data)

#Data inspection: inspect to make sure there is no massive systemic failure, not doing an EDA
song_data %>% 
  skimr::skim_without_charts()

#check target variables
p1 <- ggplot(song_data, aes(popularity))+
  geom_density()+
  theme_minimal()

p2 <- ggplot(song_data, aes(popularity))+
  geom_boxplot()+
  theme_void()

p2/p1 +
  plot_layout(height = c(1,5))
#well-behaved target variables, looking great 

#initial split and resamples (cross fold)-----
song_split <- song_data %>%
  initial_split(prop = 0.7, strata = popularity)

song_train <- training(song_split)
song_test <- testing(song_split)

# repeated V-fold cross-validation
song_fold <- song_train %>% 
  vfold_cv(v = 10, repeats = 5, strata = popularity)

#set up bas recipe/feature engineering---
song_recipe1 <- recipe(popularity ~ danceability+ duration+ liveness+
                        loudness +instrumentalness +tempo +mode + count , data = song_train) %>% 
  #step_rm(c(genre_words,artists)) %>% 
  # higher place/ update role
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 
# put normalize after zv to prevent the case that one variable has same value and 0 variance, so cant be divided correctly

#save out necessary outpus------

# items needed for tuning or resample fitting
save(song_fold, song_recipe1, file = "initial_setup/tuning_setup.rda")

#items needed for final model fitting & analysis
save(song_split, file = "initial_setup/song_split.rda")

song_recipe1 %>% 
  prep() %>% 
  bake(new_data = NULL) 

# build null model - check slides.html!!!!

# string replace, - no space square bracket as NA value
# unpack the genre value, 
# remove square brackets, 
# splits on comma, 

# response value: (possibly) remove zeros
# 2 stage model
