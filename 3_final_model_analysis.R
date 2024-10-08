# Final Model Selection & Analysis

#load packages
library(tidyverse)
library(tidymodels)
library(xgboost)
library(vip)
library(kknn)

#handle common conflicts
tidymodels_prefer()

#load required objects for model comparison/selection--------
load("results/fit_bt.rda")
load("results/fit_rf.rda")
load("results/fit_knn.rda")
load("results/fit_en.rda")
#load required data for final model analysis----------
# need training and test set, should load the split
load("initial_setup/song_split.rda")

song_train <- song_split %>% training()
song_test <- song_split %>% testing()

#autoplot
p1 <- autoplot(rf_tune, metric = "rmse")
p2<- autoplot(knn_tune, metric = "rmse")
p3 <- autoplot(bt_tune, metric = "rmse")
p4 <- autoplot(en_tune, metric = "rmse")

p1
p2
p3
p4
library(cowplot)

# # Arrange the top two plots in a row
# top_row <- plot_grid(p1, p2, nrow = 1)
# 
# # Arrange the bottom two plots in a row
# bottom_row <- plot_grid(p3, p4, nrow = 1)
# 
# # Arrange the two rows in a column
# tune_visualize <- plot_grid(top_row, bottom_row, ncol = 1, align = "v")

#setup
model_db <- tibble(
  model_type = c("boosted_tree", "rand_forest", "knn", "elastic_net"),
  tune_results = list(bt_tune, rf_tune, knn_tune, en_tune),
  metric_est = map(tune_results, collect_metrics),
  best_model = map(tune_results, ~ select_best(.x, .metric = "rmse"))
)  


model_db %>% 
  unnest(metric_est) %>% 
  filter(.metric == "rmse") %>% 
  select(model_type, mean, std_err) %>% 
  arrange(mean) %>% 
  View()
#best model is a boosted tree


#learn rate is high
#focusing on narrow, higher learn rate is appropriate for this data
# select_best(rf_tune, metric = "rmse")
# select_best(bt_tune, metric = "rmse")
# select_best(knn_tune, metric = "rmse")
# select_best(en_tune, metric = "rmse")

rf_best <- show_best(rf_tune, metric = "rmse", 1)
bt_best <- show_best(bt_tune, metric = "rmse", 1)
knn_best <- show_best(knn_tune, metric = "rmse", 1)
en_best <- show_best(en_tune, metric = "rmse", 1)


write_csv(rf_best, "best parameters/rf_best.csv")
write_csv(bt_best, "best parameters/bt_best.csv")
write_csv(knn_best, "best parameters/knn_best.csv")
write_csv(en_best, "best parameters/en_best.csv")

#Winning: rf!
rf_workflow_tuned <- rf_workflow %>% 
  finalize_workflow(select_best(rf_tune, metric = "rmse"))

rf_results <- fit(rf_workflow_tuned, song_train)

song_metric <- metric_set(rmse)

rf_pred <- predict(rf_results, new_data = song_test) %>% 
  bind_cols(song_test %>% select(popularity)) %>% 
  song_metric(truth = popularity, estimate = .pred)

rf_pred

write_csv(rf_pred, "winning model/rf_pred.csv")

# visualize the results
predict_result <- rf_results %>% 
  predict(new_data = song_test) %>% 
  bind_cols(song_test %>% select(popularity)) %>% 
  mutate(res = popularity - .pred)

predict_result

# create a scatter plot of predicted vs. observed values with a line of identity
p1 <- ggplot(predict_result, aes(x = popularity, y = .pred)) +
  geom_point(size = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", size = 1) +
  geom_errorbar(aes(ymin = popularity - res, ymax = popularity), width = 0.1) +
  labs(x = "Observed Values", y = "Predicted Values") +
  ggtitle("Predicted vs. Observed Values")

# create a scatter plot of residuals
p2 <- ggplot(predict_result, aes(x = .pred, y = res)) +
  geom_point(size = 0.3) +
  labs(x = "Predicted Values", y = "Residuals") +
  ggtitle("Residual Plot")

p1

p2

#------- The performance metrics on testing data 
library(yardstick)
library(dplyr)

# Compute performance metrics on testing data using random_forest model

# predict_result <- rf_results %>% 
#   predict(new_data = song_test) %>% 
#   bind_cols(song_test %>% select(popularity)) %>% 
#   mutate(res = popularity - .pred)
rf_popularity <- 
  predict(rf_results, new_data = song_test) %>% 
  bind_cols(song_test %>% select(popularity))

rf_popularity

rf_test_metrics <- rf_popularity %>%
  metrics(
    truth = popularity,
    estimate = .pred
  )


# View performance metrics on testing data
rf_test_metrics

write_csv(rf_test_metrics, "winning model/rf_test_metrics.csv")
