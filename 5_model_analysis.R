# Model Analysis & Final Fitting

## load-pkgs ----
library(tidyverse)
library(tidymodels)

## load data & info ----
load("model_prep/split_data.rda")
load("results/tuned_knn.rda")
load("results/tuned_mreg.rda")
load("results/tuned_rf.rda")
load("results/final_wkflw.rda")

####################################################
# Find the optimal parameters from each recipe to create a table
show_best(tune_rf_kitchen, metric = "roc_auc")
show_best(tune_rf_scoring, metric = "roc_auc")
show_best(tune_rf_defense, metric = "roc_auc")
show_best(tune_knn_kitchen, metric = "roc_auc")
show_best(tune_knn_scoring, metric = "roc_auc")
show_best(tune_knn_defense, metric = "roc_auc")
show_best(kitchen_mreg_tune, metric = "roc_auc")
show_best(scoring_mreg_tune, metric = "roc_auc")
show_best(defense_mreg_tune, metric = "roc_auc")

####################################################
# autoplots of tuning params 
autoplot(tune_rf_kitchen, metric = "accuracy") +
  labs(title = "Tuning Parameter of Best Performing Rf Model")

ggsave("figures/rf_tune.png")

autoplot(tune_knn_kitchen, metric = "accuracy") +
  labs(title = "Tuning Parameter of Best Performing KNN Model")

ggsave("figures/knn_tune.png")


autoplot(kitchen_mreg_tune, metric = "accuracy") +
  labs(title = "Tuning Parameter of Best Performing MREG Model")

ggsave("figures/mreg_tune.png")



# Create table of results
kitchen_rf_best <- show_best(tune_rf_kitchen, metric = "accuracy")[1, ]
scoring_rf_best <- show_best(tune_rf_scoring, metric = "accuracy")[1, ]
defense_rf_best <- show_best(tune_rf_defense, metric = "accuracy")[1, ]
kitchen_knn_best <- show_best(tune_knn_kitchen, metric = "accuracy")[1, ]
scoring_knn_best <- show_best(tune_knn_scoring, metric = "accuracy")[1, ]
defense_knn_best<- show_best(tune_knn_defense, metric = "accuracy")[1, ]
kitchen_mreg_best <- show_best(kitchen_mreg_tune, metric = "accuracy")[1, ]
scoring_mreg_best <- show_best(scoring_mreg_tune, metric = "accuracy")[1, ]
defense_mreg_best <- show_best(defense_mreg_tune, metric = "accuracy")[1, ]

top_accuracy <- tibble(model = c("RF", "RF", "RF", 
                                "KNN", "KNN", "KNN",
                                "MReg", "MReg", "MReg"),
                      recipe = c("kitchen", "scoring", "defense",
                                 "kitchen", "scoring", "defense",
                                 "kitchen", "scoring", "defense"),
                   accuracy = c(kitchen_rf_best$mean, scoring_rf_best$mean, 
                            defense_rf_best$mean,
                            kitchen_knn_best$mean, scoring_knn_best$mean,
                            defense_knn_best$mean,
                            kitchen_mreg_best$mean, scoring_mreg_best$mean,
                            defense_mreg_best$mean),
                   se = c(kitchen_rf_best$std_err, scoring_rf_best$std_err, 
                          defense_rf_best$std_err,
                          kitchen_knn_best$std_err, scoring_knn_best$std_err,
                          defense_knn_best$std_err,
                          kitchen_mreg_best$std_err, scoring_mreg_best$std_err,
                          defense_mreg_best$std_err),
                   n = c(kitchen_rf_best$n, scoring_rf_best$n, 
                         defense_rf_best$n,
                         kitchen_knn_best$n, scoring_knn_best$n,
                         defense_knn_best$n,
                         kitchen_mreg_best$n, scoring_mreg_best$n,
                         defense_mreg_best$n)
                   )

tuning_params <- tibble(model = c("RF", "RF", "RF", 
                                  "RF", "RF", "RF", 
                                  "KNN", "KNN", "KNN",
                                  "MReg", "MReg", "MReg"),
                        recipe = c("kitchen", "scoring", "defense",
                                   "kitchen", "scoring", "defense",
                                   "kitchen", "scoring", "defense",
                                   "kitchen", "scoring", "defense"),
                        accuracy = c(kitchen_rf_best$mean, scoring_rf_best$mean, 
                                     defense_rf_best$mean,
                                     kitchen_rf_best$mean, scoring_rf_best$mean, 
                                     defense_rf_best$mean,
                                     kitchen_knn_best$mean, scoring_knn_best$mean,
                                     defense_knn_best$mean,
                                     kitchen_mreg_best$mean, scoring_mreg_best$mean,
                                     defense_mreg_best$mean),
                        
                       tuning_param = c("mtry", "min_n",
                                                              "mtry", "min_n",
                                                             "mtry", "min_n",
                                                              "neighbors", "neighbors",
                                                              "neighbors",
                                                              "penalty",
                                                              "penalty",
                                                              "penalty"),
                       value = c(kitchen_rf_best$mtry, kitchen_rf_best$min_n,
                                 scoring_rf_best$mtry,  scoring_rf_best$min_n,
                                defense_rf_best$mtry, defense_rf_best$min_n,
                                 kitchen_knn_best$neighbors, scoring_knn_best$neighbors,
                                defense_knn_best$neighbors,
                                kitchen_mreg_best$penalty, scoring_mreg_best$penalty,
                               defense_mreg_best$penalty)
)

value = c(kitchen_rf_best$mtry, kitchen_rf_best$min_n,
          scoring_rf_best$mtry,  scoring_rf_best$min_n,
          defense_rf_best$mtry, defense_rf_best$min_n,
          kitchen_knn_best$neighbors, scoring_knn_best$neighbors,
          defense_knn_best$neighbors,
          kitchen_mreg_best$penalty, scoring_mreg_best$penalty,
          defense_mreg_best$penalty)

tuning_params %>% 
  kableExtra::kable(title = "Best Models Tuning Parameter")

# the random forest model with the kitchen recipe is the best performing model

### final-fit ----
# finalizing best model
rf_final <- rf_wkflw_kitchen %>% 
  finalize_workflow(select_best(tune_rf_kitchen, metric = "accuracy"))

rf_results <- fit(rf_final, player_training)

### predicting on testing ----
# Predict on the test data and evaluate performance
player_pred <- predict(rf_results, player_testing) %>% 
  bind_cols(player_testing %>%  
              select(pos))

accuracy(player_pred, pos, .pred_class)

# confusion matrix
player_pred %>% 
  conf_mat(pos, .pred_class)

player_pred %>% 
  conf_mat(pos, .pred_class) %>% 
  autoplot(type = "heatmap") +
  labs(title = "The Accuracy of Player Position Predictions",
       subtitle = "With a Random Forest Model",
       x = "True Player Position",
       y = "Predicted Player Position")

ggsave("figures/final_fit.png")

