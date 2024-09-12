# Define and Tune RF
# parallel processing occurs in this script

## load packages ----
library(tidyverse)
library(tidymodels)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# load folds data
load("model_prep/split_data.rda")

# load preprocessing/feature engineering/recipes
load("model_prep/recipes.rda")
################################################
# set up parallel processing!
detectCores()
registerDoMC(cores = 6)

################################################
# model specifications
set.seed(1234)

rf_model <- rand_forest(mode = "classification",
                        min_n = tune(),
                        mtry = tune()) %>% 
  set_engine("ranger")

# define workflows
rf_wkflw_defense <- workflow()  %>% 
  add_model(rf_model)  %>% 
  add_recipe(defense_recipe) 

rf_wkflw_scoring <- workflow()  %>% 
  add_model(rf_model)  %>% 
  add_recipe(scoring_recipe) 

rf_wkflw_kitchen <- workflow()  %>% 
  add_model(rf_model)  %>% 
  add_recipe(kitchen) 

# get parameters and define tuning grid
rf_params <- extract_parameter_set_dials(rf_model) |>
  # 17 predictor variables -> 70% = 12
  update(mtry = mtry(range = c(1, 8)))

rf_grid <- grid_regular(rf_params, levels = 5)

# tune models
tune_rf_scoring <-rf_wkflw_scoring |>
  tune_grid(
    resamples = player_folds,
    grid = rf_grid,
    control = control_grid(save_workflow = TRUE)
  )

tune_rf_defense <-rf_wkflw_defense |>
  tune_grid(
    resamples = player_folds,
    grid = rf_grid,
    control = control_grid(save_workflow = TRUE)
  )

tune_rf_kitchen <-rf_wkflw_kitchen |>
  tune_grid(
    resamples = player_folds,
    grid = rf_grid,
    control = control_grid(save_workflow = TRUE)
  )
# write out results
save(tune_rf_kitchen, tune_rf_defense, tune_rf_scoring, file = "results/tuned_rf.rda")
save(rf_wkflw_kitchen, file = "results/final_wkflw.rda")
