# Define and tune KNN
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
set.seed(1013)
knn_model <- nearest_neighbor(mode = "classification", 
                              neighbors = tune()) |> 
  set_engine("kknn")

# define workflows
knn_workflow_scoring <- workflow()  %>% 
  add_model(knn_model)  %>% 
  add_recipe(scoring_recipe) 

knn_workflow_defense <- workflow()  %>% 
  add_model(knn_model)  %>% 
  add_recipe(defense_recipe) 

knn_workflow_kitchen <- workflow()  %>% 
  add_model(knn_model)  %>% 
  add_recipe(kitchen) 
# get parameters and define tuning grid

knn_params <- extract_parameter_set_dials(knn_model) |>
  update(neighbors = neighbors(range = c(1, 20)))

knn_grid <- grid_regular(knn_params, levels = 20)

# tune models
tune_knn_scoring <- knn_workflow_scoring |>
  tune_grid(
    resamples = player_folds,
    grid = knn_grid,
    control = control_grid(save_workflow = TRUE),
    parallel_over = "resamples")

tune_knn_defense <- knn_workflow_defense |>
  tune_grid(
    resamples = player_folds,
    grid = knn_grid,
    control = control_grid(save_workflow = TRUE),
    parallel_over = "resamples"
  )

tune_knn_kitchen <- knn_workflow_kitchen  |>
  tune_grid(
    resamples = player_folds,
    grid = knn_grid,
    control = control_grid(save_workflow = TRUE),
    parallel_over = "resamples"
  )
# write out results
save(tune_knn_kitchen, tune_knn_scoring, tune_knn_defense, file = "results/tuned_knn.rda")