# Fitting LM Models

# fitting the multinomial regression model and tuning penalty param
### load-pkgs ----
library(tidyverse)
library(tidymodels)
library(doMC)

# handle common conflicts
tidymodels_prefer()

### load mod-spec & data ----
load("model_prep/split_data.rda")
load("model_prep/recipes.rda")

# set up parallel processing!
detectCores()
registerDoMC(cores = 6)

### model-specs ----
set.seed(0116)
multinom_model <- multinom_reg(penalty = tune()) |>
  set_engine("nnet") |>
  set_mode("classification")

### workflows ----
mreg_wkflw_kitchen <- workflow() %>% 
  add_model(multinom_model) %>% 
  add_recipe(kitchen)

mreg_wkflw_scoring <- workflow() %>% 
  add_model(multinom_model) %>% 
  add_recipe(scoring_recipe)

mreg_wkflw_defense <- workflow() %>% 
  add_model(multinom_model) %>% 
  add_recipe(defense_recipe)

### grids ----
multireg_params <- extract_parameter_set_dials(multinom_model)

mreg_grid <- grid_regular(multireg_params, levels = 5)

### tuning ----
kitchen_mreg_tune <- tune_grid(
  mreg_wkflw_kitchen, 
  player_folds,
  mreg_grid,
  control = control_grid(save_workflow = TRUE),
  parallel_over = "everything")

scoring_mreg_tune <- tune_grid(
  mreg_wkflw_scoring, 
  player_folds,
  mreg_grid,
  control = control_grid(save_workflow = TRUE),
  parallel_over = "everything")

defense_mreg_tune <- tune_grid(
  mreg_wkflw_defense, 
  player_folds,
  mreg_grid,
  control = control_grid(save_workflow = TRUE),
  parallel_over = "everything")

### save results ----
save(kitchen_mreg_tune, scoring_mreg_tune, defense_mreg_tune,
     file = "results/tuned_mreg.rda")