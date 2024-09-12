# Recipes and feature engineering

# the following script will be used to prepare recipes for modeling

## load packages ----

library(tidyverse)
library(tidymodels)

# handling common conflicts
tidymodels_prefer()

# load in split data 
load("model_prep/split_data.rda")

# per the eda we know we need to change the outcome variable to a categorical
# remove id vars
# filter out two levels from the outcome variable
kitchen <- recipe(pos ~ ., data = player_training) |> 
  step_rm(name, team, team_class, season, per) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

kitchen %>% 
  prep() %>% 
  bake(player_training)

scoring_recipe <- recipe(pos ~ x3p_percent + fg_percent +
                                ft_percent + pts + min , data = player_training) |> 
  step_dummy(all_nominal_predictors()) |>
  step_interact(~fg_percent:x3p_percent) |>
  step_interact(~fg_percent:min) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

scoring_recipe %>% 
  prep() %>% 
  bake(player_training)

# feature engineered recipe # 1
defense_recipe <- recipe(pos ~ rank + gp + to + reb + ast + blk + stl,
                      data = player_training) |> 
  step_dummy(all_nominal_predictors()) |>
  step_interact(~gp:rank) |>
  step_interact(~reb:ast) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

defense_recipe %>% 
  prep() %>% 
  bake(player_training)

# save out your recipes
save(kitchen, scoring_recipe, defense_recipe, file = "model_prep/recipes.rda")
