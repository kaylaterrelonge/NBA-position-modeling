# Initial Set Up & Data Splitting

# the cleaned data will be split and prepared for modeling
# random processes do occur in this script, seed will be set.

### load-pkgs ----
library(tidyverse)
library(tidymodels)

# handle common conflicts 
tidymodels_prefer()

### load-data ----
players <- read_rds(file = "data/processed_data/players_processed.rds") %>% 
  filter(pos != 'GF') %>% 
  filter(pos != 'NA') %>% 
  mutate(pos = as.factor(pos))

# quick inspection of target variable will be done on training data
# will used stratification

### initial split ----
# set seed
set.seed(3590)

player_split <- initial_split(players, prop = 4/5, 
                          strata = pos)

player_training <- training(player_split)
player_testing <- testing(player_split)

# folding the data for resampling

player_folds <- vfold_cv(player_training, 
                     v = 5, repeats = 3, strata = pos)

# write out results 
save(player_split, player_testing, player_training, player_folds,
     file = "model_prep/split_data.rda")
