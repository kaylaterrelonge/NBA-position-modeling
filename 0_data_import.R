# Data Import & Cleaning

# the following script will read in the raw data and clean it 
# random processes are NOT used in script

### load-pkgs ----
library(tidyverse)
library(tidymodels)

# handle common conflicts
tidymodels_prefer()

### load-data & cleaning ----

# using for loop to collect files 

# gathering all .csv files from folder
player_csv <- dir("data/raw_data_players/", pattern = "\\.csv$", full.names = TRUE)

# reading in files via for loop
player_files <-list()

for(i in seq_along(player_csv)){
  player_files[[i]] <- read_csv(player_csv[[i]])
}

# combining all data into one structure
players <- bind_rows(player_files)

# cleaning importing issues 
players <- players %>%
  rowwise() %>% 
  # removing team name from player name
  mutate(team = str_extract_all(name, "[A-Z]{2,4}$") %>%
           unlist() %>%
           paste0(collapse = ""), 
         name = str_split(name,"[A-Z]{2,4}$") %>% 
           unlist() %>%
           paste0(collapse = ""), 
         # separating the team codes for players who played for more than one team
         sec_team = str_extract_all(name, "[A-Z]{2,4}/$") %>% 
           unlist() %>%
           paste0(collapse = ""),
         # removing those team codes from the names
         name = str_split(name,"[A-Z]{2,4}/$") %>% 
           unlist() %>%
           paste0(collapse = "")) %>% 
  # removing another unwanted string
  mutate(sec_team = str_split(sec_team, "/") %>% 
           unlist() %>%
           paste0(collapse = "")) %>% 
  # tidying data to get teams into one col
  pivot_longer(cols = ends_with("team"), names_to = "all_teams") %>% 
  # filtering out rows with no team entry, meaning player only played for one team
  filter(value != "") %>% 
  # changing col names 
  mutate(team = value,
         team_class = all_teams) %>% 
  select(-c(value, all_teams)) %>% 
  janitor::clean_names()

# read out processed data
write_rds(players, file = "data/processed_data/players_processed.rds")
