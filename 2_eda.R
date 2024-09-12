# EDA 

# conducted solely on training data set created from splitting
# the following script will be used to explore the data and better understand it

## load-pkgs ----
library(tidyverse)
library(corrplot)

## load training data ----
load("model_prep/split_data.rda")

### initial peek at target variable
ggplot(data = player_training, mapping = aes(x = pos)) + 
  geom_bar() +
  theme_minimal() +
  labs(title = "Player Positions Proportional Breakdown",
       x = "Player Position",
       y = "Count",
       caption = "Source: ESPN")

ggsave("figures/target_var.png")

# there is are two levels where there are 0 observations, will remove

# lets start by looking at the relationship between points and position
player_training <- player_training %>% 
  filter(pos != 'GF') %>% 
  filter(pos != 'NA')

player_training %>% 
  skimr::skim_without_charts()

ggplot(player_training, mapping = aes(x = pos, y = pts)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Player Point Distribution by Position",
       x = "Player Position",
       y = "Point Distribution",
       caption = "Source: ESPN")
# From this figure we can see what is a typical point performance for each position
ggsave("figures/point_pos_dist.png")

ggplot(player_training, mapping = aes(x = pos, y = fg_percent)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Player Field Goal Percentage Distribution by Position",
       x = "Player Position",
       y = "Field Goal Percentage",
       caption = "Source: ESPN")
# looking at field goal
ggsave("figures/fg_pos_dist.png")


# looking at three point pct 
ggplot(player_training, mapping = aes(x = pos, y = x3p_percent)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Player Three Point Percentage Distribution by Position",
       x = "Player Position",
       y = "Three Point Percentage",
       caption = "Source: ESPN")

ggsave("figures/3p_pos_dist.png")

# looking at free throw pct
ggplot(player_training, mapping = aes(x = pos, y = ft_percent)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Player Free Throw Percentage Distribution by Position",
       x = "Player Position",
       y = "Free Throw Percentage",
       caption = "Source: ESPN")

ggsave("figures/ft_pos_dist.png")

# looking at blks  
ggplot(player_training, mapping = aes(x = blk, color = pos, fill = pos)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ pos) +
  theme_minimal() +
  labs(title = "Player Average Blocks Distribution by Position",
       x = "Average Blocks",
       y = "Density",
       caption = "Source: ESPN")

ggsave("figures/point_blks_dist.png")

# looking at ast
ggplot(player_training, mapping = aes(x = ast, color = pos, fill = pos)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ pos) +
  theme_minimal() +
  labs(title = "Player Average Assists Distribution by Position",
       y = "Density",
       x = "Average Assists",
       caption = "Source: ESPN")

ggsave("figures/point_ast_dist.png")

# pts per pos
ggplot(player_training, mapping = aes(x = pts, y = fg_percent, color = pos)) +
  geom_jitter(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Player Average Points vs. Field Goal Percentage by Position",
       x = "Points",
       y = "Field Goal Percentage",
       caption = "Source: ESPN")

ggsave("figures/point_fg_pos.png")
