## STETUP ----------
library(tidyverse)
library(caret)
library(rsample)
library(recipes)

international_matches <- read.csv(file = "~/Desktop/work/data/r/kaggle/fifa_wc_2022/international_matches.csv")


## DATA PREP ----------
all_matches <- international_matches %>%
  filter(date >= '2006-01-01', tournament %in% c("AFC Asian Cup", "African Cup of Nations", "CONCACAF Nations League", "Gold Cup",
                                                 "Copa América", "FIFA World Cup", "FIFA World Cup qualification", "UEFA Euro",
                                                 "UEFA Euro qualification", "UEFA Nations League")) %>%
  mutate(fifa_rank_dif = away_team_fifa_rank - home_team_fifa_rank,
         goalkeeper_dif = home_team_goalkeeper_score - away_team_goalkeeper_score,
         defense_dif = home_team_mean_defense_score - away_team_mean_defense_score,
         midfield_dif = home_team_mean_midfield_score - away_team_mean_midfield_score,
         attack_dif = home_team_mean_offense_score - away_team_mean_offense_score)

wc_matches <- all_matches %>%
  filter(tournament == "FIFA World Cup")

underdog_matches <- all_matches %>%
  filter(abs(fifa_rank_dif) > 10)

result_pred_data <- all_matches %>%
  drop_na() %>%
  select(c(home_team_result, fifa_rank_dif, goalkeeper_dif, defense_dif, midfield_dif, attack_dif))


## RESULT PREDICITON MODEL ----------
# split training and testing sets
result_pred <- result_pred_data %>%
  mutate_if(is.ordered, factor, ordered = FALSE)
set.seed(123)
pred_split <- initial_split(result_pred, prop = .7, strata = "home_team_result")
pred_train <- training(pred_split)

# create blueprint
blueprint <- recipe(home_team_result ~ ., data = pred_train) %>%
  step_nzv(all_nominal()) %>%
  step_integer(contains("dif")) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>%
  step_center(all_numeric(), -all_outcomes()) %>%
  step_scale(all_numeric(), -all_outcomes())

# create resampling method
cv <- trainControl(
  method = "repeatedcv", 
  number = 10, 
  repeats = 5,
  classProbs = TRUE,                 
  summaryFunction = defaultSummary
)

# hyperperameter grid search
hyper_grid <- expand.grid(
  k = floor(seq(1, nrow(pred_train)/3, length.out = 20))
)

# fit the knn model
knn_grid <- train(
  blueprint, 
  data = pred_train, 
  method = "knn", 
  trControl = cv, 
  tuneGrid = hyper_grid,
  metric = "ROC"
)

ggplot(knn_grid)
