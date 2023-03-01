library(tidyverse)
library(tidymodels)










set.seed(123)
GameLogs <- data.table::fread("GameLogs.csv") %>% 
  sample_n(15000) %>% 
  select(-contains("ID")) %>% 
  janitor::clean_names() %>% 
  select(-c(completition_information, forfeit_information, protest_information, 
            lf_ump_name, rf_ump_name, additional_information, acquisition_information)) %>% 
  mutate(
    date = lubridate::ymd(date),
    dayof_week = factor(dayof_week),
    visiting_team = factor(visiting_team),
    visiting_team_league = factor(visiting_team_league),
    home_team = factor(home_team),
    home_team_league = factor(home_team_league),
    day_night = factor(day_night),
    home_plate_ump_name = factor(home_plate_ump_name),
    x1b_ump_name = factor(x1b_ump_name),
    x2b_ump_name = factor(x2b_ump_name),
    x3b_ump_name = factor(x3b_ump_name),
    visiting_team_manager_name = factor(visiting_team_manager_name),
    home_team_manager_name = factor(home_team_manager_name),
    winning_pitcher_name = factor(winning_pitcher_name),
    losing_pitcher_name = factor(losing_pitcher_name),
    saving_pitcher_name = factor(saving_pitcher_name),
    game_winning_rbi_batter_name = factor(game_winning_rbi_batter_name),
    visiting_team_starting_pitcher_name = factor(visiting_team_starting_pitcher_name),
    home_team_starting_pitcher_name = factor(home_team_starting_pitcher_name),
    visiting_team_player1_name = factor(visiting_team_player1_name),
    visiting_team_player2_name = factor(visiting_team_player2_name),
    visiting_team_player3_name = factor(visiting_team_player3_name),
    visiting_team_player4_name = factor(visiting_team_player4_name),
    visiting_team_player5_name = factor(visiting_team_player5_name),
    visiting_team_player6_name = factor(visiting_team_player6_name),
    visiting_team_player7_name = factor(visiting_team_player7_name),
    visiting_team_player8_name = factor(visiting_team_player8_name),
    visiting_team_player9_name = factor(visiting_team_player9_name),
    home_team_player1_name = factor(home_team_player1_name),
    home_team_player2_name = factor(home_team_player2_name),
    home_team_player3_name = factor(home_team_player3_name),
    home_team_player4_name = factor(home_team_player4_name),
    home_team_player5_name = factor(home_team_player5_name),
    home_team_player6_name = factor(home_team_player6_name),
    home_team_player7_name = factor(home_team_player7_name),
    home_team_player8_name = factor(home_team_player8_name),
    home_team_player9_name = factor(home_team_player9_name)
  )




#GameLogs %>% 
#  glimpse()




VIM::aggr(GameLogs, col = c("skyblue","orange"))



na_prop <- function(col){
  sum(is.na(col))/length(col)
}



data.frame(prop = sapply(data.frame(GameLogs), na_prop) ) %>% 
  select(prop) %>% 
  filter(prop <= .1) %>% 
  top_n(2)# note that putting more than 2 results in the inclusion
#of every variable since 0 is the next lowest

#prop
#completition_information 0.999448
#forfeit_information      1.000000

#not much data is missing 



set.seed(123)
gamelog_split <- initial_split(GameLogs, strata = attendance)

gamelog_train <- training(gamelog_split)
gamelog_test <- testing(gamelog_split)

gamelog_fold <- vfold_cv(gamelog_train, v = 10)



















