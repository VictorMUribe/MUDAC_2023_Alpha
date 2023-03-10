library(tidyverse)
library(tidymodels)








lf_ump_name, rf_ump_name, saving_pitcher_name

set.seed(123)
GameLogs <- data.table::fread("/Users/victoruribe/Documents/GameLogs.csv") %>% 
  sample_n(25000) %>% 
  select(-contains(c("ID", "name"))) %>% 
  janitor::clean_names() %>% 
  select(-c(completition_information, forfeit_information, protest_information, 
            additional_information, acquisition_information)) %>% 
  mutate(
    date = lubridate::ymd(date),
    dayof_week = factor(dayof_week),
    visiting_team = factor(visiting_team),
    visiting_team_league = factor(visiting_team_league),
    home_team = factor(home_team),
    home_team_league = factor(home_team_league),
    day_night = factor(day_night),
    #home_plate_ump_name = factor(home_plate_ump_name),
    #x1b_ump_name = factor(x1b_ump_name),
    #x2b_ump_name = factor(x2b_ump_name),
    #x3b_ump_name = factor(x3b_ump_name),
    #visiting_team_manager_name = factor(visiting_team_manager_name),
    #home_team_manager_name = factor(home_team_manager_name),
    #winning_pitcher_name = factor(winning_pitcher_name),
    #losing_pitcher_name = factor(losing_pitcher_name),
    #saving_pitcher_name = factor(saving_pitcher_name),
    #game_winning_rbi_batter_name = factor(game_winning_rbi_batter_name),
    #visiting_team_starting_pitcher_name = factor(visiting_team_starting_pitcher_name),
    #home_team_starting_pitcher_name = factor(home_team_starting_pitcher_name),
    #visiting_team_player1_name = factor(visiting_team_player1_name),
    #visiting_team_player2_name = factor(visiting_team_player2_name),
    #visiting_team_player3_name = factor(visiting_team_player3_name),
    #visiting_team_player4_name = factor(visiting_team_player4_name),
    #visiting_team_player5_name = factor(visiting_team_player5_name),
    #visiting_team_player6_name = factor(visiting_team_player6_name),
    #visiting_team_player7_name = factor(visiting_team_player7_name),
    #visiting_team_player8_name = factor(visiting_team_player8_name),
    #visiting_team_player9_name = factor(visiting_team_player9_name),
    visiting_team_player1_position = factor(visiting_team_player1_position),
    visiting_team_player2_position = factor(visiting_team_player2_position),
    visiting_team_player3_position = factor(visiting_team_player3_position),
    visiting_team_player4_position = factor(visiting_team_player4_position),
    visiting_team_player5_position = factor(visiting_team_player5_position),
    visiting_team_player6_position = factor(visiting_team_player6_position),
    visiting_team_player7_position = factor(visiting_team_player7_position),
    visiting_team_player8_position = factor(visiting_team_player8_position),
    visiting_team_player9_position = factor(visiting_team_player9_position),
    #home_team_player1_name = factor(home_team_player1_name),
    #home_team_player2_name = factor(home_team_player2_name),
    #home_team_player3_name = factor(home_team_player3_name),
    #home_team_player4_name = factor(home_team_player4_name),
    #home_team_player5_name = factor(home_team_player5_name),
    #home_team_player6_name = factor(home_team_player6_name),
    #home_team_player7_name = factor(home_team_player7_name),
    #home_team_player8_name = factor(home_team_player8_name),
    #home_team_player9_name = factor(home_team_player9_name),
    home_team_player1_position = factor(home_team_player1_position),
    home_team_player2_position = factor(home_team_player2_position),
    home_team_player3_position = factor(home_team_player3_position),
    home_team_player4_position = factor(home_team_player4_position),
    home_team_player5_position = factor(home_team_player5_position),
    home_team_player6_position = factor(home_team_player6_position),
    home_team_player7_position = factor(home_team_player7_position),
    home_team_player8_position = factor(home_team_player8_position),
    home_team_player9_position = factor(home_team_player9_position)
  )


# replaces (none) with NA as it should
GameLogs[,2:ncol(GameLogs)] <- as.data.frame(GameLogs[,2:ncol(GameLogs)]) %>%
  mutate_all(~na_if(., "(none)"))


#GameLogs %>% 
#  glimpse()




VIM::aggr(GameLogs, col = c("skyblue","orange"))



na_prop <- function(col){
  sum(is.na(col))/length(col)
}




data.frame(prop = sapply(data.frame(GameLogs), na_prop) ) %>% 
  select(prop) %>% 
  filter(prop > 0)


#prop
#completition_information 0.999448
#forfeit_information      1.000000

#not much data is missing 



set.seed(123)# note that 2:119 removes the dates
gamelog_split <- initial_split(GameLogs[, 2:ncol(GameLogs)], strata = attendance)

gamelog_train <- training(gamelog_split)
gamelog_test <- testing(gamelog_split)

gamelog_fold <- vfold_cv(gamelog_train, v = 10)






gamelog_train$numberof_games^2 %>% density() %>% plot()






