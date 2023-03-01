library(tidyverse)
library(tidymodels)





game_logs <- data.table::fread("GameLogs.csv") %>% 
  select(-contains("ID")) %>% 
  janitor::clean_names()

game_logs %>% 
  dim()


game_logs$date <- lubridate::ymd(game_logs$date)




tibble(game_logs) %>% 
  timetk::filter_by_time(date, "2018", "2022") %>% 
  timetk::plot_time_series(date, attendance, .interactive = FALSE)



VIM::aggr(game_logs, col = c("skyblue","orange"))



na_prop <- function(col){
  sum(is.na(col))/length(col)
}
  
  

data.frame(prop = sapply(data.frame(game_logs), na_prop) ) %>% 
  select(prop) %>% 
  filter(prop <= .1) %>% 
  top_n(2)# note that putting more than 2 results in the inclusion
#of every variable since 0 is the next lowest
  
#prop
#completition_information 0.999448
#forfeit_information      1.000000

#not much data is missing 


#hist(game_logs$attendance)
#looks really good



cat <- lapply(game_logs, is.character) %>% 
  names()


data.frame(game_logs[,..cat]) %>% 
  unique()



set.seed(123)
GameLogs <- data.table::fread("~/Documents/GameLogs.csv") %>% 
  sample_n(500) %>% 
  select(-contains("ID")) %>% 
  janitor::clean_names() %>% 
  mutate(
    date = lubridate::ymd(date),
  )



not_num <- function(col){
  !is.numeric(col)
} 

cat %>%  select_if(GameLogs, not_num)


GameLogs[,select_if(GameLogs, not_num)] <- lapply(GameLogs[,select_if(GameLogs, not_num)], factor)



GameLogs %>% 
  glimpse()



set.seed(123)
gamelog_split <- initial_split(GameLogs, strata = attendance)
gamelog_train <- training(gamelog_split)
gamelog_test <- testing(gamelog_split)

gamelog_fold <- vfold_cv(gamelog_train, v = 10)



















