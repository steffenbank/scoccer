
# Dependencies ------------------------------------------------------------
library(dplyr)
library(magrittr)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(scales)
library(ggplot2)
library(readxl)
library(httr)

select <- dplyr::select


# current score -----------------------------------------------------------
table_sim_current_standing <- function(year_input, league_input, at_time_date) {
  
    # home
    scoccer::sco_acquire(year_input,league_input) %>% 
      filter(date < at_time_date) %>% 
      mutate(dummy = 1) %>%
      group_by(team = hometeam) %>%
      summarise(goals_scored_home = sum(fthg),
                goals_conced_home = sum(ftag),
                games_played_home = n(),
                win_home = sum(dummy[fthg > ftag]),
                draw_home = sum(dummy[fthg == ftag]),
                loss_home = sum(dummy[fthg < ftag]),
                points_home = win_home * 3 + draw_home * 1) %>% group_by(team) -> home
  
  # away
  scoccer::sco_acquire(year_input,league_input) %>% 
    filter(date < at_time_date) %>% 
    mutate(dummy = 1) %>%
    group_by(team = awayteam) %>%
    summarise(goals_scored_away = sum(ftag),
              goals_conced_away = sum(fthg),
              games_played_away = n(),
              win_away = sum(dummy[fthg < ftag]),
              draw_away = sum(dummy[fthg == ftag]),
              loss_away = sum(dummy[fthg > ftag]),
              points_away = win_away * 3 + draw_away * 1) %>% group_by(team) -> away
  
  
  # merge together
  left_join(home,away) %>% mutate(games_played = games_played_home + games_played_away,
                                  games_won = win_home + win_away,
                                  games_draw = draw_home + draw_away,
                                  games_loss = loss_home + loss_away,
                                  score = paste0(goals_scored_home+goals_scored_away,":",goals_conced_home+goals_conced_away),
                                  goal_diff = (goals_scored_home+goals_scored_away) - (goals_conced_home+goals_conced_away),
                                  points = points_home + points_away) %>%
    group_by(team,games_played,games_won,games_loss,score,goal_diff,points) %>% nest(.key = "additional_data") %>% arrange(desc(points)) %>% 
    mutate(at_time = lubridate::ymd(at_time_date), standing = 1:n()) %>% select(standing,at_time,everything()) -> t

}  
  
a <- "1819"
b <- "sco_pl"
c <- seq(ymd('2018-08-03'),ymd('2019-03-29'), by = '1 week')


pmap_df(list(a,b,c),table_sim_current_standing) -> x  



x %>% na.omit() %>%
  ggplot(.,aes(x = at_time, y = standing)) + geom_line()+ facet_wrap(~team)  + 
  scale_y_reverse(breaks = seq(1,10,1)) + geom_smooth() +
  theme_minimal()



  

