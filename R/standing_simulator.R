
#' standings t at the given time
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param at_time_date time at standing "yyyy-mm-dd" format
#'
#' @return table standing at the \code{at_time_date}
#' @export
#
sco_standings <- function(year_input, league_input, at_time_date) {

  # ---------------------------------------------------------- #
  # scores at home
  sco_acquire(year_input,league_input) %>%
    dplyr::filter(date < at_time_date) %>%
      dplyr::mutate(dummy = 1) %>%
      dplyr::group_by(team = hometeam) %>%
      dplyr::summarise(goals_scored_home = sum(fthg),
                goals_conced_home = sum(ftag),
                games_played_home = dplyr::n(),
                win_home = sum(dummy[fthg > ftag]),
                draw_home = sum(dummy[fthg == ftag]),
                loss_home = sum(dummy[fthg < ftag]),
                points_home = win_home * 3 + draw_home * 1) %>%
    dplyr::group_by(team) -> home

  # ---------------------------------------------------------- #
  # scores at away
  sco_acquire(year_input,league_input) %>%
    dplyr::filter(date < at_time_date) %>%
    dplyr::mutate(dummy = 1) %>%
    dplyr::group_by(team = awayteam) %>%
    dplyr::summarise(goals_scored_away = sum(ftag),
              goals_conced_away = sum(fthg),
              games_played_away = dplyr::n(),
              win_away = sum(dummy[fthg < ftag]),
              draw_away = sum(dummy[fthg == ftag]),
              loss_away = sum(dummy[fthg > ftag]),
              points_away = win_away * 3 + draw_away * 1) %>%
    dplyr::group_by(team) -> away


  # ---------------------------------------------------------- #
  # merged
  dplyr::left_join(home,away) %>%
    dplyr::mutate(games_played = games_played_home + games_played_away,
                                  games_won = win_home + win_away,
                                  games_draw = draw_home + draw_away,
                                  games_loss = loss_home + loss_away,
                                  score = paste0(goals_scored_home+goals_scored_away,":",goals_conced_home+goals_conced_away),
                                  goal_diff = (goals_scored_home+goals_scored_away) - (goals_conced_home+goals_conced_away),
                                  points = points_home + points_away) %>%
    dplyr::group_by(team,games_played,games_won,games_loss,score,goal_diff,points) %>%
    tidyr::nest(.key = "additional_data") %>%
    dplyr::arrange(desc(points)) %>%
    dplyr::mutate(at_time = lubridate::ymd(at_time_date), standing = 1:dplyr::n()) %>%
    dplyr::select(standing,at_time,dplyr::everything()) -> merged

  return(merged)

}






