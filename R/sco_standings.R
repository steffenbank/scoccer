
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
    dplyr::filter(.data$date < at_time_date) %>%
      dplyr::mutate(dummy = 1) %>%
      dplyr::group_by(team = .data$hometeam) %>%
      dplyr::summarise(goals_scored_home = sum(.data$fthg),
                goals_conced_home = sum(.data$ftag),
                games_played_home = dplyr::n(),
                win_home = sum(.data$dummy[.data$fthg > .data$ftag]),
                draw_home = sum(.data$dummy[.data$fthg == .data$ftag]),
                loss_home = sum(.data$dummy[.data$fthg < .data$ftag]),
                points_home = .data$win_home * 3 + .data$draw_home * 1) %>%
    dplyr::group_by(.data$team) -> home

  # ---------------------------------------------------------- #
  # scores at away
  sco_acquire(year_input,league_input) %>%
    dplyr::filter(.data$date < at_time_date) %>%
    dplyr::mutate(dummy = 1) %>%
    dplyr::group_by(team = .data$awayteam) %>%
    dplyr::summarise(goals_scored_away = sum(.data$ftag),
              goals_conced_away = sum(.data$fthg),
              games_played_away = dplyr::n(),
              win_away = sum(.data$dummy[.data$fthg < .data$ftag]),
              draw_away = sum(.data$dummy[.data$fthg == .data$ftag]),
              loss_away = sum(.data$dummy[.data$fthg > .data$ftag]),
              points_away = .data$win_away * 3 + .data$draw_away * 1) %>%
    dplyr::group_by(.data$team) -> away


  # ---------------------------------------------------------- #
  # merged
  dplyr::left_join(home,away) %>%
    dplyr::mutate(games_played = .data$games_played_home + .data$games_played_away,
                                  games_won = .data$win_home + .data$win_away,
                                  games_draw = .data$draw_home + .data$draw_away,
                                  games_loss = .data$loss_home + .data$loss_away,
                                  score = paste0(.data$goals_scored_home+.data$goals_scored_away,":",.data$goals_conced_home+.data$goals_conced_away),
                                  goal_diff = (.data$goals_scored_home+.data$goals_scored_away) - (.data$goals_conced_home+.data$goals_conced_away),
                                  points = .data$points_home + .data$points_away) %>%
    dplyr::group_by(.data$team,.data$games_played,.data$games_won,.data$games_loss,.data$score,.data$goal_diff,.data$points) %>%
    tidyr::nest(.key = "additional_data") %>%
    dplyr::arrange(plyr::desc(.data$points)) %>%
    dplyr::mutate(at_time = lubridate::ymd(at_time_date), standing = 1:dplyr::n()) %>%
    dplyr::select(.data$standing,.data$at_time,dplyr::everything()) -> merged

  return(merged)

}






