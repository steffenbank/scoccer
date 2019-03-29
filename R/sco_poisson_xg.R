
#' possion model of expected goals for home and away team
#'
#' @param relative_strength_input input object from \code{sco_relative_strength}
#'
#' @return a object with prediction
#' @export
#'
sco_poisson_xg <- function(relative_strength_input) {

  # ---------------------------------------------------------- #
  # calculate relative strength
  dplyr::left_join(
    dplyr::bind_cols(
      relative_strength_input %>% dplyr::select(hometeam,home_attack_strength,avg_home_score),
      relative_strength_input %>% dplyr::select(awayteam,away_defence_strength)) %>%
      dplyr::mutate(xg_home = home_attack_strength * away_defence_strength * avg_home_score),
    dplyr::bind_cols(
      relative_strength_input  %>% dplyr::select(hometeam,home_defence_strength,avg_away_score),
      relative_strength_input %>% dplyr::select(awayteam,away_attack_strength)) %>%
      dplyr::mutate(xg_away = home_defence_strength * away_attack_strength * avg_away_score),by = NULL) %>%
    dplyr::group_by(hometeam,awayteam,xg_home,xg_away) %>% tidyr::nest(.,.key = "xg_inputs") ->
    rel_strength

  # ---------------------------------------------------------- #
  # calculate probability for 0-5 goals for home team and select highest
  dplyr::tibble(homegoals = seq(0,5,1), home_prob = stats::dpois(homegoals,rel_strength$xg_home)) %>%
    tidyr::spread(homegoals,home_prob,sep = "") %>%
    tidyr::nest(.,.key = "home_prob") ->
    home_goal_data

  home_goal_pred <- as.numeric(stringr::str_sub(names(home_goal_data %>% tidyr::unnest())[apply(home_goal_data %>% tidyr::unnest(),1,which.max)],10,10))

  # ---------------------------------------------------------- #
  # calculate probability for 0-5 goals for away team and select highest
  dplyr::tibble(awaygoals = seq(0,5,1), away_prob = stats::dpois(awaygoals,rel_strength$xg_away)) %>%
    tidyr::spread(awaygoals,away_prob,sep = "") %>%
    tidyr::nest(.,.key = "away_prob") ->
    away_goal_data

  away_goal_pred <- as.numeric(stringr::str_sub(names(away_goal_data %>% tidyr::unnest())[apply(away_goal_data %>% tidyr::unnest(),1,which.max)],10,10))

  # bind prediction data together to human-readable
  dplyr::bind_cols(
    hometeam = relative_strength_input$hometeam, awayteam = relative_strength_input$awayteam, home_prob = home_goal_data, away_prob = away_goal_data, n_previous_games = relative_strength_input$n_previous_games) %>%
    dplyr::mutate(prediction = paste0(home_goal_pred,"-",away_goal_pred)) ->
    prediction_data
}
