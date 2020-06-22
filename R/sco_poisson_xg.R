
#' possion model of expected goals for home and away team
#'
#' @param relative_strength_input input object from \code{sco_relative_strength}
#'
#' @return a object with prediction
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_poisson_xg <- function(relative_strength_input) {

  # ---------------------------------------------------------- #
  # calculate relative strength
  dplyr::left_join(
    dplyr::bind_cols(
      relative_strength_input %>% dplyr::select(.data$hometeam,.data$home_attack_strength,.data$avg_home_score),
      relative_strength_input %>% dplyr::select(.data$awayteam,.data$away_defence_strength)) %>%
      dplyr::mutate(xg_home = .data$home_attack_strength * .data$away_defence_strength * .data$avg_home_score),
    dplyr::bind_cols(
      relative_strength_input  %>% dplyr::select(.data$hometeam,.data$home_defence_strength,.data$avg_away_score),
      relative_strength_input %>% dplyr::select(.data$awayteam,.data$away_attack_strength)) %>%
      dplyr::mutate(xg_away = .data$home_defence_strength * .data$away_attack_strength * .data$avg_away_score),by = NULL) %>%
    dplyr::group_by(.data$hometeam,.data$awayteam,.data$xg_home,.data$xg_away) %>% tidyr::nest(.,.key = "xg_inputs") ->
    rel_strength

  # ---------------------------------------------------------- #
  # calculate probability for 0-5 goals for home team and select highest
  dplyr::tibble(homegoals = seq(0,5,1), home_prob = stats::dpois(.data$homegoals,rel_strength$xg_home)) %>%
    tidyr::spread(.data$homegoals,.data$home_prob,sep = "") %>%
    tidyr::nest(.,.key = "home_prob") ->
    home_goal_data

  home_goal_pred <- as.numeric(stringr::str_sub(names(home_goal_data %>% tidyr::unnest())[apply(home_goal_data %>% tidyr::unnest(),1,which.max)],10,10))

  # ---------------------------------------------------------- #
  # calculate probability for 0-5 goals for away team and select highest
  dplyr::tibble(awaygoals = seq(0,5,1), away_prob = stats::dpois(.data$awaygoals,rel_strength$xg_away)) %>%
    tidyr::spread(.data$awaygoals,.data$away_prob,sep = "") %>%
    tidyr::nest(.,.key = "away_prob") ->
    away_goal_data

  away_goal_pred <- as.numeric(stringr::str_sub(names(away_goal_data %>% tidyr::unnest())[apply(away_goal_data %>% tidyr::unnest(),1,which.max)],10,10))

  # bind prediction data together to human-readable
  dplyr::bind_cols(
    hometeam = relative_strength_input$hometeam, awayteam = relative_strength_input$awayteam, home_prob = .data$home_goal_data, away_prob = .data$away_goal_data, n_previous_games = relative_strength_input$n_previous_games) %>%
    dplyr::mutate(prediction = paste0(.data$home_goal_pred,"-",.dta$away_goal_pred)) ->
    prediction_data
}
