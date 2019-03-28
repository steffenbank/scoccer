
#' relative strength between two opponenets
#'
#' @param data_input a object from \code{score_aquire}
#' @param hometeam_input hometeam
#' @param awayteam_input awayteam
#' @param n_previous_games  number of games to base the relative strength upon
#'
#' @return a dataframe of relative strength in terms of scoring and conceding
#' @export

sco_relative_strength <- function(data_input,hometeam_input,awayteam_input,n_previous_games) {

  # ---------------------------------------------------------- #
  # filter data
  dplyr::bind_rows(
    data_input %>% dplyr::group_by(hometeam) %>% dplyr::arrange(desc(dateplayed)) %>% dplyr::slice(1:n_previous_games),
    data_input %>% dplyr::group_by(awayteam) %>% dplyr::arrange(desc(dateplayed)) %>% dplyr::slice(1:n_previous_games)) %>% dplyr::distinct() ->
    filter_data


  # ---------------------------------------------------------- #
  # calculate averages
  avg_home_score <- (sum(filter_data$fthg, na.rm = TRUE))/nrow(filter_data) # average number of goals scored at home // average number of goals conceded away
  avg_away_score <- (sum(filter_data$ftag, na.rm = TRUE))/nrow(filter_data) # average number of goals scored away // average number of goals conceded home


  # ---------------------------------------------------------- #
  # relative attack strength @ home
  filter_data %>%
    dplyr::filter(hometeam == hometeam_input) %>% dplyr::group_by(hometeam) %>%
    dplyr::summarise(home_goals_scored = sum(fthg), home_games_played = n()) %>%
    dplyr::mutate(home_attack_strength = (home_goals_scored/home_games_played)/avg_home_score) ->
    home_attack_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ home
  filter_data %>%
    dplyr::filter(hometeam == hometeam_input) %>% dplyr::group_by(hometeam) %>%
    dplyr::summarise(home_goals_conc = sum(ftag), home_games_played = n()) %>%
    dplyr::mutate(home_defence_strength = (home_goals_conc/home_games_played)/avg_away_score) ->
    home_defence_strength

  # ---------------------------------------------------------- #
  # relative attack strength @ away
  filter_data %>%
    filter(awayteam == awayteam_input) %>% group_by(awayteam) %>%
    summarise(away_goals_scored = sum(ftag), away_games_played = n()) %>%
    mutate(away_attack_strength = (away_goals_scored/away_games_played)/avg_away_score) ->
    away_attack_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ away
  filter_data %>%
    filter(awayteam == awayteam_input) %>% group_by(awayteam) %>%
    summarise(away_goals_conc = sum(fthg), away_games_played = n()) %>%
    mutate(away_defence_strength = (away_goals_conc/away_games_played)/avg_home_score) ->
    away_defence_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ away
  bind_cols(
    inner_join(
      home_attack_strength %>% select(hometeam,home_attack_strength) %>% mutate(avg_home_score = avg_home_score),
      home_defence_strength %>% select(hometeam, home_defence_strength) %>% mutate(avg_away_score = avg_away_score), by = NULL),
    inner_join(
      away_attack_strength %>% select(awayteam,away_attack_strength),
      away_defence_strength %>% select(awayteam, away_defence_strength), by = NULL)) ->
    strength_data


}
