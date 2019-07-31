
#' relative strength between two opponenets
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param hometeam_input hometeam
#' @param awayteam_input awayteam
#' @param n_previous_games  number of games to base the relative strength upon
#'
#' @return a dataframe of relative strength in terms of scoring and conceding
#' @export

sco_relative_strength <- function(year_input,league_input,hometeam_input,awayteam_input,n_previous_games) {

  # ---------------------------------------------------------- #
  # filter data
  dplyr::bind_rows(
    sco_acquire(year_input,league_input) %>% dplyr::group_by(hometeam) %>% dplyr::arrange(desc(date)) %>% dplyr::slice(1:n_previous_games),
    sco_acquire(year_input,league_input) %>% dplyr::group_by(awayteam) %>% dplyr::arrange(desc(date)) %>% dplyr::slice(1:n_previous_games)) %>% dplyr::distinct() ->
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
    dplyr::filter(awayteam == awayteam_input) %>% dplyr::group_by(awayteam) %>%
    dplyr::summarise(away_goals_scored = sum(ftag), away_games_played = n()) %>%
    dplyr::mutate(away_attack_strength = (away_goals_scored/away_games_played)/avg_away_score) ->
    away_attack_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ away
  filter_data %>%
    dplyr::filter(awayteam == awayteam_input) %>% dplyr::group_by(awayteam) %>%
    dplyr::summarise(away_goals_conc = sum(fthg), away_games_played = n()) %>%
    dplyr::mutate(away_defence_strength = (away_goals_conc/away_games_played)/avg_home_score) ->
    away_defence_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ away
  dplyr::bind_cols(
    dplyr::inner_join(
      home_attack_strength %>% dplyr::select(hometeam,home_attack_strength) %>% dplyr::mutate(avg_home_score = avg_home_score),
      home_defence_strength %>% dplyr::select(hometeam, home_defence_strength) %>% dplyr::mutate(avg_away_score = avg_away_score), by = NULL),
    dplyr::inner_join(
      away_attack_strength %>% dplyr::select(awayteam,away_attack_strength),
      away_defence_strength %>% dplyr::select(awayteam, away_defence_strength), by = NULL)) %>%
    dplyr::mutate(n_previous_games = n_previous_games) ->
    strength_data

  return(strength_data)


}
