
#' relative strength between two opponenets
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input  input of league
#' @param hometeam_input hometeam
#' @param awayteam_input awayteam
#' @param date_lim limit of date to include
#'
#' @return a dataframe of relative strength in terms of scoring and conceding
#' @importFrom rlang .data

#' @export
#'
sco_relative_strength <- function(year_input,league_input,hometeam_input,awayteam_input,date_lim) {

  # ---------------------------------------------------------- #
  # filter data
  dplyr::bind_rows(
    sco_acquire(year_input,league_input) %>% dplyr::filter(lubridate::ymd(.data$date) < lubridate::ymd(date_lim)) %>%
      dplyr::group_by(.data$hometeam) %>% dplyr::arrange(plyr::desc(.data$date)),
    sco_acquire(year_input,league_input) %>% dplyr::filter(lubridate::ymd(.data$date) < lubridate::ymd(date_lim)) %>%
      dplyr::group_by(.data$awayteam) %>% dplyr::arrange(plyr::desc(date))) %>%
    dplyr::distinct() -> filter_data



  # ---------------------------------------------------------- #
  # calculate averages
  avg_home_score <- (sum(filter_data$fthg, na.rm = TRUE))/nrow(filter_data) # average number of goals scored at home // average number of goals conceded away
  avg_away_score <- (sum(filter_data$ftag, na.rm = TRUE))/nrow(filter_data) # average number of goals scored away // average number of goals conceded home


  # ---------------------------------------------------------- #
  # relative attack strength @ home
  filter_data %>%
    dplyr::filter(.data$hometeam == hometeam_input) %>% dplyr::group_by(.data$hometeam) %>%
    dplyr::summarise(home_goals_scored = sum(.data$fthg), home_games_played = dplyr::n()) %>%
    dplyr::mutate(home_attack_strength = (.data$home_goals_scored/.data$home_games_played)/avg_home_score) ->
    home_attack_strength


  # ---------------------------------------------------------- #
  # relative defence strength @ home
  filter_data %>%
    dplyr::filter(.data$hometeam == hometeam_input) %>% dplyr::group_by(.data$hometeam) %>%
    dplyr::summarise(home_goals_conc = sum(.data$ftag), home_games_played = dplyr::n()) %>%
    dplyr::mutate(home_defence_strength = (.data$home_goals_conc/.data$home_games_played)/avg_away_score) ->
    home_defence_strength


  # ---------------------------------------------------------- #
  # relative attack strength @ away
  filter_data %>%
    dplyr::filter(.data$awayteam == awayteam_input) %>% dplyr::group_by(.data$awayteam) %>%
    dplyr::summarise(away_goals_scored = sum(.data$ftag), away_games_played = dplyr::n()) %>%
    dplyr::mutate(away_attack_strength = (.data$away_goals_scored/.data$away_games_played)/avg_away_score) ->
    away_attack_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ away
  filter_data %>%
    dplyr::filter(.data$awayteam == awayteam_input) %>% dplyr::group_by(.data$awayteam) %>%
    dplyr::summarise(away_goals_conc = sum(.data$fthg), away_games_played = dplyr::n()) %>%
    dplyr::mutate(away_defence_strength = (.data$away_goals_conc/.data$away_games_played)/avg_home_score) ->
    away_defence_strength

  # ---------------------------------------------------------- #
  # relative defence strength @ away
  suppressMessages(dplyr::bind_cols(
    dplyr::inner_join(
      home_attack_strength %>% dplyr::select(.data$hometeam,home_attack_strength) %>% dplyr::mutate(avg_home_score = avg_home_score),
      home_defence_strength %>% dplyr::select(.data$hometeam, home_defence_strength) %>% dplyr::mutate(avg_away_score = avg_away_score), by = NULL),
    dplyr::inner_join(
      away_attack_strength %>% dplyr::select(.data$awayteam,away_attack_strength),
      away_defence_strength %>% dplyr::select(.data$awayteam, away_defence_strength), by = NULL))) ->    strength_data

  return(strength_data)


}


# ---------------------------------------------------------------------------------------------------------------------------- #


#' possion model of expected goals for home and away team
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param hometeam_input hometeam
#' @param awayteam_input awayteam
#' @param date_lim limit of date to include
#'
#' @return a object with prediction
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_poisson_xg <- function(year_input,league_input,hometeam_input,awayteam_input,date_lim) {

  # ---------------------------------------------------------- #
  # calculate relative strength
  sco_relative_strength(year_input,league_input,hometeam_input,awayteam_input,date_lim) -> data_input

  suppressMessages(dplyr::left_join(
    dplyr::bind_cols(
      data_input %>% dplyr::select(.data$hometeam,.data$home_attack_strength,.data$avg_home_score),
      data_input %>% dplyr::select(.data$awayteam,.data$away_defence_strength)) %>%
      dplyr::mutate(xg_home = .data$home_attack_strength * .data$away_defence_strength * .data$avg_home_score),
    dplyr::bind_cols(
      data_input  %>% dplyr::select(.data$hometeam,.data$home_defence_strength,.data$avg_away_score),
      data_input %>% dplyr::select(.data$awayteam,.data$away_attack_strength)) %>%
      dplyr::mutate(xg_away = .data$home_defence_strength * .data$away_attack_strength * .data$avg_away_score),by = NULL) %>%
    dplyr::group_by(.data$hometeam,.data$awayteam,.data$xg_home,.data$xg_away) %>% tidyr::nest(x = -dplyr::group_cols())) ->
    rel_strength

  # ---------------------------------------------------------- #
  # calculate probability for 0-5 goals for home team and select highest
  dplyr::tibble(homegoals = seq(0,5,1), home_prob = stats::dpois(.data$homegoals,rel_strength$xg_home))  -> home_goal_data

  home_goal_pred <- as.numeric(home_goal_data[which.max(home_goal_data$home_prob),1])

  # ---------------------------------------------------------- #
  # calculate probability for 0-5 goals for away team and select highest
  dplyr::tibble(awaygoals = seq(0,5,1), away_prob = stats::dpois(.data$awaygoals,rel_strength$xg_away)) -> away_goal_data

  away_goal_pred <- as.numeric(away_goal_data[which.max(away_goal_data$away_prob),1])


  # bind prediction data together to human-readable
  dplyr::bind_cols(
    hometeam = data_input$hometeam,
    awayteam = data_input$awayteam) %>%
    dplyr::mutate(prediction = paste0(home_goal_pred,"-",away_goal_pred)) -> prediction_data


  # create plot
  dplyr::bind_rows(
    home_goal_data %>% dplyr::mutate(goals = .data$homegoals, prob = .data$home_prob, indi = paste0(data_input$hometeam, " (home)")),
    away_goal_data %>% dplyr::mutate(goals = .data$awaygoals, prob = .data$away_prob, indi = paste0(data_input$awayteam, " (away)"))) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$goals, y = .data$prob, color = .data$indi)) + ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = c('#BD1118','darkgreen')) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = '#F8F8F2'),
                   plot.background = ggplot2::element_rect(fill = '#F8F8F2')) +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "top") +
    ggplot2::labs(x = "Number of Goals", y = "Probability", title = "Probability of scoring x number of goals ", subtitle = "Poisson/relative streng") -> plot

  plot


  }
