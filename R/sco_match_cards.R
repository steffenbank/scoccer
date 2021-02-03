
#' acquire match card statistics
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param hometeam_input hometeam
#' @param awayteam_imput awayteam
#' @param referee_input referee name (D Robertson)
#' @param card_line_input card line input
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return a data frame
#' @export
#'
sco_match_cards <- function(year_input, league_input,hometeam_input,awayteam_input,referee_input,card_line_input) {

  # ---------------------------------------------------------- #
  # Get data
  data <- sco_acquire(year_input, league_input) %>% dplyr::mutate(dummy = 1)

  data$hr <- if(any(names(data) == 'hr')) {data$hr} else {0}
  data$ar <- if(any(names(data) == 'ar')) {data$ar} else {0}

  # ---------------------------------------------------------- #
  # Get data of hometeam
  data %>%
    dplyr::filter(hometeam == hometeam_input) %>%
    dplyr::mutate(cards = hy+ay+hr+ar) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> hometeam_home_data

  data %>%
    dplyr::filter(awayteam == hometeam_input) %>%
    dplyr::mutate(cards = hy+ay+hr+ar) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> hometeam_away_data


  # ---------------------------------------------------------- #
  # Get data of awayteam
  data %>%
    dplyr::filter(awayteam == awayteam_input) %>%
    dplyr::mutate(cards = hy+ay+hr+ar) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> awayteam_away_data

  data %>%
    dplyr::filter(hometeam == awayteam_input) %>%
    dplyr::mutate(cards = hy+ay+hr+ar) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> awayteam_home_data

  # ---------------------------------------------------------- #
  # Get data of referee
  data %>%
    dplyr::filter(referee == referee_input) %>%
    dplyr::mutate(cards = hy+ay+hr+ar) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> referee_data


  # ---------------------------------------------------------- #
  # merge data
  suppressMessages(dplyr::bind_rows(
    dplyr::left_join(
      hometeam_home_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0(hometeam_input, " home")) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[cards > card_line_input]),"/5")),
      hometeam_home_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0(hometeam_input, " home")) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[cards > card_line_input]),"/10"))) %>%
     dplyr::left_join(., hometeam_home_data %>% dplyr::group_by(Parameter = paste0(hometeam_input, " home")) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[cards > card_line_input]),"/",sum(dummy)))),

    dplyr::left_join(
      hometeam_away_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0(hometeam_input, " away")) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[cards > card_line_input]),"/5")),
      hometeam_away_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0(hometeam_input, " away")) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[cards > card_line_input]),"/10"))) %>%
      dplyr::left_join(., hometeam_away_data %>% dplyr::group_by(Parameter = paste0(hometeam_input, " away")) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[cards > card_line_input]),"/",sum(dummy)))),

    dplyr::left_join(
      awayteam_away_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0(awayteam_input, " away")) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[cards > card_line_input]),"/5")),
      awayteam_away_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0(awayteam_input, " away")) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[cards > card_line_input]),"/10"))) %>%
      dplyr::left_join(., awayteam_away_data %>% dplyr::group_by(Parameter = paste0(awayteam_input, " away")) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[cards > card_line_input]),"/",sum(dummy)))),

    dplyr::left_join(
      awayteam_home_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0(awayteam_input, " home")) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[cards > card_line_input]),"/5")),
      awayteam_home_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0(awayteam_input, " home")) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[cards > card_line_input]),"/10"))) %>%
      dplyr::left_join(., awayteam_home_data %>% dplyr::group_by(Parameter = paste0(awayteam_input, " home")) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[cards > card_line_input]),"/",sum(dummy)))),

    dplyr::left_join(
      referee_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0("Referee ", referee_input)) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[cards > card_line_input]),"/5")),
      referee_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0("Referee ", referee_input)) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[cards > card_line_input]),"/10"))) %>%
      dplyr::left_join(., referee_data %>% dplyr::group_by(Parameter = paste0("Referee ", referee_input)) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[cards > card_line_input]),"/",sum(dummy))))
  )) -> data_merged

  # ---------------------------------------------------------- #
  # return
  return(data_merged)

}


