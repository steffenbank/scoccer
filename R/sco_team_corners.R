
#' acquire team corner statistics
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param team_input hometeam
#' @param card_line_input card line input
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @return a data frame
#' @export
#'
sco_team_corners <- function(year_input, league_input,team_input,card_line_input) {

  # ---------------------------------------------------------- #
  # Get data
  data <- sco_acquire(year_input, league_input) %>% dplyr::mutate(dummy = 1)

  # ---------------------------------------------------------- #
  # Get data of hometeam
  data %>%
    dplyr::filter(hometeam == team_input) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> hometeam_home_data

  data %>%
    dplyr::filter(awayteam == team_input) %>%
    dplyr::arrange(dplyr::desc(lubridate::ymd(date))) -> hometeam_away_data


  # ---------------------------------------------------------- #
  # merge data
  suppressMessages(dplyr::bind_rows(
    dplyr::left_join(
      hometeam_home_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0(team_input, " home")) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[hc > card_line_input]),"/5")),
      hometeam_home_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0(team_input, " home")) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[hc > card_line_input]),"/10"))) %>%
      dplyr::left_join(., hometeam_home_data %>% dplyr::group_by(Parameter = paste0(team_input, " home")) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[hc > card_line_input]),"/",sum(dummy)))),

    dplyr::left_join(
      hometeam_away_data %>% dplyr::slice(1:5) %>% dplyr::group_by(Parameter = paste0(team_input, " away")) %>% dplyr::summarise(`Previous 5 games` = paste0(sum(dummy[ac > card_line_input]),"/5")),
      hometeam_away_data %>% dplyr::slice(1:10) %>% dplyr::group_by(Parameter = paste0(team_input, " away")) %>% dplyr::summarise(`Previous 10 games` = paste0(sum(dummy[ac > card_line_input]),"/10"))) %>%
      dplyr::left_join(., hometeam_away_data %>% dplyr::group_by(Parameter = paste0(team_input, " away")) %>% dplyr::summarise(`All season` =  paste0(sum(dummy[ac > card_line_input]),"/",sum(dummy)))))) -> data_merged

  # ---------------------------------------------------------- #
  # return
  return(data_merged)

}



