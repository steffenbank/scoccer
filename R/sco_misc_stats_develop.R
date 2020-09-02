
#' Misc stats (goals,shots, cards, corners) development during year
#'
#' @param stat_input_home stat devleopment (shots, shontsontarget,corners,yellowcard,redcards)
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a ggplot2 object of development of \code{stat_input_home} and \code{stat_input_away} during \code{year_input}
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_misc_stats_develop <- function(stat_input,year_input, league_input) {


  # ---------------------------------------------------------- #
  # define inputs
  if(stat_input == "shots") {
    stat_input_home <- "hs"
    stat_input_away <- "as"
  } else if(stat_input == "shotsontarget") {
    stat_input_home <- "hst"
    stat_input_away <- "ast"
  } else if(stat_input == "corners") {
    stat_input_home <- "hc"
    stat_input_away <- "ac"
  } else if(stat_input == "yellowcards") {
    stat_input_home <- "hy"
    stat_input_away <- "ay"
  }  else if(stat_input == "redcards") {
    stat_input_home <- "hr"
    stat_input_away <- "ar"
  } else {
    print("Unviable input")
  }

  # ---------------------------------------------------------- #
  # home team stats
  sco_acquire(year_input,league_input) %>%
    dplyr::select(.data$date,team = .data$hometeam,val = stat_input_home) %>%
    dplyr::mutate(ven = "home") %>%
    dplyr::group_by(.data$team) %>%
    dplyr::arrange(plyr::desc(.data$date)) %>%
    dplyr::filter(.data$ven == 'home') %>%
    dplyr::group_by(.data$team,.data$ven) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(matchs = 1:dplyr::n()) %>% stats::na.omit() -> home


  # ---------------------------------------------------------- #
  # away team stats
  sco_acquire(year_input,league_input) %>%
    dplyr::select(.data$date,team = .data$awayteam,val = stat_input_away) %>%
    dplyr::mutate(ven = "away") %>%
    dplyr::group_by(.data$team) %>%
    dplyr::arrange(plyr::desc(.data$date)) %>%
    dplyr::filter(.data$ven == 'away') %>%
    dplyr::group_by(.data$team,.data$ven) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(matchs = 1:dplyr::n()) %>% stats::na.omit() -> away

  # ---------------------------------------------------------- #
  #away team stats
  dplyr::bind_rows(home,away) %>%
  dplyr::filter(.data$team != "") %>%
    ggplot2::ggplot(.data,ggplot2::aes(x = .data$matchs, y = .data$val, color = .data$ven)) + ggplot2::geom_point(shape = 21) + ggplot2::facet_wrap(~team) +
    ggplot2::scale_y_continuous(breaks = seq(1,max(home$val,na.rm = TRUE),2)) + ggplot2::geom_smooth(se = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = "", title = paste0(stat_input," per game @ ", league_input),
                  subtitle = paste0(stringr::str_sub(year_input,1,2),"/",stringr::str_sub(year_input,3,4)))

}
