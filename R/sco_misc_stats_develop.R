
#' Misc stats (shots, cards, corners) development during year
#'
#' @param stat_input_home stat devleopment of hometeam
#' @param stat_input_away stat development of awayteam
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a ggplot2 object of development of \code{stat_input_home} and \code{stat_input_away} during \code{year_input}
#' @export
#'
sco_misc_stats_develop <- function(stat_input_home,stat_input_away,year_input, league_input) {

  # ---------------------------------------------------------- #
  # home team stats
  sco_acquire(year_input,league_input) %>%
    dplyr::select(date,team = hometeam,val = stat_input_home) %>%
    dplyr::mutate(ven = "home") %>%
    dplyr::group_by(team) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::filter(ven == 'home') %>%
    dplyr::group_by(team,ven) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(matchs = 1:dplyr::n()) %>% na.omit() -> home


  # ---------------------------------------------------------- #
  # away team stats
  sco_acquire(year_input,league_input) %>%
    dplyr::select(date,team = awayteam,val = stat_input_away) %>%
    dplyr::mutate(ven = "away") %>%
    dplyr::group_by(team) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::filter(ven == 'away') %>%
    dplyr::group_by(team,ven) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(matchs = 1:dplyr::n()) %>% na.omit() -> away

  # ---------------------------------------------------------- #
  #away team stats
  dplyr::bind_rows(home,away) %>%
  dplyr::filter(team != "") %>%
    ggplot2::ggplot(.,ggplot2::aes(x = matchs, y = val, color = ven)) + ggplot2::geom_point(shape = 21) + ggplot2::facet_wrap(~team) +
    ggplot2::scale_y_continuous(breaks = seq(1,max(home$val,na.rm = TRUE),2)) + ggplot2::geom_smooth(se = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.ticks.x = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = "", title = paste0(stat_input_home," + ", stat_input_away," per game @", league_input),
                  subtitle = paste0(stringr::str_sub(year_input,1,2),"/",stringr::str_sub(year_input,3,4)))

}
