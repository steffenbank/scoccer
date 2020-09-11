
#' Corner plot
#'
#' @param team_input team to look at
#' @param line_on line to compare with
#'
#' @return a ggplot2 object
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_corner_plot <- function(team_input, line_on) {


  # ---------------------------------------------------------- #
  # get raw data
  dplyr::bind_rows(
    sco_acquire("1920","sco_pl") %>% mutate(season = "1920"),
    sco_acquire("2021","sco_pl") %>% mutate(season = "2021")) %>%
    dplyr::filter(hometeam == team_input | awayteam == team_input) -> raw_data


  # ---------------------------------------------------------- #
  # manage data
  dplyr::bind_rows(
    raw_data %>%
      dplyr::filter(.data$hometeam == team_input) %>%
      dplyr::mutate(team = .data$hometeam, opp = .data$awayteam,c = .data$hc, place = "home") %>%
      dplyr::select(.data$team,.data$opp,.data$place,.data$season,.data$c),
    raw_data %>%
      dplyr::filter(.data$awayteam == team_input) %>%
      dplyr::mutate(team = .data$awayteam, opp = .data$hometeam,c = .data$ac, place = "away") %>%
      dplyr::select(.data$team,.data$opp,.data$place,.data$season,.data$c)) -> plot_data

  # ---------------------------------------------------------- #
  # percentages
  overall_pct <-  round(sum(plot_data$c >= line_input)/nrow(plot_data)*100)
  home_pct <-  round(sum(plot_data$c >= line_input & plot_data$place == 'home')/nrow(subset(plot_data, place == 'home'))*100)
  away_pct <-  round(sum(plot_data$c >= line_input & plot_data$place == 'away')/nrow(subset(plot_data, place == 'away'))*100)

  # ---------------------------------------------------------- #
  #  plot data
  ggplot2::ggplot(plot_data, aes(x = .data$opp, y = .data$c, fill = .data$place)) + geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
    ggplot2::facet_wrap(~.data$season) +
    ggplot2::coord_flip() +
    geom_hline(ggplot2::aes(yintercept = line_input)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "Mongolian Baiti"),
      title = ggplot2::element_text(size = 18),
      plot.subtitle = ggplot2::element_text(size = 16),
      plot.caption = ggplot2::element_text(size = 10),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none") +
    labs(x = "", y = "", title = paste0("Corner stats: ", team_input),
         caption = paste0("Overall: ", overall_pct,"%\n Home: ", home_pct,"%\n Away: ", away_pct,"%"))


}


