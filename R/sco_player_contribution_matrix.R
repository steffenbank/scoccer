
#' Goal Contribution Matrix
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a dataframe of player data
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_player_contribution_matrix <- function(year_input, league_input) {


  # ---------------------------------------------------------- #
  # create cotribution matrix
  sco_player_data(year_input, league_input) -> raw

  raw %>%
    dplyr::left_join(., raw %>% dplyr::group_by(Team) %>%
                       dplyr::summarise(Gls_total = sum(as.numeric(Gls),na.rm = TRUE), Ast_total = sum(as.numeric(Ast),na.rm = TRUE))) %>%
    dplyr::mutate(goal_contrib = as.numeric(Gls)/Gls_total,
         assist_contrib = as.numeric(Ast)/Ast_total) -> contribution


  # ---------------------------------------------------------- #
  # create titles
  if(year_input == '2021' & league_input == "sco_pl") {
    title_input <- "Scottish Premiership (20/21)"
  } else if(year_input == '1920' & league_input == "sco_pl") {
    title_input <- "Scottish Premiership (19/21)"
  } else if(year_input == '1920' & league_input == "sco_ch") {
    title_input <- "Scottish Championship (19/20)"

  }


  # ---------------------------------------------------------- #
  # create plot
  contribution %>%
    ggplot2::ggplot(ggplot2::aes(assist_contrib, goal_contrib)) +
    ggplot2::geom_point(data = contribution %>%
               dplyr::filter(goal_contrib < 0.25 | assist_contrib < 0.15),
             color = "grey20", size = 4, alpha = 0.2) +
    ggplot2::geom_point(data = contribution %>%
               dplyr::filter(goal_contrib > 0.25 | assist_contrib > 0.15),
             color = "red", size = 4) +
    ggplot2::geom_hline(yintercept = 0.25, color = "grey20", alpha = 0.4) +
    ggplot2::geom_vline(xintercept = 0.15, color = "grey20", alpha = 0.4) +
  ggrepel::geom_text_repel(data = contribution %>%
                    dplyr::filter(goal_contrib > 0.25 | assist_contrib > 0.15),
                                  ggplot2::aes(label = Player, family = "Roboto Condensed", fontface = "bold"),
                  seed = 15, size = 5,
                  min.segment.length = 0, segment.color = "red",
                  point.padding = 0.5) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                     limits = c(0, 0.3)) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     limits = c(0, 0.5)) +
    ggplot2::labs(title = paste0("Goal Contribution Matrix: ", title_input),
       subtitle = "Team goal involvement as percentage of total club goals* and/or assists.",
       caption = paste0("
                      *Own goals not counted in total/player can play at multiple clubs
                      Data: soccerway.com
                      By: @SteffenBank"),
       x = "Percentage of club goals assisted",
       y = "Percentage of club goals scored") +
  ggplot2::theme_minimal() +
  ggplot2::theme(text = ggplot2::element_text(family = "Roboto Condensed"),
        title = ggplot2::element_text(size = 18),
        plot.subtitle = ggplot2::element_text(size = 16),
        plot.caption = ggplot2::element_text(size = 10),
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        panel.grid.minor.x = ggplot2::element_blank()) -> goal_contribution_matrix

goal_contribution_matrix

}
