
#' test actual data against model data for difference previous games
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param n_previous_game_low number of games to base the relative strength upon (lower boundary)
#' @param n_previous_game_high number of games to base the relative strength upon (upper boundary)
#' #'
#' @return a plot that shows the highest rate of trues (result hits) in terms of looking games back
#' @export
#'
sco_poisson_xg_calibration <- function(year_input,league_input,n_previous_game_low, n_previous_game_high) {

  # ---------------------------------------------------------- #
  # comparison data
  a <- year_input
  b <- league_input
  c <- seq(n_previous_game_low,n_previous_game_high,1)

  # map model_tester oveer input
  model_tester_run <- purrr::pmap_df(list(a,b,c),sco_poisson_xg_tester)

  # ---------------------------------------------------------- #
  #  TRUE/FALSE over previous games plot data
  model_tester_run %>%
    dplyr::group_by(n_previous_games) %>%
    dplyr::summarise(total = n()) ->
    model_tester_run_total

  model_tester_run %>%
    dplyr::group_by(n_previous_games, score_hit) %>%
    dplyr::tally() %>%
    dplyr::left_join(.,model_tester_run_total, by = NULL) %>%
    dplyr::mutate(pct = n/total) ->
    model_tester_plot_data

  # ---------------------------------------------------------- #
  #  plot dataa
  ggplot2::ggplot(model_tester_plot_data,ggplot2::aes(x = as.factor(n_previous_games), fill = score_hit, y = pct)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "", y = "", title = paste0("% score hit over time @ sco"), subtitle = "Last n games (1 = last game only)") +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_fill_brewer("",palette = "Set1")

}

