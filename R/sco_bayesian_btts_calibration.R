
#' calibrate the bayesian model by changing the threshold values
#'
#' @param league_input co_pl (Premiership) or sco_ch (Championship)
#' @param year_input input as character in years of seasons, eg. "yyyy"
#'
#' @return a object of prediction strength for combination of upper nad lower treshold values
#' @export
#'
sco_bayesian_calibration <- function(league_input,year_input) {

  # ---------------------------------------------------------- #
  # inputs
  seq(0.3,0.5,0.05) -> lower
  seq(0.5,0.7,0.05) -> upper
  expand.grid(lower = lower,upper = upper) -> lower_upper_grid

  # ---------------------------------------------------------- #
  # map through alle elements
  purrr::pmap_df(list(league_input,year_input,lower_upper_grid$lower,lower_upper_grid$upper), sco_bayesian_tester) -> calibration

  # ---------------------------------------------------------- #
  # summary of calibration
  calibration %>%
  dplyr::group_by(tresh) %>%
  dplyr::mutate(win = dplyr::if_else(play == TRUE & play == case, TRUE,FALSE)) %>%
  dplyr::summarise(plays = sum(play,na.rm = TRUE),wins = sum(win,na.rm = TRUE), pct = wins/plays) -> calibration_summary

  return(calibration_summary)

}
