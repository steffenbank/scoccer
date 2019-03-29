
#' test actual data against model data
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)

#' @param n_previous_games number of games to base the relative strength upon
#'
#' @return an object that compares played games versus predictions for one value of \code{n_previous_games}
#' @export
#'
sco_poisson_xg_tester <- function(year_input,league_input,n_previous_games) {

  # ---------------------------------------------------------- #
  # comparison data
  sco_acquire(year_input, league_input) %>%
    dplyr::filter(date >= lubridate::floor_date(Sys.Date(),"month") - months(1)) %>%
    dplyr::mutate(result = paste0(fthg,"-",ftag)) %>%
    dplyr::select(date,hometeam,awayteam,result) ->
    comparison_data

  # ---------------------------------------------------------- #
  # Create relative strength data to model from
  a <- year_input
  b <- league_input
  c <- rep(as.character(comparison_data$hometeam))
  d <- rep(as.character(comparison_data$awayteam))
  e <- n_previous_games

  purrr::pmap_df(list(a,b,c,d,e),sco_relative_strength) -> rel

  # ---------------------------------------------------------- #
  # predict outcome of each match
  pred_base <- sco_poisson_xg(rel[1,])

  for(i in 2:nrow(rel)) {

    dplyr::bind_rows(pred_base, sco_poisson_xg(rel[i,])) -> pred_base

  }

  # ---------------------------------------------------------- #
  # compare prediction with acctual result of match
  dplyr::left_join(comparison_data,pred_base, by = NULL) %>%
    dplyr::select(date,hometeam,awayteam,n_previous_games,prediction,result) %>%
    dplyr::mutate(score_hit = dplyr::if_else(as.numeric(stringr::str_sub(prediction,1,1)) == as.numeric(stringr::str_sub(result,1,1)) &
                                 as.numeric(stringr::str_sub(prediction,3,3)) == as.numeric(stringr::str_sub(result,3,3)), TRUE, FALSE)) %>%
    dplyr::distinct() ->
    model_comparison

}
