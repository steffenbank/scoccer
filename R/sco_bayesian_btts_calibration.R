
#' calibrate the bayesian model with respect to where to put best guess thresh
#'
#' @param league_input  sco_pl (Premiership) or sco_ch (Championship)
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param play_upper_treshhold thresh of best guess to accept play of case
#' @param play_lower_treshhold thresh of best guess to accept play of non-case

#'
#' @return an object
#' @export
#'
sco_bayesian_calibration <- function(league_input,year_input,play_upper_treshhold, play_lower_treshhold) {

  # ---------------------------------------------------------- #
  # test data
  sco_acquire(year_input, league_input) %>%
  dplyr::filter(date >= Sys.Date()-30) %>%
  dplyr::mutate(case = dplyr::if_else(fthg > 0 & ftag > 0,TRUE,FALSE)) %>%
  dplyr::select(date,hometeam,awayteam,case) -> testdata

  # ---------------------------------------------------------- #
  # combinations to predict
  hometeams <- as.character(testdata$hometeam)
  awayteams <- as.character(testdata$awayteam)

  # ---------------------------------------------------------- #
  # first match (home and away + binding) [skal laves til en funktion, men hvad skal den hedde]
  sco_bayesian_btts("1819","sco_ch","hometeam",hometeams[1],FALSE,TRUE) %>% dplyr::rename(median_prop_home = median_prop, hometeam = team) %>% dplyr::select(-as,-sd_prop) -> home
  sco_bayesian_btts("1819","sco_ch","awayteam",awayteams[1],FALSE,TRUE) %>% dplyr::rename(median_prop_away = median_prop, awayteam = team) %>% dplyr::select(-as,-sd_prop) -> away

  dplyr::bind_cols(home,away) %>%
    dplyr::mutate(play = dplyr::if_else(median_prop_away > play_upper_treshhold & median_prop_home > play_upper_treshhold,TRUE,
                                                                      dplyr::if_else(median_prop_away < play_lower_treshhold & median_prop_home < play_lower_treshhold,FALSE,NA))) -> played_base

  # ---------------------------------------------------------- #
  # loop through rest of matches
  for(i in 2:nrow(testdata)) {

    sco_bayesian_btts("1819","sco_ch","hometeam",hometeams[i],FALSE,TRUE) %>% dplyr::rename(median_prop_home = median_prop, hometeam = team) %>% dplyr::select(-as,-sd_prop) -> home
    sco_bayesian_btts("1819","sco_ch","awayteam",awayteams[i],FALSE,TRUE) %>% dplyr::rename(median_prop_away = median_prop, awayteam = team) %>% dplyr::select(-as,-sd_prop) -> away

    dplyr::bind_cols(home,away) %>%
      dplyr::mutate(play = dplyr::if_else(median_prop_away > play_upper_treshhold & median_prop_home > play_upper_treshhold ,TRUE,
                                                                        dplyr::if_else(median_prop_away < play_lower_treshhold & median_prop_home < play_lower_treshhold, FALSE,NA))) -> played

    dplyr::bind_rows(played_base,played) -> played_base

  }

  # ---------------------------------------------------------- #
  # return data wih added threshold
  return(dplyr::left_join(played_base, testdata,by = c('hometeam','awayteam')) %>% dplyr::mutate(tresh = paste0(play_lower_treshhold," - ",play_upper_treshhold)))


}
