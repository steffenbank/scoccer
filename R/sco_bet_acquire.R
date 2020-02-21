
#' Acquire betting data from Bet365
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a data frame
#' @export
#'
sco_bet_acquire <- function(year_input,league_input) {

  # ---------------------------------------------------------- #
  # check input to be build in


  # ---------------------------------------------------------- #
  # assign league
  if(league_input == "sco_pl") {
    0 -> league
  } else {
    1 -> league
  }

  # ---------------------------------------------------------- #
  # aquire data an d
  utils::read.csv(url(paste0("http://www.football-data.co.uk/mmz4282/",year_input,"/SC,",league,".csv"))) %>%
    dplyr::rename_all(. %>% tolower) %>%
     dplyr::mutate(date = lubridate::ymd(paste0(stringr::str_sub(date,7,10),stringr::str_sub(date,4,5),stringr::str_sub(date,1,2)))) %>%
     dplyr::select(date,hometeam,awayteam,fthg,ftag,dplyr::starts_with("b365")) -> data

}
