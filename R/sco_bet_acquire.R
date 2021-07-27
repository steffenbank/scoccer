
#' Acquire betting data from Bet365
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a data frame
#' @export
#'
sco_acquire_bet <- function(year_input,league_input) {

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
    dplyr::rename_all(tolower) %>%
     dplyr::mutate(date = lubridate::ymd(paste0(stringr::str_sub(.data$date,7,10),stringr::str_sub(.data$date,4,5),stringr::str_sub(.data$date,1,2)))) %>%
     dplyr::select(.data$date,.data$hometeam,.data$awayteam,.data$fthg,.data$ftag,dplyr::starts_with("b365")) -> data

}


