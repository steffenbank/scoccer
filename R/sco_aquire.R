
#' acquire data and rearrange columns
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data

#'
#' @return a data frame
#' @export
#'
sco_acquire <- function(year_input, league_input) {

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
    dplyr::select(1:23) %>%
    dplyr::mutate(Date = lubridate::ymd(paste0(stringr::str_sub(.data$Date,7,10),stringr::str_sub(.data$Date,4,5),stringr::str_sub(.data$Date,1,2)))) %>%
    dplyr::rename_all(. %>% tolower)

}
