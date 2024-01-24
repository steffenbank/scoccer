
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
  Sys.setenv(R_LIBCURL_SSL_REVOKE_BEST_EFFORT=TRUE)

  # ---------------------------------------------------------- #
  # assign league
  dplyr::tibble(league = c("sco_pl","sco_ch","sco_l1","sco_l2"), trans = c(0,1,2,3)) %>%
    dplyr::filter(league == league_input) %>%
    dplyr::pull(.data$trans) -> league


  # ---------------------------------------------------------- #
  # aquire data an d
  utils::read.csv(url(paste0("https://www.football-data.co.uk/mmz4281/",year_input,"/SC",league,".csv"))) %>%
    dplyr::mutate(Date = lubridate::ymd(paste0(stringr::str_sub(.data$Date,7,10),stringr::str_sub(.data$Date,4,5),stringr::str_sub(.data$Date,1,2)))) %>%
    dplyr::rename_all(tolower)

}
