
#' Title
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a stat sheet
#'
#' @importFrom rlang .data
#' @export
#'
sco_team_stats <- function(year_input,league_input) {

  # get data
  sco_acquire(year_input,league_input) -> data

  # bind and join data
  dplyr::bind_rows(
    data %>%
    dplyr::select(.data$hometeam, .data$hs, .data$hst,.data$hc,.data$hy,.data$hr) %>%
    dplyr::rename(team = .data$hometeam,
                  shots = .data$hs,
                  shots_on_target = .data$hst,
                  corners = .data$hc,
                  yellow_cards = .data$hy,
                  red_cards = data$hr),
  data %>%
    dplyr::select(data$awayteam, data$as, data$ast,data$ac,data$ay,data$ar) %>%
    dplyr::rename(team = .data$awayteam,
                  shots = .data$as,
                  shots_on_target = .data$ast,
                  corners = .data$ac,
                  yellow_cards = .data$ay,
                  red_cards = .data$ar)) %>%
    dplyr::group_by(.data$team) %>%
    dplyr::summarise_if(.data, is.numeric,mean,na.rm = TRUE) %>%
    dplyr::mutate(shots = round(.data$shots),
         shots_on_target = round(.data$shots_on_target),
         corners = round(.data$corners),
         yellow_cards = round(.data$yellow_cards,digits = 2),
         red_cards = round(.data$red_cards, digits = 2))

}
