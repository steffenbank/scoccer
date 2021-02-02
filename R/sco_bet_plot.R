
#' Analyse input of odds: how is the input odds compared to data from this season?

#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param venue hometeam or awayteam not in ""
#' @param venue_naming hometeam or awayteam as character (in "")
#' @param team team to look at (in "")
#' @param odds current odds to compare with
#'
#' @importFrom rlang .data
#'
#' @return a ggplot2 object
#' @export
#'
sco_bet_plot <- function(venue,venue_naming,team,odds) {

    # acquire data and filter
    dplyr::bind_rows(
      sco_bet_acquire(year_input,"sco_ch"),
      sco_bet_acquire(year_input,"sco_pl")) %>% dplyr::filter({{venue}} == team) -> dat_raw

    # create wide and long fomats
    if(venue_naming == 'hometeam') {dat_raw %>% dplyr::select(date,team = .data$awayteam,open = .data$b365h, close = .data$b365ch) -> dat_wide}
    if(venue_naming == 'awayteam') {dat_raw %>% dplyr::select(date,team = .data$hometeam,open = .data$b365a, close = .data$b365ca) -> dat_wide}

    tidyr::pivot_longer(dat_wide, cols = c(open,close),names_to = "type") -> dat_long

    # plot data
    ggplot2::ggplot() +
      ggplot2::geom_segment(data = dat_wide,
                   ggplot2::aes(x = open,
                       xend = close,
                       y    = stats::reorder(paste0(team," (",date,")"),lubridate::ymd(date)),
                       yend = paste0(team," (",date,")")),
                   size = 3, colour = '#D0D0D0') +
      ggplot2::geom_point(data = dat_long,
                 ggplot2::aes(x  = .data$value,
                     y      = paste0(team," (",date,")"),
                     colour = .data$type),
                 size = 4) +
      ggplot2::geom_vline(xintercept = odds, color = "#042B41", linetype = "dashed", size = 1) +
      ggplot2::labs(title = paste0("Odds analysis [", team," as ", venue_naming,"] @ Bet365"),
           subtitle = 'Red line = current odds',
           caption = 'source: football-data.co.uk/',
           x = NULL, y = NULL) +
      ggplot2::scale_colour_manual(values = c('#BD1118','darkgreen')) +
      ggplot2::scale_shape_manual("", values = c(4,19), guide = F) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = '#F8F8F2'),
                     plot.background = ggplot2::element_rect(fill = '#F8F8F2')) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "top")



}

