
#' Analyse input of odds: how is the input odds compared to data from this season?

#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param team_input selected_team
#'
#' @importFrom rlang .data
#' @importFrom stats reorder
#'
#' @return a ggplot2 object
#' @export
#'
sco_bet_plot <- function(year_input,league_input, team_input) {


    sco_acquire(year_input, league_input) %>% dplyr::select(.data$date,.data$hometeam,.data$awayteam,
                        hometeam_open = .data$b365h,
                        hometeam_closes = .data$b365ch,
                        awayteam_open = .data$b365a,
                        awayteam_closes = .data$b365ca,
                        over_open = .data$b365.2.5,
                        over_closes = .data$b365c.2.5) %>%
      tidyr::pivot_longer(.data, cols = c(-.data$date,-.data$hometeam,-.data$awayteam), names_to = "type") %>%
        dplyr::filter(.data$hometeam == team_input | .data$awayteam == team_input) %>%
      dplyr::filter(.data$hometeam == team_input & grepl("hometeam",.data$type)
                    | .data$awayteam == team_input & grepl("awayteam",.data$type) |
                      grepl("over",.data$type)) %>%
      dplyr::mutate(type2 = dplyr::if_else(grepl("over",.data$type),"over 2.5 goals","1X2")) %>%
      dplyr::mutate(type = dplyr::if_else(grepl("open",.data$type),"open","closes")) %>%
      tidyr::pivot_wider(.data, id_cols = c(.data$date,.data$hometeam,.data$awayteam,.data$type2), names_from = .data$type, values_from = .data$value) -> data_man

    # may be used to print later
    dplyr::left_join(
      data_man %>%
        dplyr::mutate(dummy = 1) %>%
        dplyr::group_by(odds = .data$type2) %>%
        dplyr::summarise(pct_matches_decreasing_odds = sum(.data$dummy[.data$open>.data$closes])/sum(.data$dummy),
                         pct_matches_increasing_odds = sum(.data$dummy[.data$open<.data$closes])/sum(.data$dummy),
                         pct_matches_same_odds = sum(.data$dummy[.data$open==.data$closes])/sum(.data$dummy)),
      data_man %>%
        dplyr::filter(.data$open>.data$closes) %>%
        dplyr::group_by(odds = .data$type2) %>%
        dplyr::summarise(decreasing_odds_change = mean(.data$open)-mean(.data$closes))) -> temp

    # plot data
    ggplot2::ggplot() +
      ggplot2::geom_segment(data = data_man,
                            ggplot2::aes(x = .data$open,
                                         xend = .data$closes,
                                         y    = stats::reorder(paste0(.data$hometeam,"-",.data$awayteam," (",.data$date,")"), lubridate::ymd(.data$date)),
                                         yend = stats::reorder(paste0(.data$hometeam,"-",.data$awayteam," (",.data$date,")"), lubridate::ymd(.data$date))),
                            size = 3, colour = '#D0D0D0') +
      ggplot2::facet_wrap(~.data$type2, scales = "free_x") +
      ggplot2::geom_point(data = data_man,
                          ggplot2::aes(x  = .data$open,
                                       color = "open",
                                       y = reorder(paste0(.data$hometeam,"-",.data$awayteam," (",.data$date,")"), lubridate::ymd(.data$date))),
                          size = 2) +

      ggplot2::geom_point(data = data_man,
                          ggplot2::aes(x  = .data$closes,
                                       color = "closes",
                                       y      = stats::reorder(paste0(.data$hometeam,"-",.data$awayteam," (",.data$date,")"), lubridate::ymd(.data$date))),
                                       size = 2) +
      ggplot2::scale_colour_manual(values = c('#BD1118','darkgreen')) +
      ggplot2::scale_shape_manual("", values = c(4,19), guide = F) +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = '#F8F8F2'),
                     plot.background = ggplot2::element_rect(fill = '#F8F8F2')) +
      ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "top") +
    ggplot2::labs(x = "", y = "", title = "Odds movement", subtitle = team_input)



}

