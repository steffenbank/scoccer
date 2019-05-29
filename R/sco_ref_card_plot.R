
#' Plot cards given by each referee in a season
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a ggplot2 object of cards by refs in \code{year_input} of \code{league_input}
#' @export
#'
sco_ref_card_plot <- function(year_input, league_input) {

  # ---------------------------------------------------------- #
  # compute cards for each referee and devide into subcategories
  sco_acquire(year_input,league_input) %>%
    dplyr::mutate(cards = hy + ay + hr + ar) %>%
    dplyr::mutate(card_group = dplyr::if_else(cards %in% c(0,1,2), "0 - 2",
                                dplyr::if_else(cards %in% c(3,4), "3 - 4",
                                        dplyr::if_else(cards %in% c(5,6), "5 - 6","6 - ")))) %>%
    dplyr::group_by(referee,card_group = cards) %>%
    dplyr::tally() -> each_ref

  # ---------------------------------------------------------- #
  # compute total cards for each referee
  each_ref %>%
    dplyr::group_by(referee) %>%
    dplyr::summarise(n_total = sum(n)) -> total_ref

  # ---------------------------------------------------------- #
  # compute total cards for each referee
  dplyr::left_join(each_ref,total_ref, by = NULL) %>%
    dplyr::mutate(pct = n/n_total) %>%
    ggplot2::ggplot(.,ggplot2::aes(x = paste0(referee," (",n_total,")"), fill = as.factor(card_group), y = pct)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = rev(viridis::viridis_pal(option = "B")(12))) +
    ggplot2::labs(x = "", y = "", title = paste0("Times cards given by ref @ ",league_input), subtitle = paste0(year_input," - (number of games in season)")) +
    ggplot2::theme(legend.title = ggplot2::element_blank())

}
