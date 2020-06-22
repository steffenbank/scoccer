
#' Plot cards given by each referee in a season
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a ggplot2 object of cards by refs in \code{year_input} of \code{league_input}
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_ref_card_plot <- function(year_input, league_input) {

  # ---------------------------------------------------------- #
  # compute cards for each referee and devide into subcategories
  sco_acquire(year_input,league_input) -> data


  data$hr <- if(any(names(data) == 'hr')) {data$hr} else {0}
  data$ar <- if(any(names(data) == 'ar')) {data$ar} else {0}


  data %>%
    dplyr::mutate(cards = .data$hy + .data$ay + .data$hr + .data$ar) %>%
    dplyr::group_by(.data$referee) %>%
    dplyr::summarise(cards = sum(.data$cards), games = dplyr::n()) -> each_ref


  # ---------------------------------------------------------- #
  # compute total cards for each referee
  each_ref %>%
    ggplot2::ggplot(.,ggplot2::aes(x = reorder(paste0(.data$referee," (",games,")"),.data$cards/.data$games), fill = reorder(.data$cards/.data$games,.data$cards/.data$games), y = .data$cards/.data$games)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = rev(viridis::viridis_pal(option = "B")(15))) +
    ggplot2::labs(x = "", y = "", title = paste0("Cards per game @ ",league_input), subtitle = paste0(year_input," - (number of games in season)")) +
    ggplot2::theme(legend.position = "none")

}
