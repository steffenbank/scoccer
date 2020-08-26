
#' Goals per minute
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#' @param min_goals minimum goals to be scored to be shown
#' @param min_matches minimum matches played to be shown
#'
#' @return a dataframe of player data
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @return
#' @export
#'
sco_goal_per_minute <- function(year_input, league_input, min_goals, min_matches) {

  # ---------------------------------------------------------- #
  # create titles
  if(year_input == '2021' & league_input == "sco_pl") {
    title_input <- "Scottish Premiership (20/21)"
  } else if(year_input == '1920' & league_input == "sco_pl") {
    title_input <- "Scottish Premiership (19/21)"
  } else if(year_input == '1920' & league_input == "sco_ch") {
    title_input <- "Scottish Championship (19/20)"
  }

  # ---------------------------------------------------------- #
  # create plot
  sco_player_data(year_input,league_input) %>%
    dplyr::filter(as.numeric(.data$Gls) >= min_goals & as.numeric(.data$MP) >= min_matches) %>%
    dplyr::mutate(val = as.numeric(gsub(",","",.data$Min))/as.numeric(.data$Gls), param = "Goals") %>%
    dplyr::select(.data$Player,.data$Team,.data$val,.data$param) %>%
    ggplot2::ggplot(.data, ggplot2::aes(y = .data$val, x = stats::reorder(paste0(.data$Player, " (",.data$Team,")"),.data$val), fill = .data$val)) + ggplot2::geom_bar(stat = "identity") + ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::  scale_fill_gradient(low = "darkgreen", high = "red", na.value = NA) +
    ggplot2::theme(text = ggplot2::element_text(family = "Roboto Condensed"),
                   title = ggplot2::element_text(size = 18),
                   plot.subtitle = ggplot2::element_text(size = 16),
                   plot.caption = ggplot2::element_text(size = 10),
                   axis.title = ggplot2::element_text(size = 14),
                   axis.text = ggplot2::element_text(size = 12),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(title = paste0("Goal per minute: ", title_input),
                  subtitle = paste0("Atleast ", min_matches, " played and ", min_goals, " scored."),
                  caption = paste0("
                      Data: soccerway.com
                      By: @SteffenBank"),
                  x = "Minutes",
                  y = " ") +
    ggplot2::theme(legend.position = "none")

}






