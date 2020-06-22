
#' plot poisson model
#'
#' @param relative_strength_input input object from \code{sco_relative_strength}
#'
#' @return a data frame object to plot result of model
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_poisson_xg_predictions <- function(relative_strength_input) {


  # ---------------------------------------------------------- #
  # comparison data
  sco_poisson_xg(relative_strength_input) %>%
    dplyr::select(-n_previous_games,-prediction) %>% tidyr::unnest() %>%
    tidyr::gather(.data, key = "goals", value = "pct",-hometeam,-awayteam) %>%
    tidyr::gather(.data, key = "position", value = "team",-goals,-pct) %>%
    dplyr::filter(stringr::str_sub(.data$goals,1,4) == stringr::str_sub(.data$position,1,4)) %>%
    dplyr::mutate(goals = stringr::str_sub(.data$goals,10,10)) -> model_data



  # ---------------------------------------------------------- #
  # add matchup
  matchup <- paste0(dplyr::filter(model_data,.data$position == 'hometeam') %>% dplyr::filter(dplyr::row_number() == 1) %>% dplyr::pull(.data$team),"-",
                    dplyr::filter(model_data,.data$position == 'awayteam') %>% dplyr::filter(dplyr::row_number() == 1) %>% dplyr::pull(.data$team))
  model_data <- model_data %>% dplyr::mutate(matchup = .data$matchup)


}



