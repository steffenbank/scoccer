
#' plot poisson model
#'
#' @param relative_strength_input input object from \code{sco_relative_strength}
#'
#' @return a data frame object to plot result of model
#' @export
#'
#'
sco_poisson_xg_predictions <- function(relative_strength_input) {


  # ---------------------------------------------------------- #
  # comparison data
  sco_poisson_xg(relative_strength_input) %>%
    dplyr::select(-n_previous_games,-prediction) %>% tidyr::unnest() %>%
    tidyr::gather(., key = "goals", value = "pct",-hometeam,-awayteam) %>%
    tidyr::gather(., key = "position", value = "team",-goals,-pct) %>%
    dplyr::filter(stringr::str_sub(goals,1,4) == stringr::str_sub(position,1,4)) %>%
    dplyr::mutate(goals = stringr::str_sub(goals,10,10)) -> model_data

 

  # ---------------------------------------------------------- #
  # add matchup
  matchup <- paste0(dplyr::filter(model_data,position == 'hometeam') %>% dplyr::filter(row_number() == 1) %>% dplyr::pull(team),"-",
                    dplyr::filter(model_data,position == 'awayteam') %>% dplyr::filter(row_number() == 1) %>% dplyr::pull(team))
  model_data <- model_data %>% dplyr::mutate(matchup = matchup)


}



