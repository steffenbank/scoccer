
#' Plot cards given to each team in a season (home and away)
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a ggplot2 object of cards by refs in \code{year_input} of \code{league_input}
#' @export
#'
sco_team_card_plot <- function(year_input, league_input) {

  # ---------------------------------------------------------- #
  # home team per team
    sco_acquire(year_input,league_input) %>%
      dplyr::mutate(cards = hy + ay + hr + ar) %>%
      dplyr::mutate(card_group = dplyr::if_else(cards %in% c(0,1,2), "0 - 2",
                                  dplyr::if_else(cards %in% c(3,4), "3 - 4",
                                          dplyr::if_else(cards %in% c(5,6), "5 - 6","6 - ")))) %>%
      dplyr::group_by(team = hometeam,card_group = cards) %>%
      dplyr::tally() %>%
      dplyr::mutate(ven = 'home') -> each_home_team

  # ---------------------------------------------------------- #
  # home team total and join
  each_home_team %>%
    dplyr::group_by(team) %>%
    dplyr::summarise(n_total = sum(n)) -> total_home_team

  dplyr::left_join(each_home_team,total_home_team, by = NULL) %>% dplyr::mutate(pct = n/n_total) -> home


    ## ---------------------------------------------------------- #
    # away team per team
  sco_acquire(year_input,league_input) %>%
    dplyr::mutate(cards = hy + ay + hr + ar) %>%
      dplyr::mutate(card_group = if_else(cards %in% c(0,1,2), "0 - 2",
                                  if_else(cards %in% c(3,4), "3 - 4",
                                          if_else(cards %in% c(5,6), "5 - 6","6 - ")))) %>%
      dplyr::group_by(team = awayteam,card_group = cards) %>%
      dplyr::tally() %>%
      dplyr::mutate(ven = 'away') -> each_away_team

    # ---------------------------------------------------------- #
    # home team total and join
    each_away_team %>%
      dplyr::group_by(team) %>%
      dplyr::summarise(n_total = sum(n)) -> total_away_team

    left_join(each_away_team,total_away_team, by = NULL) %>% mutate(pct = n/n_total) -> away

    # ---------------------------------------------------------- #
    # join and plot
    dplyr::bind_rows(home,away) %>%
      ggplot2::ggplot(.,ggplot2::aes(x = paste0(team," (",n_total*2,")"), fill = as.factor(card_group), y = pct)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme_minimal() +
      ggplot2::scale_fill_manual(values = rev(viridis::viridis_pal(option = "B")(12))) +
      ggplot2::labs(x = "", y = "", title = paste0("Times cards given by team @ ", league_input), subtitle = paste0(year_input," - (number of games in season)")) +
      ggplot2::theme(legend.title = ggplot2::element_blank()) +
      ggplot2::facet_wrap(~ven)

  }
