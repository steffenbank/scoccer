

#' Player data from scottish divisions
#'
#' @param year_input input as character in years of seasons, eg. "yyyy"
#' @param league_input sco_pl (Premiership) or sco_ch (Championship)
#'
#' @return a dataframe of player data
#' @export
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
sco_player_data <- function(year_input,league_input) {

  # ---------------------------------------------------------- #
  # predifned links
  if(year_input == '2021' & league_input == "sco_pl") {
    page <- "https://fbref.com/en/comps/40/Scottish-Premiership-Stats"
    link <- paste0('//*[@id="results107501_overall"]/tbody/tr[',1:12,']/td[1]/a')

  } else if(year_input == '1920' & league_input == "sco_pl") {
    page <- "https://fbref.com/en/comps/40/3289/2019-2020-Scottish-Premiership-Stats"
    link <- paste0('//*[@id="results32890_overall"]/tbody/tr[',c(1,2,3,4,5,6,8,9,10,11,12,13),']/td[1]/a')
  } else if(year_input == '1920' & league_input == "sco_ch") {
    page <- "https://fbref.com/en/comps/72/Scottish-Championship-Stats"
    link <- paste0('//*[@id="results32901_overall"]/tbody/tr[',seq(1,10,1),']/td[1]/a')
  }


  # ---------------------------------------------------------- #
  # define gather function
  gather_func <- function(page_input, link_input) {

  # read all tables from link/page
  xml2::read_html(paste0("https://fbref.com/",xml2::read_html(page_input) %>%
                     rvest::html_node(xpath = link_input) %>%
                       rvest::html_attr('href'))) %>%
      rvest::html_table() -> all_tables

  # acauire relevant player data
  all_tables[[1]][,1:9] %>%
    as.data.frame() -> player_data

  # Tidy data to desireable format
  names(player_data) <- player_data[1,] # change first row to header
  player_data <- player_data[-1,]
  player_data <- player_data[1:nrow(player_data)-1,]

  player_data$Team <- xml2::read_html(page_input) %>% # add teamname
  rvest::html_node(xpath = link_input) %>%
  rvest::html_text()

    return(player_data)
  }

  # ---------------------------------------------------------- #
  # run function for all clubs and return
  purrr::map2_df(page, link, gather_func) -> final

return(final)

  }

