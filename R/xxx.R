

library(rvest)   # 0.3.5
library(polite)  # 0.1.1
library(dplyr)   # 0.8.5
library(tidyr)   # 1.0.2
library(purrr)   # 0.3.4
library(stringr) # 1.4.0
library(glue)    # 1.4.0
library(rlang)   # 0.4.6
library(ggplot)
library(ggrepel)
library(ggforce)
library(scales)



premiership2021_page <- "https://fbref.com/en/comps/40/Scottish-Premiership-Stats" # overview stats
#premiership19_page <- read_html("https://fbref.com/en/comps/40/3289/2019-2020-Scottish-Premiership-Stats")
#premiership_page <- read_html("https://fbref.com/en/comps/50/Superliga-Stats")
#premiership_page <- read_html("https://fbref.com/en/comps/9/Premier-League-Stats")
#premiership_page <- read_html("https://fbref.com/en/comps/72/Scottish-Championship-Stats")



premiership_links <- paste0('//*[@id="results107501_overall"]/tbody/tr[',1:12,']/td[1]/a') # find links of each
#premiership_links <- paste0('//*[@id="results32890_overall"]/tbody/tr[',c(1,2,3,4,5,6,8,9,10,11,12,13),']/td[1]/a')
#premiership_links <- paste0('//*[@id="results32291_overall"]/tbody/tr[',c(1,2,3,4,5,6,8,9,10,11,12,14,14,15),']/td[1]/a')
#premiership_links <- paste0('//*[@id="results32321_overall"]/tbody/tr[',seq(1,20,1),']/td[1]/a')
#premiership_links <- paste0('//*[@id="results32901_overall"]/tbody/tr[',seq(1,10,1),']/td[1]/a')



sco_player_club_data <- function(page_input,link_input) {


read_html(paste0("https://fbref.com/",read_html(page_input) %>%
                   html_node(xpath = link_input) %>%
                   html_attr('href'))) %>%
                   html_table() -> all_tables


all_tables[[1]][,1:9] %>% as.data.frame() -> player_data

names(player_data) <- player_data[1,]
player_data <- player_data[-1,]
player_data <- player_data[1:nrow(player_data)-1,]
player_data$Team <- read_html(page_input) %>%
  html_node(xpath = link_input) %>%
  html_text()

return(player_data)


}

sco_player_club_data(premiership2021_page, premiership_links[1]) -> t

sco_player_all_data <- function(page_input, link_input) {
  map2_df(list(page_input),link_input,sco_player_club_data) -> xx

}

xuab <- sco_player_all_data(premiership2021_page,premiership_links) # husk at det skal være en liste





datafinal %>%
  #filter(Gls > 0 | Ast > 0) %>%
  left_join(., datafinal %>% group_by(Team) %>% summarise(Gls_total = sum(as.numeric(Gls),na.rm = TRUE), Ast_total = sum(as.numeric(Ast),na.rm = TRUE))) %>%
  mutate(goal_contrib = as.numeric(Gls)/Gls_total,
         assist_contrib = as.numeric(Ast)/Ast_total) -> contribution


## Description text
desc_hazard <- "Hazard FC: With 16 goals and 15 assists Eden Hazard has been involved in the most goals for a team this season."
desc_vardymurray <- "Scoring 37.5% and 37.1% of their team's goals, Jamie Vardy and Glen Murray have proven to be talismans for their team yet again!"
desc_fraser <- "Another fantastic season from Ryan Fraser with 7 goals and 14 assists (one behind league-leader Hazard)"

title_input <- "PL (19/20)"



contribution %>%
ggplot(aes(assist_contrib, goal_contrib)) +
  geom_point(data = contribution %>%
               filter(goal_contrib < 0.25 | assist_contrib < 0.15),
             color = "grey20", size = 4, alpha = 0.2) +
  geom_point(data = contribution %>%
               filter(goal_contrib > 0.25 | assist_contrib > 0.15),
             color = "red", size = 4) +
  geom_hline(yintercept = 0.25, color = "grey20", alpha = 0.4) +
  geom_vline(xintercept = 0.15, color = "grey20", alpha = 0.4) +
  geom_text_repel(data = contribution %>%
                    filter(goal_contrib > 0.25 | assist_contrib > 0.15,
                           !Player %in% c("E. Hazard", "R. Fraser", "J. Vardy", "G. Murray")),
                  aes(label = Player, family = "Roboto Condensed", fontface = "bold"),
                  seed = 15, size = 5,
                  min.segment.length = 0, segment.color = "red",
                  point.padding = 0.5) +
  geom_mark_circle(aes(filter = Player == "E. Hazard", label = "Eden Hazard",
                       description = desc_hazard),
                   label.family = "Roboto Condensed", label.fontsize = c(14, 10)) +
  geom_mark_hull(aes(filter = Player %in% c("G. Murray", "J. Vardy"), label = "Vardy & Murray",
                     description = desc_vardymurray),
                 label.buffer = unit(20, "mm"), label.fontsize = c(14, 10),
                 label.family = "Roboto Condensed") +
  geom_mark_circle(aes(filter = Player == "R. Fraser", label = "Ryan Fraser",
                       description = desc_fraser),
                   label.buffer = unit(9.8, "mm"), label.fontsize = c(14, 10),
                   label.family = "Roboto Condensed") +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3),
                     limits = c(0, 0.3)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     limits = c(0, 0.5)) +
  labs(title = glue("Goal Contribution Matrix: ", title_input),
       subtitle = "Team goal involvement as percentage of total club goals* and/or assists.",
       caption = glue("
                      *Own goals not counted in total/player can play at multiple clubs
                      Data: soccerway.com
                      By: @SteffenBank"),
       x = "Percentage of club goals assisted",
       y = "Percentage of club goals scored") +
  theme_minimal() +
  theme(text = element_text(family = "Roboto Condensed"),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 10),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        panel.grid.minor.x = element_blank()) -> goal_contribution_matrix

goal_contribution_matrix


ggsave(plot = goal_contribution_matrix,
       "plot.png",
       height = 9, width = 11)




url <- "https://uk.soccerway.com/teams/scotland/rangers-fc/1899/"

session <- bow(url)

team_links <- scrape(session) %>%
  #html_nodes("#page_competition_1_block_competition_tables_8_block_competition_league_table_1_table .large-link a") %>%
  html_attr("href")


url <- "https://fbref.com/en/squads/86b7acd2/Rangers-Stats"




  page <- read_html("https://fbref.com/en/comps/40/Scottish-Premiership-Stats")
  page %>% html_node(xpath = '//*[@id="results107501_overall"]/tbody/tr[3]/td[1]/a') %>% html_attr('href')

















