
library(magrittr)
library(dplyr)
library(scoccer)
library(purrr)
library(ggplot2)


rep(paste0(seq(15,19,1),seq(16,20,1)),2) -> a
c(rep("sco_pl",length(a)/2),rep("sco_ch",length(a)/2)) -> b

map2_df(a,b,sco_acquire) -> d
  
  


d %>% mutate(eff_hs = hst/hs, eff_as = ast/as) -> d1 

bind_rows(
d %>% group_by(team = hometeam) %>% summarise(games = n(), s = mean(hs,na.rm = TRUE), st = mean(hst,na.rm = TRUE), g = mean(fthg)) %>% mutate(ven = "home"),
d %>% group_by(team = awayteam) %>% summarise(games = n(), s = mean(as,na.rm = TRUE), st = mean(ast,na.rm = TRUE), g = mean(ftag)) %>% mutate(ven = "away")) -> d2
 


 ggplot(d2 %>% na.omit(),aes(x = reorder(team,s), y = s, fill = team)) + geom_bar(stat = "identity") + 
   facet_wrap(~ven) + coord_flip() + labs(x = "", y = "", title = "Shots per game", subtitle = "Scottish Premiership and Championship (2015-2019)", 
                                          source = "https://www.football-data.co.uk/ + https://github.com/steffenbank/scoccer") +
  theme_minimal() +
  scale_fill_viridis_d() + 
  geom_text(aes(label = round(s,2), color = team), hjust = -0.2, size = 3, fontface = "bold") + theme(legend.position = "none") +
  scale_color_viridis_d() + ggsave("shots.png", width = 15, height = 8, dpi = 300)
 
 ggplot(d2 %>% na.omit(),aes(x = reorder(team,st), y = st, fill = team)) + geom_bar(stat = "identity") + 
   facet_wrap(~ven) + coord_flip() + labs(x = "", y = "", title = "Shots on target per game", subtitle = "Scottish Premiership and Championship (2015-2019)", 
                                          source = "https://www.football-data.co.uk/ + https://github.com/steffenbank/scoccer") +
   theme_minimal() +
   scale_fill_viridis_d() + 
   geom_text(aes(label = round(st,2), color = team), hjust = -0.2, size = 3, fontface = "bold") + theme(legend.position = "none") +
   scale_color_viridis_d() + ggsave("shots_target.png", width = 15, height = 8, dpi = 300)
 
 ggplot(d2 %>% na.omit(),aes(x = reorder(team,g), y = g, fill = team)) + geom_bar(stat = "identity") + 
   facet_wrap(~ven) + coord_flip() + labs(x = "", y = "", title = "Goals scored per game", subtitle = "Scottish Premiership and Championship (2015-2019)", 
                                          source = "https://www.football-data.co.uk/ + https://github.com/steffenbank/scoccer") +
   theme_minimal() +
   scale_fill_viridis_d() + 
   geom_text(aes(label = round(g,2), color = team), hjust = -0.2, size = 3, fontface = "bold") + theme(legend.position = "none") +
   scale_color_viridis_d() + ggsave("goals.png", width = 15, height = 8, dpi = 300)
 
 
 



summary(m1 <- glm(num_awards ~ prog + math, family="poisson", data=p))

d %>% filter(hometeam == 'Celtic' & awayteam == 'Rangers')


