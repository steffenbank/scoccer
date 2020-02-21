
sco_bet_acquire("1920","sco_ch") %>% mutate(league = "ch") %>%
  bind_rows(.,sco_bet_acquire("1920","sco_pl") %>% mutate(league = "pl")) -> t
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggrepel)

glimpse(t)

t %>%
  select(hometeam, awayteam, b365h,b365ch,b365d,b365cd,b365a,b365ca,league) %>%
  mutate(h_diff = b365ch - b365h,
         d_diff = b365cd - b365d,
         a_diff = b365ca - b365a) %>%
  arrange(-desc(h_diff)) %>%
  ggplot(.,aes(x = h_diff, y = a_diff, color = league)) + geom_point(alpha = .3) + ylim(-2,2) + geom_smooth(se = F, method = "lm")


# how much does odds change for each team when the yare home?
t %>%
  select(hometeam, awayteam, b365h,b365ch,b365d,b365cd,b365a,b365ca,league) %>%
  mutate(h_diff = b365ch - b365h,
         d_diff = b365cd - b365d,
         a_diff = b365ca - b365a) %>%
  group_by(hometeam,league) %>%
  summarise(own_odds = mean(h_diff), other_odds = mean(a_diff)) %>%
  gather(.,key = "type", val = "odds", c(-league,-hometeam)) %>%
  mutate(indi = if_else(odds > 0, 1,-1)) %>%
  ggplot(.,aes(x = type, y = odds, fill = as.factor(indi))) + geom_bar(stat = "identity", position = position_dodge(), alpha = .4) + facet_wrap(~reorder(hometeam,league)) +
  coord_flip() + scale_fill_manual(values = c("red","darkgreen")) + theme_minimal() +
  geom_text(aes(x = type, y = odds+(indi*0.15), label = round(odds,2)), size = 3, color = "black") +
  labs(subtitle = " Red = odds reduced from open til close (back early)\n Green = odds increased from open til close (back late)")


t %>%
  select(hometeam, awayteam, b365h,b365ch,b365d,b365cd,b365a,b365ca,league) %>%
  mutate(h_diff = b365ch - b365h,
         d_diff = b365cd - b365d,
         a_diff = b365ca - b365a) %>%
  group_by(awayteam,league) %>%
  summarise(own_odds = mean(h_diff), other_odds = mean(a_diff)) %>%
  gather(.,key = "type", val = "odds", c(-league,-awayteam)) %>%
  mutate(indi = if_else(odds > 0, 1,-1)) %>%
  ggplot(.,aes(x = type, y = odds, fill = as.factor(indi))) + geom_bar(stat = "identity", position = position_dodge(), alpha = .4) + facet_wrap(~reorder(awayteam,league)) +
  coord_flip() + scale_fill_manual(values = c("red","darkgreen")) + theme_minimal() +
  geom_text(aes(x = type, y = odds+(indi*0.15), label = round(odds,2)), size = 3, color = "black") +
  labs(subtitle = " Red = odds reduced from open til close (back early)\n Green = odds increased from open til close (back late)")


# e

bind_rows(
  sco_bet_acquire("1920","sco_ch"),
  sco_bet_acquire("1819","sco_ch"),
  sco_bet_acquire("1718","sco_ch"),
  sco_bet_acquire("1617","sco_ch"),
  sco_bet_acquire("1516","sco_ch")) -> t2


t2 %>%
  ggplot(.,aes(x = b365h, fill = awayteam)) + geom_histogram(alpha = 0.3, binwidth = .1) + facet_wrap(~awayteam, scales = "free")
  bind_rows(.,sco_bet_acquire("1920","sco_pl") %>% mutate(league = "pl")) -> t

t2 %>%
  filter(hometeam == 'Celtic') %>%
  mutate(away_win = if_else(ftag > fthg, TRUE,FALSE)) %>%
  ggplot(.,aes(x = awayteam, y = b365h, color = away_win)) + geom_point() + coord_flip()




# how much does odds change for each team when the yare away?
t %>%
  select(hometeam, awayteam, b365h,b365ch,b365d,b365cd,b365a,b365ca,league) %>%
  mutate(h_diff = b365ch - b365h,
         d_diff = b365cd - b365d,
         a_diff = b365ca - b365a) %>%
  group_by(awayteam,league) %>%
  summarise(own_odds = mean(h_diff), draw = mean(d_diff), other_odds = mean(a_diff)) %>%
  gather(.,key = "type", val = "odds", c(-league,-awayteam)) %>%
  ggplot(.,aes(x = type, y = odds, fill = league)) + geom_bar(stat = "identity", position = position_dodge()) + facet_wrap(~awayteam) + coord_flip()


  gather(., key = "key", val = "oddds", c(-hometeam,-awayteam)) %>%
  mutate(type = if_else(grepl("h",key),"home",
                        if_else(grepl("d",key),"draw", "away"))) %>%
  #group_by(hometeam,awayteam) %>%
  spread(key, oddds,c(-hometeam,-awayteam,-type))
  group_by(x = str_sub(nchar(key)-1,nchar(key)))


t %>%
  filter(awayteam == 'Motherwell') %>%
  select(hometeam, b365.2.5, b365c.2.5) %>%
  pivot_longer(., cols = c('b365.2.5','b365c.2.5'),names_to = "type",values_to = "odds") %>%
  ggplot(.,aes(x = hometeam, y = odds, color = type)) + geom_point() + coord_flip()


t %>%
  filter(hometeam == 'St Johnstone') %>%
  select(awayteam, b365.2.5, b365c.2.5) %>%
  pivot_longer(., cols = c('b365.2.5','b365c.2.5'),names_to = "type",values_to = "odds") %>%
  ggplot(.,aes(x = awayteam, y = odds, color = type)) + geom_point() + coord_flip()

