

data %>% filter(hometeam == 'Celtic' | awayteam == 'Celtic') %>%
  mutate(corner_diff = if_else(hometeam == 'Celtic',hc-ac,ac-hc)) %>% select(hometeam,awayteam,hc,ac,corner_diff)


data %>% filter(hometeam == 'Motherwell' | awayteam == 'Motherwell') %>%
  mutate(corner_diff = if_else(hometeam == 'Motherwell',hc-ac,ac-hc)) %>% select(hometeam,awayteam,hc,ac,corner_diff)
