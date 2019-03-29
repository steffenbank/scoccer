# scoccer

from http://www.football-data.co.uk

## Acquire basic data

The most usefull functions within the packages are \code{sco_acquire} and \code{sco_relative_strength}. Description of fields are availble below @ the column notes. Currently are two leagues available:

1. Scottish Premiership ("sco_pl")
2. Scottish Champtionship ("sco_ch")

Years from 2001 till now are available - season inputs may be specificed as "yyyy", for instance 2018/2019 season "1819". 

Data from Scottish Premiership 2017/2018:
``` r
sco_acquire("1718", "sco_pl") -> dat
head(dat) # data from Scottish Premiership 2017/2018
```

Relative strength between Ayr and Partick Thistle the last five games. May be used for model purposes
``` r
sco_relative_strength("1819","sco_pl","Ayr","Partick",5) 
```

# Modeling functions

## Column notes
Key to results data:

Div = League Division
Date = Match Date (dd/mm/yy)
HomeTeam = Home Team
AwayTeam = Away Team
FTHG and HG = Full Time Home Team Goals
FTAG and AG = Full Time Away Team Goals
FTR and Res = Full Time Result (H=Home Win, D=Draw, A=Away Win)
HTHG = Half Time Home Team Goals
HTAG = Half Time Away Team Goals
HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)

Match Statistics (where available)
Attendance = Crowd Attendance
Referee = Match Referee
HS = Home Team Shots
AS = Away Team Shots
HST = Home Team Shots on Target
AST = Away Team Shots on Target
HHW = Home Team Hit Woodwork
AHW = Away Team Hit Woodwork
HC = Home Team Corners
AC = Away Team Corners
HF = Home Team Fouls Committed
AF = Away Team Fouls Committed
HFKC = Home Team Free Kicks Conceded
AFKC = Away Team Free Kicks Conceded
HO = Home Team Offsides
AO = Away Team Offsides
HY = Home Team Yellow Cards
AY = Away Team Yellow Cards
HR = Home Team Red Cards
AR = Away Team Red Cards
HBP = Home Team Bookings Points (10 = yellow, 25 = red)
ABP = Away Team Bookings Points (10 = yellow, 25 = red)
