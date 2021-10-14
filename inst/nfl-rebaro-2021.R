library(dplyr)
library(kableExtra)

nfl_dat <- read.csv("data-raw/nfl-sched-2021.csv") %>%
    mutate(Place=ifelse(X=="@", "Away", "Home"))

game_dat <- nfl_dat %>%
    filter(X.1=="boxscore")

future_dat <- nfl_dat %>%
    filter(X.1=="preview")

game_stats <- game_dat %>%
    transmute(Date=lubridate::ymd(Date),
              Team=Winner.tie,
              Opp=Loser.tie,
              Place=Place,
              PF=PtsW,
              PA=PtsL) %>%
    bind_rows(game_dat %>%
                  transmute(Date=lubridate::ymd(Date),
                            Team=Loser.tie,
                            Opp=Winner.tie,
                            Place=ifelse(Place=="Home", "Away", "Home"),
                            PF=PtsL,
                            PA=PtsW)) %>%
    group_by(Opp) %>%
    mutate(PD=PF-PA,
           Opp_PFpg=(sum(PA) - PA)/(n() - 1),
           Opp_PApg=(sum(PF) - PF)/(n() - 1),
           Opp_PDpg=Opp_PFpg-Opp_PApg) %>%
    ungroup() %>%
    mutate(PF_above=PF-Opp_PApg,
           PA_below=Opp_PFpg-PA,
           P_better=PF_above + PA_below,
           Win=ifelse(PF > PA, 1,0))

sos <- game_stats %>%
    group_by(Team) %>%
    summarize(Opp_PF=mean(Opp_PFpg),
              Opp_PA=mean(Opp_PApg),
              Opp_PD=Opp_PF-Opp_PA,
              Wins=sum(Win)) %>%
    arrange(desc(Opp_PD))

rebaro <- game_stats %>%
    group_by(Team) %>%
    summarize(Opp_PF=mean(Opp_PFpg),
              Opp_PA=mean(Opp_PApg),
              Opp_PD=Opp_PF-Opp_PA,
              PF_above=mean(PF_above),
              PA_below=mean(PA_below),
              P_better=PF_above + PA_below,
              Win_pct=mean(Win) %>% round(2)) %>%
    arrange(desc(P_better), desc(Opp_PD))

hfa <- game_stats %>%
    group_by(Place) %>%
    summarize(PF=mean(PF),
              PA=mean(PA),
              PD=PF-PA,
              PF_above=mean(PF_above),
              PA_below=mean(PA_below),
              ReBaRO=PF_above+PA_below)

future_stats <- future_dat %>%
    transmute(Date=lubridate::ymd(Date),
              Team=Winner.tie,
              Opp=Loser.tie,
              Place=Place) %>%
    bind_rows(game_dat %>%
                  transmute(Date=lubridate::ymd(Date),
                            Team=Loser.tie,
                            Opp=Winner.tie,
                            Place=ifelse(Place=="Home", "Away", "Home"))) %>%
    left_join(rebaro, by=c("Opp" = "Team")) %>%
    group_by(Team) %>%
    summarize(future_rebaro = mean(P_better),
              future_win_pct = mean(Win_pct))
