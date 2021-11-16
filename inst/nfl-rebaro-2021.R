library(dplyr)
library(kableExtra)

nfl_dat <- read.csv("data-raw/nfl-sched-2021.csv") %>%
    mutate(Home=X!="@")

game_dat <- nfl_dat %>%
    filter(X.1=="boxscore")

future_dat <- nfl_dat %>%
    filter(X.1=="preview")

calc_rebaro <- function(dat,
                        date_col="Date",
                        team_col="Winner.tie",
                        opp_col="Loser.tie",
                        place_col="Home",
                        pf_col="PtsW",
                        pa_col="PtsL"){

    game_stats <- tibble(Date=lubridate::ymd(dat[[date_col]]),
                         Team=dat[[team_col]],
                         Opp=dat[[opp_col]],
                         Home=dat[[place_col]],
                         PF=dat[[pf_col]],
                         PA=dat[[pa_col]],
                         PD=PF-PA) %>%
        bind_rows(tibble(Date=lubridate::ymd(dat[[date_col]]),
                         Team=dat[[opp_col]],
                         Opp=dat[[team_col]],
                         Home=!dat[[place_col]],
                         PF=dat[[pa_col]],
                         PA=dat[[pf_col]],
                         PD=PF-PA))

    teams <- unique(game_stats$Team)
    game_rebaros <- c()
    for(i in 1:length(teams)){
        tmp <- game_stats %>%
            filter(Team == teams[i])
        tmp_opp <- game_stats %>%
            filter(Team %in% tmp$Opp,
                   Opp != teams[i]) %>%
            group_by(Team) %>%
            summarize(Opp_PF = mean(PF),
                      Opp_PA = mean(PA),
                      Opp_PD = mean(PD),
                      Opp_PF_med = median(PF),
                      Opp_PA_med = median(PA),
                      Opp_PD_med = median(PD),
                      .groups = "drop")
        tmp_rebaro <- tmp %>%
            left_join(tmp_opp, by=c("Opp" = "Team")) %>%
            ungroup() %>%
            mutate(PF_above = PF - Opp_PA,
                   PA_below = Opp_PF - PA,
                   ReBaRO = PF_above + PA_below,
                   PF_above_med = PF - Opp_PA_med,
                   PA_below_med = Opp_PF_med - PA,
                   ReBaRO_med = PD + Opp_PD_med,
                   Win=ifelse(PF > PA, 1,0))
        game_rebaros <- bind_rows(game_rebaros,
                                  tmp_rebaro)
    }
    return(game_rebaros)
}

game_rebaros <- calc_rebaro(game_dat)

yard_rebaros <- calc_rebaro(game_dat, pf_col = "YdsW", pa_col="YdsL") %>%
    select(-Win)

sos <- game_rebaros %>%
    group_by(Team) %>%
    summarize(Opp_PF=mean(Opp_PFpg),
              Opp_PA=mean(Opp_PApg),
              Opp_PD=mean(Opp_PDpg),
              Wins=mean(Win)) %>%
    arrange(desc(Opp_PD))

rebaro <- game_rebaros %>%
    group_by(Team) %>%
    summarize(PF_above=mean(PF_above),
              PA_below=mean(PA_below),
              rebaro_ovr=mean(ReBaRO),
              PF_above_med=median(PF_above_med),
              PA_below_med=median(PA_below_med),
              rebaro_ovr_med=median(ReBaRO_med),
              MAD = median(abs(ReBaRO_med - rebaro_ovr_med)),
              med_over_mad = rebaro_ovr_med / MAD,
              Win_pct=mean(Win) %>% round(2)) %>%
    arrange(desc(rebaro_ovr_med), desc(med_over_mad))

hfa <- game_rebaros %>%
    group_by(Place) %>%
    summarize(PF=median(PF),
              PA=median(PA),
              PD=median(PD),
              PF_above=median(PF_above),
              PA_below=median(PA_below),
              ReBaRO=median(P_better),
              .groups="drop")

future_stats <- future_dat %>%
    transmute(Date=lubridate::ymd(Date),
              Team=Winner.tie,
              Opp=Loser.tie,
              Place=Place) %>%
    bind_rows(future_dat %>%
                  transmute(Date=lubridate::ymd(Date),
                            Team=Loser.tie,
                            Opp=Winner.tie,
                            Place=ifelse(Place=="Home", "Away", "Home"))) %>%
    left_join(rebaro, by=c("Opp" = "Team")) %>%
    group_by(Team) %>%
    summarize(future_rebaro = mean(ReBaRO),
              future_win_pct = mean(Win_pct))


yard_stats <- game_dat %>%
    transmute(Date=lubridate::ymd(Date),
              Team=Winner.tie,
              Opp=Loser.tie,
              Place=Place,
              YF=YdsW,
              YA=YdsL) %>%
    bind_rows(game_dat %>%
                  transmute(Date=lubridate::ymd(Date),
                            Team=Loser.tie,
                            Opp=Winner.tie,
                            Place=ifelse(Place=="Home", "Away", "Home"),
                            YF=YdsL,
                            YA=YdsW)) %>%
    group_by(Opp) %>%
    mutate(YD=YF-YA,
           Opp_YFpg=(sum(YA) - YA)/(n() - 1),
           Opp_YApg=(sum(YF) - YF)/(n() - 1),
           Opp_YDpg=Opp_YFpg-Opp_YApg) %>%
    ungroup() %>%
    mutate(YF_above=YF-Opp_YApg,
           YA_below=Opp_YFpg-YA,
           Y_better=YF_above + YA_below,
           Win=ifelse(YF > YA, 1,0))

rebaro_yds <- yard_stats %>%
    group_by(Team) %>%
    summarize(Opp_YF=mean(Opp_YFpg),
              Opp_YA=mean(Opp_YApg),
              Opp_YD=Opp_YF-Opp_YA,
              YF_above=mean(YF_above),
              YA_below=mean(YA_below),
              Y_better=YF_above + YA_below,
              Win_pct=mean(Win) %>% round(2)) %>%
    arrange(desc(Y_better), desc(Opp_YD))
