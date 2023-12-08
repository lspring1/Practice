library(RCurl)
library(png)
library(hoopR)
library(tidyverse)
library(knitr)
library(kableExtra)
library(tinytex)
library(bigballR)
library("xlsx")
library(ncaahoopR)
library(hoopR)
library(ggplot2)
library("toRvik")
library(gt)
library(gtExtras)
library(gtsummary)
library(dplyr)

ncaa_game_id <- 5583867
espn_game_id <- 401581085
Opponent <- "Towson"

opponent_roster <- get_roster("Towson")

opponent_roster$Player <- opponent_roster$name

player_image <- c(
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FDaniel_Hankins_Sanford_d3tPh.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJaylen_HWhjg.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRahsool_Diggins_nOrSz.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRobert_F5SIh.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FKeon_Thompson_j7lVX.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FMarqui_Worthy_fB7nD.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJayden_Ndjigue_uDraA.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FTarique_Foster_JV3ru.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRyan_Marcus_zHwGy.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJackson_Cronin_SdErY.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJosh_MYSjy.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FMatt_q1H5m.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FMathok_PYwqm.jpg&width=100&height=100&gravity=north&type=webp",
  "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRollie_Castineyra_CgC7L.jpg&width=100&height=100&gravity=north&type=webp",
  "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/nextgen_2022/logo_main.svg"
)


Player <- c("Daniel Hankins-Sanford", "Jaylen Curry", "Rahsool Diggins", "Robert Davis Jr.", "Keon Thompson", "Marqui Worthy", "Jayden Ndjigue",
            "Tarique Foster", "Ryan Marcus", "Jackson Cronin", "Josh Cohen", "Matt Cross", "Mathok Majok", "Rollie Castineyra", "Totals")

player_images_current <- data.frame(player_image, Player)

UMass_vs_Opponent <- get_team_stats(play_by_play_data = get_play_by_play(ncaa_game_id), include_transition = T)

player_stats <- get_player_stats(get_play_by_play(ncaa_game_id), multi.games = F, simple = F)

#UMass Basic Stats

UMass_vs_Opponent_Basic <- player_stats

UMass_vs_Opponent_Basic <- UMass_vs_Opponent_Basic %>% filter(Team == "Massachusetts")

team_FG_per <- round(sum(UMass_vs_Opponent_Basic$FGM)/sum(UMass_vs_Opponent_Basic$FGA)*100, 1)
team_3_per <- round(sum(UMass_vs_Opponent_Basic$TPM)/sum(UMass_vs_Opponent_Basic$TPA)*100, 1)
team_FT_per <- round(sum(UMass_vs_Opponent_Basic$FTM)/sum(UMass_vs_Opponent_Basic$FTA)*100, 1)

UMass_vs_Opponent_Basic$FG <- paste0(UMass_vs_Opponent_Basic$FGM, "/", UMass_vs_Opponent_Basic$FGA)
UMass_vs_Opponent_Basic$`3FG` <- paste0(UMass_vs_Opponent_Basic$TPM, "/", UMass_vs_Opponent_Basic$TPA)
UMass_vs_Opponent_Basic$FT <- paste0(UMass_vs_Opponent_Basic$FTM, "/", UMass_vs_Opponent_Basic$FTA)
UMass_vs_Opponent_Basic$`FG%` <- UMass_vs_Opponent_Basic$FG.*100
UMass_vs_Opponent_Basic$`3FG%` <- UMass_vs_Opponent_Basic$TP.*100
UMass_vs_Opponent_Basic$`FT%` <- UMass_vs_Opponent_Basic$FT.*100
UMass_vs_Opponent_Basic$MINS <- round(UMass_vs_Opponent_Basic$MINS, 1)

UMass_vs_Opponent_Basic <- select(UMass_vs_Opponent_Basic, Player, MINS, PTS, ORB, DRB, AST, STL, BLK, TOV, FG, `FG%`, `3FG`, `3FG%`, FT, `FT%`)

UMass_vs_Opponent_Basic$Player <- gsub("\\.", " ", UMass_vs_Opponent_Basic$Player)

UMass_vs_Opponent_Basic$Player <- str_to_title(UMass_vs_Opponent_Basic$Player)

UMass_vs_Opponent_Basic <- arrange(UMass_vs_Opponent_Basic, Player)

UMass_vs_Opponent_Basic$Player <- gsub("Daniel Hankinssanford", "Daniel Hankins-Sanford", UMass_vs_Opponent_Basic$Player)

UMass_vs_Opponent_Basic <- merge(UMass_vs_Opponent_Basic, player_images_current, by = "Player")

UMass_vs_Opponent_Basic <- select(UMass_vs_Opponent_Basic, player_image, Player, MINS, PTS, ORB, DRB, AST, STL, BLK, TOV, FG, `FG%`, `3FG`, `3FG%`, FT, `FT%`)

UMass_vs_Opponent_Basic <- arrange(UMass_vs_Opponent_Basic, desc(PTS))

UMass_vs_Opponent_Basic %>% 
  gt() %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Basic Stats- `Sorted by Pts.`")) %>% 
  data_color(
    columns = c(PTS, DRB, `FT%`, `FG%`),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "") %>% 
  text_transform(
    locations = cells_body(columns = player_image),
    fn = function(x) {
      web_image(x,
                height = px(30))
    }
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0("Team Stats: ", 'FG%: ', team_FG_per, '%  | 3pt%: ', team_3_per, '%  | FT%: ', team_FT_per)) %>% 
  gtsave(paste0("UMass_Postgame_Basic_Stats.png"), path = "/Users/lukespring/Desktop/Postgame Tables")


#Opponent Basic Stats

Opponent_Basic <- player_stats

Opponent_Basic <- Opponent_Basic %>% filter(Team == Opponent)

opp_team_FG_per <- round(sum(Opponent_Basic$FGM)/sum(Opponent_Basic$FGA)*100, 1)
opp_team_3_per <- round(sum(Opponent_Basic$TPM)/sum(Opponent_Basic$TPA)*100, 1)
opp_team_FT_per <- round(sum(Opponent_Basic$FTM)/sum(Opponent_Basic$FTA)*100, 1)

Opponent_Basic$FG <- paste0(Opponent_Basic$FGM, "/", Opponent_Basic$FGA)
Opponent_Basic$`3FG` <- paste0(Opponent_Basic$TPM, "/", Opponent_Basic$TPA)
Opponent_Basic$FT <- paste0(Opponent_Basic$FTM, "/", Opponent_Basic$FTA)
Opponent_Basic$`FG%` <- Opponent_Basic$FG.*100
Opponent_Basic$`3FG%` <- Opponent_Basic$TP.*100
Opponent_Basic$`FT%` <- Opponent_Basic$FT.*100
Opponent_Basic$MINS <- round(Opponent_Basic$MINS, 1)

Opponent_Basic <- select(Opponent_Basic, Player, MINS, PTS, ORB, DRB, AST, STL, BLK, TOV, FG, `FG%`, `3FG`, `3FG%`, FT, `FT%`)

Opponent_Basic$Player <- gsub("\\.", " ", Opponent_Basic$Player)

Opponent_Basic$Player <- str_to_title(Opponent_Basic$Player)

Opponent_Basic <- arrange(Opponent_Basic, Player)

Opponent_Basic <- merge(Opponent_Basic, opponent_roster, by = "Player")

Opponent_Basic <- select(Opponent_Basic, player_image, Player, MINS, PTS, ORB, DRB, AST, STL, BLK, TOV, FG, `FG%`, `3FG`, `3FG%`, FT, `FT%`)

Opponent_Basic <- arrange(Opponent_Basic, desc(PTS))

Opponent_Basic %>% 
  gt() %>% 
  tab_header(title = md("**Towson**"), subtitle = md("Basic Stats- `Sorted by Pts.`")) %>% 
  data_color(
    columns = c(PTS, DRB, `FT%`, `FG%`),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "") %>% 
  text_transform(
    locations = cells_body(columns = player_image),
    fn = function(x) {
      web_image(x,
                height = px(30))
    }
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0("Team Stats: ", 'FG%: ', opp_team_FG_per, '%  | 3pt%: ', opp_team_3_per, '%  | FT%: ', opp_team_FT_per)) %>% 
  gtsave(paste0("Opponent_Postgame_Basic_Stats.png"), path = "/Users/lukespring/Desktop/Postgame Tables")

#UMass Shooting Stats

UMass_vs_Opponent_Shooting <- player_stats

UMass_vs_Opponent_Shooting <- UMass_vs_Opponent_Shooting %>% filter(Team == "Massachusetts")

team_FG_per <- round(sum(UMass_vs_Opponent_Shooting$FGM)/sum(UMass_vs_Opponent_Shooting$FGA)*100, 1)
team_3_per <- round(sum(UMass_vs_Opponent_Shooting$TPM)/sum(UMass_vs_Opponent_Shooting$TPA)*100, 1)
team_FT_per <- round(sum(UMass_vs_Opponent_Shooting$FTM)/sum(UMass_vs_Opponent_Shooting$FTA)*100, 1)

UMass_vs_Opponent_Shooting$Rim <- paste0(UMass_vs_Opponent_Shooting$RIMM, "/", UMass_vs_Opponent_Shooting$RIMA)
UMass_vs_Opponent_Shooting$Mid <- paste0(UMass_vs_Opponent_Shooting$MIDM, "/", UMass_vs_Opponent_Shooting$MIDA)
UMass_vs_Opponent_Shooting$`3FGs` <- paste0(UMass_vs_Opponent_Shooting$TPM, "/", UMass_vs_Opponent_Shooting$TPA)
UMass_vs_Opponent_Shooting$FT <- paste0(UMass_vs_Opponent_Shooting$FTM, "/", UMass_vs_Opponent_Shooting$FTA)
UMass_vs_Opponent_Shooting$`Rim%` <- UMass_vs_Opponent_Shooting$RIM.*100
UMass_vs_Opponent_Shooting$`Mid%` <- UMass_vs_Opponent_Shooting$MID.*100
UMass_vs_Opponent_Shooting$`3FG%` <- UMass_vs_Opponent_Shooting$TP.*100
UMass_vs_Opponent_Shooting$`FT%` <- UMass_vs_Opponent_Shooting$FT.*100
UMass_vs_Opponent_Shooting$MINS <- round(UMass_vs_Opponent_Shooting$MINS, 1)

UMass_vs_Opponent_Shooting <- select(UMass_vs_Opponent_Shooting, Player, MINS, eFG., TS., Rim, `Rim%`, Mid, `Mid%`, `3FGs`, `3FG%`,FT, `FT%`)

UMass_vs_Opponent_Shooting$Player <- gsub("\\.", " ", UMass_vs_Opponent_Shooting$Player)

UMass_vs_Opponent_Shooting$Player <- str_to_title(UMass_vs_Opponent_Shooting$Player)

UMass_vs_Opponent_Shooting <- arrange(UMass_vs_Opponent_Shooting, Player)

UMass_vs_Opponent_Shooting[1,1] <- "Daniel Hankins-Sanford"

UMass_vs_Opponent_Shooting <- merge(UMass_vs_Opponent_Shooting, player_images_current, by = "Player")

UMass_vs_Opponent_Shooting <- select(UMass_vs_Opponent_Shooting, player_image, Player, MINS, eFG., TS., Rim, `Rim%`, Mid, `Mid%`, `3FGs`, `3FG%`,FT, `FT%`)

UMass_vs_Opponent_Shooting <- arrange(UMass_vs_Opponent_Shooting, desc(eFG.))

UMass_vs_Opponent_Shooting %>% 
  gt() %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Shooting Stats- `Sorted by EFG%`")) %>% 
  data_color(
    columns = c(eFG., TS., `3FG%`),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "",
             eFG. = "EFG%",
             TS. = "TS%") %>% 
  text_transform(
    locations = cells_body(columns = player_image),
    fn = function(x) {
      web_image(x,
                height = px(30))
    }
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0("Team Stats: ", 'FG%: ', team_FG_per, '%  | 3pt%: ', team_3_per, '%  | FT%: ', team_FT_per)) %>% 
  gtsave(paste0("UMass_Postgame_Shooting_Stats.png"), path = "/Users/lukespring/Desktop/Postgame Tables")

#Opponent Shooting Stats

Opponent_Shooting <- player_stats

Opponent_Shooting <- Opponent_Shooting %>% filter(Team == Opponent)

Opponent_Shooting$Rim <- paste0(Opponent_Shooting$RIMM, "/", Opponent_Shooting$RIMA)
Opponent_Shooting$Mid <- paste0(Opponent_Shooting$MIDM, "/", Opponent_Shooting$MIDA)
Opponent_Shooting$`3FGs` <- paste0(Opponent_Shooting$TPM, "/", Opponent_Shooting$TPA)
Opponent_Shooting$FT <- paste0(Opponent_Shooting$FTM, "/", Opponent_Shooting$FTA)
Opponent_Shooting$`Rim%` <- Opponent_Shooting$RIM.*100
Opponent_Shooting$`Mid%` <- Opponent_Shooting$MID.*100
Opponent_Shooting$`3FG%` <- Opponent_Shooting$TP.*100
Opponent_Shooting$`FT%` <- Opponent_Shooting$FT.*100
Opponent_Shooting$MINS <- round(Opponent_Shooting$MINS, 1)

Opponent_Shooting <- select(Opponent_Shooting, Player, MINS, eFG., TS., Rim, `Rim%`, Mid, `Mid%`, `3FGs`, `3FG%`, FT, `FT%`)

Opponent_Shooting$Player <- gsub("\\.", " ", Opponent_Shooting$Player)

Opponent_Shooting$Player <- str_to_title(Opponent_Shooting$Player)

Opponent_Shooting <- arrange(Opponent_Shooting, Player)

Opponent_Shooting <- merge(Opponent_Shooting, opponent_roster, by = "Player")

Opponent_Shooting <- select(Opponent_Shooting, player_image, Player, MINS, eFG., TS., Rim, `Rim%`, Mid, `Mid%`, `3FGs`, `3FG%`, FT, `FT%`)

Opponent_Shooting <- arrange(Opponent_Shooting, desc(eFG.))

Opponent_Shooting %>% 
  gt() %>% 
  tab_header(title = md("**Towson**"), subtitle = md("Shooting Stats- `Sorted by EFG%`")) %>% 
  data_color(
    columns = c(eFG., TS., `3FG%`),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "") %>% 
  text_transform(
    locations = cells_body(columns = player_image),
    fn = function(x) {
      web_image(x,
                height = px(30))
    }
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0("Team Stats: ", 'FG%: ', opp_team_FG_per, '%  | 3pt%: ', opp_team_3_per, '%  | FT%: ', opp_team_FT_per)) %>% 
  gtsave(paste0("Opponent_Postgame_Shooting_Stats.png"), path = "/Users/lukespring/Desktop/Postgame Tables")

#Lineup Data

lineup_data <- get_player_lineups(Lineup_Data = get_lineups(play_by_play_data = get_play_by_play(ncaa_game_id)))
lineup_data$P1 <- sub(".*\\.", "", lineup_data$P1)
lineup_data$P2 <- sub(".*\\.", "", lineup_data$P2)
lineup_data$P3 <- sub(".*\\.", "", lineup_data$P3)
lineup_data$P4 <- sub(".*\\.", "", lineup_data$P4)
lineup_data$P5 <- sub(".*\\.", "", lineup_data$P5)

UMass_lineup_data <- lineup_data %>% filter(Team == "Massachusetts")
Opponent_lineup_data <- lineup_data %>% filter(Team == "Towson")

UMass_lineup_data$P1 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", UMass_lineup_data$P1)
UMass_lineup_data$P2 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", UMass_lineup_data$P2)
UMass_lineup_data$P3 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", UMass_lineup_data$P3)
UMass_lineup_data$P4 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", UMass_lineup_data$P4)
UMass_lineup_data$P5 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", UMass_lineup_data$P5)

UMass_lineup_data <- arrange(UMass_lineup_data, desc(Mins))
Opponent_lineup_data <- arrange(Opponent_lineup_data, desc(Mins))

UMass_lineup_data <- top_n(UMass_lineup_data,12,Mins)
Opponent_lineup_data <- top_n(Opponent_lineup_data,12,Mins)

UMass_lineup_data$Lineups <- paste0(UMass_lineup_data$P1, "-", UMass_lineup_data$P2, "-", UMass_lineup_data$P3, "-", UMass_lineup_data$P4, "-", UMass_lineup_data$P5)
Opponent_lineup_data$Lineups <- paste0(Opponent_lineup_data$P1, "-", Opponent_lineup_data$P2, "-", Opponent_lineup_data$P3, "-", Opponent_lineup_data$P4, "-", Opponent_lineup_data$P5)

UMass_lineup_data$RIMrate <- UMass_lineup_data$RIMrate*100
UMass_lineup_data$MIDrate <- UMass_lineup_data$MIDrate*100
UMass_lineup_data$TPrate <- UMass_lineup_data$TPrate*100
UMass_lineup_data$FG. <- UMass_lineup_data$FG.*100
UMass_lineup_data$dFG. <- UMass_lineup_data$dFG.*100

Opponent_lineup_data$RIMrate <- Opponent_lineup_data$RIMrate*100
Opponent_lineup_data$MIDrate <- Opponent_lineup_data$MIDrate*100
Opponent_lineup_data$TPrate <- Opponent_lineup_data$TPrate*100
Opponent_lineup_data$FG. <- Opponent_lineup_data$FG.*100
Opponent_lineup_data$dFG. <- Opponent_lineup_data$dFG.*100

UMass_lineup_data1 <- select(UMass_lineup_data, Lineups, Mins, ORTG, DRTG, NETRTG, PTS, dPTS, AST, ORB, DRB)
UMass_lineup_data2 <- select(UMass_lineup_data, Lineups, Mins, FG., dFG., RIMrate, MIDrate, TPrate)
Opponent_lineup_data1 <- select(Opponent_lineup_data, Lineups, Mins, ORTG, DRTG, NETRTG, PTS, dPTS, AST, ORB, DRB)
Opponent_lineup_data2 <- select(Opponent_lineup_data, Lineups, Mins, FG., dFG., RIMrate, MIDrate, TPrate)

UMass_lineup_data1 %>% 
  gt() %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Lineup Data Basic- `Sorted by Mins`")) %>%
  data_color(
    columns = c(NETRTG, PTS, Mins),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(dPTS = "PTS Against") %>% 
  tab_style(
       style = list(
          cell_text(weight = "bold")
               ),
               locations = cells_body(
                 columns = Lineups
               )
             ) %>%
  cols_align(align = "center")

Opponent_lineup_data1 %>% 
  gt() %>% 
  tab_header(title = md("**Opponent**"), subtitle = md("Lineup Data Basic- `Sorted by Mins`")) %>%
  data_color(
    columns = c(NETRTG, PTS, Mins),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(dPTS = "PTS Against") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Lineups
    )
  ) %>%
  cols_align(align = "center")

UMass_lineup_data2 %>% 
  gt() %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Lineup Data Shooting- `Sorted by Mins`")) %>%
  data_color(
    columns = c(FG., dFG., TPrate),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(FG. = "FG%",
             dFG. = "Def FG%",
             RIMrate = "Rim Rate",
             MIDrate = "Mid Rate",
             TPrate = "Three Rate") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Lineups
    )
  ) %>%
  cols_align(align = "center")

Opponent_lineup_data2 %>% 
  gt() %>% 
  tab_header(title = md("**Opponent**"), subtitle = md("Lineup Data Shooting- `Sorted by Mins`")) %>%
  data_color(
    columns = c(FG., dFG., TPrate),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(FG. = "FG%",
             dFG. = "Def FG%",
             RIMrate = "Rim Rate",
             MIDrate = "Mid Rate",
             TPrate = "Three Rate") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = Lineups
    )
  ) %>%
  cols_align(align = "center")

print(game_flow_chart <- game_flow(espn_game_id, "#FFBB00", "maroon"))