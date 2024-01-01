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
cbbdata::cbd_login(username = 'lspring', password = 'Bonnetshores73')
library(cbbdata)
library(glue)
library(here)
library(magick)
library(matrixStats)

UMass_player_stats <- cbd_torvik_player_season(year = 2024, team = 'Massachusetts')

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


player <- c("Daniel Hankins-Sanford", "Jaylen Curry", "Rahsool Diggins", "Robert Davis Jr.", "Keon Thompson", "Marqui Worthy", "Jayden Ndjigue",
            "Tarique Foster", "Ryan Marcus", "Jackson Cronin", "Josh Cohen", "Matt Cross", "Mathok Majok", "Rollie Castineyra", "Totals")

player_images_current <- data.frame(player_image, player)

UMass_player_stats <- merge(UMass_player_stats, player_images_current, by = "player")

umass_torvik_2024 <- cbd_torvik_ratings_archive(year = 2024, team = 'Massachusetts')

#Full Season Lineups...............................................................................................................................................
Umass_postgame_schedule <- get_team_schedule(team.name = "Massachusetts", season = "2023-24")
Umass_postgame_schedule <- Umass_postgame_schedule[!is.na(Umass_postgame_schedule$Game_ID),]
umass_postgame_game_ids <- Umass_postgame_schedule$Game_ID

full_lineup_data <- get_player_lineups(Lineup_Data = get_lineups(play_by_play_data = get_play_by_play(umass_postgame_game_ids)))

full_lineup_data$P1 <- sub(".*\\.", "", full_lineup_data$P1)
full_lineup_data$P2 <- sub(".*\\.", "", full_lineup_data$P2)
full_lineup_data$P3 <- sub(".*\\.", "", full_lineup_data$P3)
full_lineup_data$P4 <- sub(".*\\.", "", full_lineup_data$P4)
full_lineup_data$P5 <- sub(".*\\.", "", full_lineup_data$P5)

full_lineup_data <- full_lineup_data %>% filter(Team == "Massachusetts")

full_lineup_data$P1 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", full_lineup_data$P1)
full_lineup_data$P2 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", full_lineup_data$P2)
full_lineup_data$P3 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", full_lineup_data$P3)
full_lineup_data$P4 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", full_lineup_data$P4)
full_lineup_data$P5 <- gsub("HANKINSSANFORD", "HANKINS-SANFORD", full_lineup_data$P5)

full_lineup_data <- arrange(full_lineup_data, desc(Mins))

full_lineup_data <- top_n(full_lineup_data,12,Mins)

full_lineup_data$Lineups <- paste0(full_lineup_data$P1, "-", full_lineup_data$P2, "-", full_lineup_data$P3, "-", full_lineup_data$P4, "-", full_lineup_data$P5)

full_lineup_data$RIMrate <- full_lineup_data$RIMrate*100
full_lineup_data$MIDrate <- full_lineup_data$MIDrate*100
full_lineup_data$TPrate <- full_lineup_data$TPrate*100
full_lineup_data$FG. <- full_lineup_data$FG.*100
full_lineup_data$dFG. <- full_lineup_data$dFG.*100

lineup_data <- select(full_lineup_data, Lineups, Mins, ORTG, DRTG, NETRTG, PTS, dPTS, AST, ORB, DRB)
Shooting_lineup_data <- select(full_lineup_data, Lineups, Mins, FG., dFG., RIMrate, MIDrate, TPrate)

lineup_data %>% 
  gt() %>% 
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Lineup Data Basic- `Sorted by Mins`")) %>%
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

Shooting_lineup_data %>% 
  gt() %>% 
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Lineup Data Shooting- `Sorted by Mins`")) %>%
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
  cols_align(align = "center") %>% 
  gtsave(paste0("UMass_Shooting_Lineup.png"), path = "/Users/lukespring/Desktop/Non-Conference Report Tables")

#Basic Stats....................................................................................................................................................

UMass_player_basic_stats <- select(UMass_player_stats, player_image, player, mpg, ppg, rpg, apg, tov, spg, bpg, fg_pct, three_pct, ft_pct)

UMass_player_basic_stats$fg_pct <- round(UMass_player_basic_stats$fg_pct*100, 1)
UMass_player_basic_stats$ft_pct <- round(UMass_player_basic_stats$ft_pct*100, 1)
UMass_player_basic_stats$three_pct <- round(UMass_player_basic_stats$three_pct*100, 1)
UMass_player_basic_stats$ppg <- round(UMass_player_basic_stats$ppg, 1)
UMass_player_basic_stats$mpg <- round(UMass_player_basic_stats$mpg, 1)
UMass_player_basic_stats$rpg <- round(UMass_player_basic_stats$rpg, 1)
UMass_player_basic_stats$apg <- round(UMass_player_basic_stats$apg, 1)
UMass_player_basic_stats$tov <- round(UMass_player_basic_stats$tov, 1)
UMass_player_basic_stats$spg <- round(UMass_player_basic_stats$spg, 1)
UMass_player_basic_stats$bpg <- round(UMass_player_basic_stats$bpg, 1)

UMass_player_basic_stats[is.na(UMass_player_basic_stats)] <- 0

UMass_player_basic_stats <- arrange(UMass_player_basic_stats, desc(ppg))

UMass_player_basic_stats %>%
  gt() %>%
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Basic Stats- `Sorted by Pts.`")) %>%
  data_color(
    columns = c(ppg, rpg, ft_pct, three_pct),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "",
             player = "Player",
             ppg = "PPG",
             mpg = "MPG",
             rpg = "RPG",
             apg = "APG",
             tov = "TOV",
             spg = "SPG",
             bpg = "BPG",
             fg_pct = "FG%",
             three_pct = "3P%",
             ft_pct = "FT%"
             ) %>%
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
      columns = player
    )
  ) %>%
  cols_align(align = "center") %>% 
  gtsave(paste0("UMass_Basic_Stats.png"), path = "/Users/lukespring/Desktop/Non-Conference Report Tables")

#Shooting Tables
UMass_player_shooting_stats <- select(UMass_player_stats, player_image, player, efg, ts, fg_pct, three_pct, ft_pct, rim_a, mid_a, three_a, fga)

UMass_player_shooting_stats$"Rim Att. %" <- UMass_player_shooting_stats$rim_a/(UMass_player_shooting_stats$three_a + UMass_player_shooting_stats$rim_a + UMass_player_shooting_stats$mid_a)
UMass_player_shooting_stats$"Mid Att. %" <- UMass_player_shooting_stats$mid_a/(UMass_player_shooting_stats$three_a + UMass_player_shooting_stats$rim_a + UMass_player_shooting_stats$mid_a)
UMass_player_shooting_stats$"Three Att. %" <- UMass_player_shooting_stats$three_a/(UMass_player_shooting_stats$three_a + UMass_player_shooting_stats$rim_a + UMass_player_shooting_stats$mid_a)

UMass_player_shooting_stats_final <- select(UMass_player_shooting_stats, player_image, player, efg, ts, fg_pct, three_pct, ft_pct, `Rim Att. %`,`Mid Att. %`, `Three Att. %`, fga)

UMass_player_shooting_stats_final$fg_pct <- round(UMass_player_shooting_stats_final$fg_pct*100, 1)
UMass_player_shooting_stats_final$ft_pct <- round(UMass_player_shooting_stats_final$ft_pct*100, 1)
UMass_player_shooting_stats_final$three_pct <- round(UMass_player_shooting_stats_final$three_pct*100, 1)
UMass_player_shooting_stats_final$efg <- round(UMass_player_shooting_stats_final$efg, 1)
UMass_player_shooting_stats_final$ts <- round(UMass_player_shooting_stats_final$ts, 1)
UMass_player_shooting_stats_final$`Rim Att. %` <- round(UMass_player_shooting_stats_final$`Rim Att. %`*100, 1)
UMass_player_shooting_stats_final$`Mid Att. %` <- round(UMass_player_shooting_stats_final$`Mid Att. %`*100, 1)
UMass_player_shooting_stats_final$`Three Att. %` <- round(UMass_player_shooting_stats_final$`Three Att. %`*100, 1)

UMass_player_shooting_stats_final[is.na(UMass_player_shooting_stats_final)] <- 0

UMass_player_shooting_stats_final <- arrange(UMass_player_shooting_stats_final, desc(fga))

UMass_player_shooting_stats_final %>%
  gt() %>%
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Shooting Stats- `Sorted by FGA`")) %>%
  data_color(
    columns = c(efg, ts, `Three Att. %`),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "",
             player = "Player",
             efg = "EFG %",
             ts = "TS %",
             fg_pct = "FG%",
             three_pct = "3P%",
             ft_pct = "FT%"
  ) %>%
  cols_hide(fga) %>% 
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
      columns = player
    )
  ) %>%
  cols_align(align = "center") %>% 
  gtsave(paste0("UMass_Shooting_Stats.png"), path = "/Users/lukespring/Desktop/Non-Conference Report Tables")

#Advanced Stats
UMass_player_advanced_stats <- select(UMass_player_stats, player_image, player, adj_oe, adj_de, usg, bpm, oreb_rate, ast_to)

UMass_player_advanced_stats$adj_oe <- round(UMass_player_advanced_stats$adj_oe, 1)
UMass_player_advanced_stats$adj_de <- round(UMass_player_advanced_stats$adj_de, 1)
UMass_player_advanced_stats$usg <- round(UMass_player_advanced_stats$usg, 1)
UMass_player_advanced_stats$bpm <- round(UMass_player_advanced_stats$bpm, 2)
UMass_player_advanced_stats$ast_to <- round(UMass_player_advanced_stats$ast_to, 2)

UMass_player_advanced_stats[is.na(UMass_player_advanced_stats)] <- 0

UMass_player_advanced_stats <- arrange(UMass_player_advanced_stats, desc(adj_oe))

UMass_player_advanced_stats %>%
  gt() %>%
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Advanced Stats- `Sorted by Adj. OE`")) %>%
  data_color(
    columns = c(adj_oe, oreb_rate),
    colors = scales::col_numeric(
      palette = c("white", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "",
             player = "Player",
             adj_oe = "Adj. OE",
             adj_de = "Adj. DE",
             usg = "Usage",
             bpm = "Box +/-",
             oreb_rate = "OREB Rate",
             ast_to = "Ast/TO"
  ) %>%
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
      columns = player
    )
  ) %>%
  cols_align(align = "center") %>% 
  gtsave(paste0("UMass_Advanced_Stats.png"), path = "/Users/lukespring/Desktop/Non-Conference Report Tables")

#Team Stats
four_factors <- cbd_torvik_team_split(year = 2024, team = "Massachusetts", split = "game_result")

four_factors_win_loss <- select(four_factors, result, fg_pct, tp_pct, oreb, ast, to, opp_fg_pct, opp_tp_pct, tempo)

four_factors_win_loss$fg_pct <- round(four_factors_win_loss$fg_pct*100, 1)
four_factors_win_loss$tp_pct <- round(four_factors_win_loss$tp_pct*100, 1)
four_factors_win_loss$opp_fg_pct <- round(four_factors_win_loss$opp_fg_pct*100, 1)
four_factors_win_loss$opp_tp_pct <- round(four_factors_win_loss$opp_tp_pct*100, 1)
four_factors_win_loss$oreb <- round(four_factors_win_loss$oreb, 1)
four_factors_win_loss$ast <- round(four_factors_win_loss$ast, 1)
four_factors_win_loss$to <- round(four_factors_win_loss$to, 1)
four_factors_win_loss$tempo <- round(four_factors_win_loss$tempo, 1)

colnames(four_factors_win_loss) <- c("result","FG%","3P%", "OREB", "AST", "TOV", "Opp FG%", "Opp 3P%", "Tempo")

four_factors_win_loss <- t(four_factors_win_loss)

colnames(four_factors_win_loss) <- four_factors_win_loss[1, ]

four_factors_win_loss <- four_factors_win_loss[-1,]

four_factors_win_loss <- as.data.frame(four_factors_win_loss)

four_factors_win_loss <- four_factors_win_loss %>%
  rownames_to_column(var = "RowNames")

colnames(four_factors_win_loss) <- c("Stat", "W", "L")

four_factors_win_loss$W <- as.numeric(four_factors_win_loss$W)

four_factors_win_loss %>%
  gt() %>%
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Wins vs Losses")) %>%
  cols_align(align = "center") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = 
    cells_column_labels(columns=c("Stat", "W", "L"))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = "Stat")
  ) %>% 
  tab_options(
    column_labels.background.color = "white",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 25,
    heading.align = "center"
  ) %>% 
  gtsave(paste0("UMass_Wins_vs_Losses_Stats.png"), path = "/Users/lukespring/Desktop/Non-Conference Report Tables")

#Team Stats

full_team_stats <- cbd_torvik_team_factors(
  year = 2024,
  venue = "all",
  game_type = "all"
)

full_team_stats$adj_o_rank<-round(rank(-full_team_stats$adj_o), 0)
full_team_stats$adj_d_rank<-round(rank(full_team_stats$adj_d), 0)
full_team_stats$efg_rank<-round(rank(-full_team_stats$efg), 0)
full_team_stats$def_efg_rank<-round(rank(full_team_stats$def_efg), 0)
full_team_stats$three_pt_pct_rank<-round(rank(-full_team_stats$three_pt_pct), 0)
full_team_stats$def_three_pt_pct_rank<-round(rank(full_team_stats$def_three_pt_pct), 0)
full_team_stats$ft_pct_rank<-round(rank(-full_team_stats$ft_pct), 0)
full_team_stats$assist_rate_rank<-round(rank(-full_team_stats$assist_rate), 0)
full_team_stats$oreb_rate_rank<-round(rank(-full_team_stats$oreb_rate), 0)
full_team_stats$tov_rate_rank<-round(rank(full_team_stats$tov_rate), 0)

UMass_team_stats <- filter(full_team_stats, team == "Massachusetts")
UMass_team_stats <- select(UMass_team_stats, adj_o, adj_d, efg, def_efg, three_pt_pct, def_three_pt_pct, ft_pct, assist_rate, oreb_rate, tov_rate)

UMass_team_stats$adj_o <- round(UMass_team_stats$adj_o, 1)
UMass_team_stats$adj_d <- round(UMass_team_stats$adj_d, 1)
UMass_team_stats$efg <- round(UMass_team_stats$efg, 1)
UMass_team_stats$def_efg <- round(UMass_team_stats$def_efg, 1)
UMass_team_stats$three_pt_pct <- round(UMass_team_stats$three_pt_pct, 1)
UMass_team_stats$def_three_pt_pct <- round(UMass_team_stats$def_three_pt_pct, 1)
UMass_team_stats$ft_pct <- round(UMass_team_stats$ft_pct, 1)
UMass_team_stats$assist_rate <- round(UMass_team_stats$assist_rate, 1)
UMass_team_stats$oreb_rate <- round(UMass_team_stats$oreb_rate, 1)

colnames(UMass_team_stats) <- c("Adj O","Adj D","EFG %", "Def. EFG %", "3P%", "Def. 3P%", "FT %", "Ast. Rate", "OREB Rate", "TOV Rate")

UMass_team_stats <- t(UMass_team_stats)

ranks <- filter(full_team_stats, team == "Massachusetts")
ranks <- select(ranks, adj_o_rank, adj_d_rank, efg_rank, def_efg_rank, three_pt_pct_rank, def_three_pt_pct_rank, ft_pct_rank, assist_rate_rank, oreb_rate_rank, tov_rate_rank)
ranks <- t(ranks)

UMass_team_stats <- cbind(UMass_team_stats, ranks)

UMass_team_stats <- as.data.frame(UMass_team_stats)

UMass_team_stats <- UMass_team_stats %>%
  rownames_to_column(var = "RowNames")

colnames(UMass_team_stats) <- c("Stat", "Value", "NCAA D1 Rank")

UMass_team_stats %>%
  gt() %>%
  tab_header(title = html(web_image(
    url = "https://1000logos.net/wp-content/uploads/2022/02/Massachusetts-Minutemen-logo.png",
    height = px(100)
  )), subtitle = md("Team Ranks")) %>%
  cols_align(align = "center") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = 
      cells_column_labels(columns=c("Stat", "Value", "NCAA D1 Rank"))
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(columns = "Stat")
  ) %>% 
  tab_options(
    column_labels.background.color = "transparent",
    table.border.top.width = px(3),
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    table.border.bottom.width = px(3),
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "transparent",
    column_labels.border.bottom.width = px(3),
    column_labels.border.bottom.color = "black",
    data_row.padding = px(3),
    source_notes.font.size = 12,
    table.font.size = 25,
    heading.align = "center",
    table.background.color = "transparent"
  ) %>% 
  data_color(
    columns = "NCAA D1 Rank",
    fn = scales::col_numeric(
      palette = c("red", "yellow", "darkgreen"),
      domain = c(1, 362),
      reverse = TRUE
    )
  ) %>% 
  gtsave(paste0("UMass_Team_Stats.png"), path = "/Users/lukespring/Desktop/Non-Conference Report Tables")