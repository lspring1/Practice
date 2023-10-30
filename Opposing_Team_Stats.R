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
library(ggplot2)
library("toRvik")
library(gt)
library(dplyr)

if (!requireNamespace('devtools', quietly = TRUE)){
  install.packages('devtools')
}

#Basic Stats Table

#force(ids) %>% as_tibble() %>% print(n=363)

file_name <- paste0("UMass_Opponent_Basic_Stats.png")

opposing_roster <- get_roster("Quinnipiac", season = "2022-23")
opposing_player_pics <- arrange(opposing_roster, name)
#opposing_player_pics <- opposing_player_pics[-15,]
opposing_player_pics = select(opposing_player_pics, name, player_image, -player_id, -number, -position, -height, -weight, -class, -hometown)

bart_box_stats <- bart_player_season(
  team = "Quinnipiac",
  stat = 'box'
)

colnames(bart_box_stats)[1] = "name"

bart_box_stats$bpg <- format(round(bart_box_stats$bpg, 2), nsmall = 2)

bart_box_stats$fg_pct <- as.numeric(bart_box_stats$fg_pct) * 100

bart_box_stats$fg_pct <- format(round(bart_box_stats$fg_pct, 1), nsmall = 1)

#bart_box_stats <- arrange(bart_box_stats, player)

opposing_team_basic_stats <- merge(bart_box_stats, opposing_player_pics, by = "name")

#opposing_team_basic <- cbind(opposing_player_pics, bart_box_stats)

opposing_team_basic_stats <- arrange(opposing_team_basic_stats, desc(mpg))

options(digits = 2)
opposing_team_basic_stats %>% 
  gt() %>% 
  tab_header(title = md("**Quinnipiac**"), subtitle = md("Basic Stats- `Sorted by Min.`")) %>% 
  cols_hide(columns = c(num, year, id, inches, team, conf, ast_to, oreb, dreb)) %>% 
  data_color(
    columns = c(ppg, apg, rpg),
    colors = scales::col_numeric(
      palette = c("#ECFFDC", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(player_image = "",
             name = "Player", 
             pos = "Pos",
             exp = "Exp.",
             hgt = "Height",
             g = "GP",
             mpg = "Min", 
             ppg = "PPG", 
             fg_pct = "FG%",
             rpg = "RPG",
             apg = "APG",
             tov = "TOV",
             spg = "Stl",
             bpg = "Blk") %>% 
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
      columns = name
    )
  ) %>% 
  cols_move_to_start(player_image) %>%
  fmt_number(columns = c(spg),
             decimal = 2
  ) %>% 
  gtsave(file_name, path = "/Users/lukespring/Desktop/UMass Basketball Projects")

#Advanced Stats Table

file_name <- paste0("UMass_Opponent_Advanced_Stats.png")

opposing_roster <- get_roster("Quinnipiac")
opposing_player_pics <- arrange(opposing_roster, name)
#opposing_player_pics <- opposing_player_pics[-15,]
opposing_player_pics = select(opposing_player_pics, -player_id, -number, -position, -height, -weight, -class, -hometown)

opponent_advanced_stats <- bart_player_season(
  team = "Quinnipiac",
  stat = 'advanced'
)

colnames(opponent_advanced_stats)[1] = "name"

opponent_advanced_stats <- merge(opponent_advanced_stats, opposing_player_pics, by = "name")

#advanced_stats <- arrange(advanced_stats, player)

#opposing_team_advanced <- cbind(opposing_player_pics, advanced_stats)

opponent_advanced_stats <- arrange(opponent_advanced_stats, desc(min))

options(digits = 2)
opponent_advanced_stats %>% 
  gt() %>% 
  tab_header(title = md("**Quinnipiac**"), subtitle = md("Advanced Stats- `Sorted by Min.`")) %>% 
  cols_hide(columns = c(exp, team, conf, g, porpag, dporpag, oreb, dreb, ast, to, blk, stl, ftr, pfr, rec, pick, year, id, min)) %>% 
  data_color(
    columns = c(ortg, bpm, drtg),
    colors = scales::col_numeric(
      palette = c("#ECFFDC", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(player_image = "",
             name = "Player", 
             pos = "Pos", 
             min = "Min", 
             ortg = "ORtg", 
             adj_oe = "Adj. OE", 
             adj_de = "Adj. DE", 
             drtg = "DRtg", 
             stops = "Stops",
             obpm = "Off. BPM",
             dbpm = "Def. BPM",
             bpm = "BPM") %>% 
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
      columns = name
    )
  ) %>% 
  cols_move_to_start(player_image) %>% 
  gtsave(file_name, path = "/Users/lukespring/Desktop/UMass Basketball Projects")

#Shooting Stats Table

file_name <- paste0("UMass_Opponent_Shooting_Stats.png")

opposing_roster <- get_roster("Quinnipiac")
opposing_player_pics <- arrange(opposing_roster, name)
#opposing_player_pics <- opposing_player_pics[-15,]
opposing_player_pics = select(opposing_player_pics, -player_id, -number, -position, -height, -weight, -class, -hometown)

opponent_shooting_stats <- bart_player_season(
  team = "Quinnipiac",
  stat = 'shooting'
)

colnames(opponent_shooting_stats)[1] = "name"

opponent_shooting_stats <- merge(opponent_shooting_stats, opposing_player_pics, by = "name")

opponent_shooting_stats$FT <- paste0(opponent_shooting_stats$ftm, "/", opponent_shooting_stats$fta)

opponent_shooting_stats$Rim <- paste0(opponent_shooting_stats$rim_m, "/", opponent_shooting_stats$rim_a)

opponent_shooting_stats$"3Point" <- paste0(opponent_shooting_stats$three_m, "/", opponent_shooting_stats$three_a)

opponent_shooting_stats$Mid <- paste0(opponent_shooting_stats$mid_m, "/", opponent_shooting_stats$mid_a)

opponent_shooting_stats$rim_pct <- as.numeric(opponent_shooting_stats$rim_pct) * 100

opponent_shooting_stats$mid_pct <- as.numeric(opponent_shooting_stats$mid_pct) * 100

opponent_shooting_stats$ft_pct <- as.numeric(opponent_shooting_stats$ft_pct) * 100

opponent_shooting_stats$three_pct <- as.numeric(opponent_shooting_stats$three_pct) * 100

#advanced_stats <- arrange(advanced_stats, player)

#opposing_team_advanced <- cbind(opposing_player_pics, advanced_stats)

opponent_shooting_stats <- arrange(opponent_shooting_stats, desc(mpg))

options(digits = 2)
opponent_shooting_stats %>% 
  gt() %>% 
  tab_header(title = md("**Quinnipiac**"), subtitle = md("Advanced Stats- `Sorted by Min.`")) %>% 
  cols_hide(columns = c(exp, team, conf, g, mpg, p_per, ortg, dunk_m, dunk_a, dunk_pct, two_m, two_a, two_pct, year, id, ftm,
                        fta, rim_m, rim_a, three_m, three_a, mid_m, mid_a, usg, ppg)) %>% 
  data_color(
    columns = c(rim_pct, mid_pct, three_pct, ft_pct),
    colors = scales::col_numeric(
      palette = c("#ECFFDC", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(player_image = "",
             name = "Player", 
             ts = "TS%", 
             efg = "eFG%", 
             rim_pct = "Rim %", 
             mid_pct = "Mid %", 
             three_pct = "3Pt %",
             ft_pct = "FT %") %>% 
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
      columns = name
    )
  ) %>%
  fmt_number(columns = c(efg, ts, rim_pct, mid_pct, three_pct, ft_pct),
             decimal = 2
             ) %>% 
  cols_move_to_start(player_image) %>% 
  cols_move(Rim, ts) %>% 
  cols_move(rim_pct, Rim) %>%
  cols_move(Mid, rim_pct) %>%
  cols_move(mid_pct, Mid) %>%
  cols_move("3Point", mid_pct) %>%
  cols_move(three_pct, "3Point") %>%
  cols_move(FT, three_pct) %>%
  cols_move(ft_pct, FT) %>%
  cols_align(align = "center") %>% 
  gtsave(file_name, path = "/Users/lukespring/Desktop/UMass Basketball Projects")