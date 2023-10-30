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

force(ids) %>% as_tibble() %>% print(n=363)

file_name <- paste0("UMass_Opponent_Advanced_Stats.png")

opposing_roster <- get_roster("Albany")
opposing_player_pics <- arrange(opposing_roster, name)
opposing_player_pics <- opposing_player_pics[-15,]
opposing_player_pics = select(opposing_player_pics, -player_id, -number, -name, -position, -height, -weight, -class, -hometown)

advanced_stats <- bart_player_season(
  team = "Albany",
  stat = 'advanced'
)
advanced_stats <- arrange(advanced_stats, player)

opposing_team_advanced <- cbind(opposing_player_pics, advanced_stats)

opposing_team_advanced_stats <- arrange(opposing_team_advanced, desc(min))

options(digits = 2)
opposing_team_advanced_stats %>% 
  gt() %>% 
  tab_header(title = md("**ALbany**"), subtitle = md("Advanced Stats- `Sorted by Min.`")) %>% 
  cols_hide(columns = c(exp, team, conf, g, porpag, dporpag, oreb, dreb, ast, to, blk, stl, ftr, pfr, rec, pick, year, id, min)) %>% 
  data_color(
    columns = c(ortg, bpm, drtg),
    colors = scales::col_numeric(
      palette = c("#ECFFDC", "lightgreen", "darkgreen"),
      domain = NULL
    )
  ) %>% 
  cols_label(player_image = "",
             player = "Player", 
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
      columns = player
    )
  ) %>% 
  gtsave(file_name, path = "/Users/lukespring/Desktop/UMass Basketball Projects")