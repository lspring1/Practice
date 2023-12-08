library(shiny)
library(shinythemes)
library(dplyr)
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
library(here)
library(stringr)

#data(ids)

team_name <- "UMass Lowell"
game_ids <- c(5562592, 5565213, 5569766, 5571170, 5574586, 5582947, 5581933, 5583192)

roster <- get_roster(team = team_name, "2023-24")

roster$Player <- roster$name

player_personnel_stats <- get_player_stats(play_by_play_data = get_play_by_play(game_ids), multi.games = T, simple = F)

player_personnel_stats <- filter(player_personnel_stats, Team == "UMass Lowell")

player_personnel_stats$Player <- gsub("\\.", " ", player_personnel_stats$Player)

player_personnel_stats$Player <- str_to_title(player_personnel_stats$Player)

player_personnel_stats <- player_personnel_stats %>%
  group_by(Player)

player_personnel_stats$FG <- paste0(player_personnel_stats$FGM, "/", player_personnel_stats$FGA)
player_personnel_stats$`3FGs` <- paste0(player_personnel_stats$TPM, "/", player_personnel_stats$TPA)
player_personnel_stats$FT <- paste0(player_personnel_stats$FTM, "/", player_personnel_stats$FTA)

player_personnel_stats$Mins <- round(player_personnel_stats$MINS/player_personnel_stats$GP, 1)
player_personnel_stats$PTS <- round(player_personnel_stats$PTS/player_personnel_stats$GP, 1)
player_personnel_stats$AST <- round(player_personnel_stats$AST/player_personnel_stats$GP, 1)
player_personnel_stats$ORB <- round(player_personnel_stats$ORB/player_personnel_stats$GP, 1)
player_personnel_stats$DRB <- round(player_personnel_stats$DRB/player_personnel_stats$GP, 1)
player_personnel_stats$TOV <- round(player_personnel_stats$TOV/player_personnel_stats$GP, 1)

player_personnel_stats$Mins <- round(player_personnel_stats$Mins, 1)

player_personnel_stats$`FT%` <- round(player_personnel_stats$FT.*100, 1)
player_personnel_stats$`3P%` <- round(player_personnel_stats$TP.*100, 1)
player_personnel_stats$`FG%` <- round(player_personnel_stats$FG.*100, 1)

player_personnel_stats <- merge(player_personnel_stats, roster, by = "Player")

player_personnel_stats$Number <- player_personnel_stats$number

player_personnel_stats$Position <- player_personnel_stats$position

player_personnel_stats <- select(player_personnel_stats, c(Number, Player, Position, Mins, FG, `FG%`, `3FGs`, `3P%`, FT, `FT%`, TOV, ORB, DRB, AST, PTS))

player_personnel_stats <- arrange(player_personnel_stats, Number)

player_personnel_stats %>% 
  kbl() %>%  
  kable_classic_2(full_width = F) %>% 
  kable_styling(c("striped", "bordered"), position = "center", font_size = 30) %>% 
  add_header_above(c(" " = 4, "Shooting" = 6, " Basic Per Game" = 5))