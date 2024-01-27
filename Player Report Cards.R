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

#Player Report Cards
player_var <- "Jaedon LeDee"

national_player_stats <- cbd_torvik_player_season(year = 2024)

national_player_stats$Position <- ""

national_player_stats <- national_player_stats %>%
  mutate(
    `Position` = case_when(
      (pos %in% c("Scoring PG", "Pure PG", "Combo G")) ~ "Guards",
      TRUE ~ `Position`
    )
  )

national_player_stats <- national_player_stats %>%
  mutate(
    `Position` = case_when(
      (pos %in% c("Wing G", "Stretch 4")) ~ "Wings",
      TRUE ~ `Position`
    )
  )

national_player_stats <- national_player_stats %>%
  mutate(
    `Position` = case_when(
      (pos %in% c("Wing F", "PF/C", "C")) ~ "Bigs",
      TRUE ~ `Position`
    )
  )

national_player_stats$`Conference Level` <- "Low"

national_player_stats <- national_player_stats %>%
  mutate(
    `Conference Level` = case_when(
      (conf %in% c("ACC", "SEC", "BE", "B12", "B10", "P12")) ~ "High Major",
      TRUE ~ `Conference Level`
    )
  )

national_player_stats <- national_player_stats %>%
  mutate(
    `Conference Level` = case_when(
      (conf %in% c("MWC", "A10", "Amer", "MVC", "WAC", "CUSA", "WCC", "Ivy")) ~ "Mid Major",
      TRUE ~ `Conference Level`
    )
  )

player_data <- national_player_stats %>% filter(player == player_var)

team <- player_data$team

team_df <- cbd_teams()

selected_team_df <- team_df %>% filter(common_team == team)

player_position <- player_data$Position
conf_level <- player_data$`Conference Level`
color <- selected_team_df$color
logo <- selected_team_df$logo

position_filter <- national_player_stats %>% filter(Position == player_position, mpg  >= 8)

position_filter <- position_filter %>%
  mutate("ORTG Rank" = round((rank(ortg)/length(ortg))*100), 0)

position_filter <- position_filter %>%
  mutate("FG% Rank" = round((rank(fg_pct)/length(fg_pct))*100), 0)

position_filter <- position_filter %>%
  mutate("EFG% Rank" = round((rank(efg)/length(efg))*100), 0)

position_filter <- position_filter %>%
  mutate("Usage Rank" = round((rank(usg)/length(usg))*100), 0)

position_filter <- position_filter %>%
  mutate("Ast/TO Rank" = round((rank(ast_to)/length(ast_to))*100), 0)

position_filter <- position_filter %>%
  mutate("SPG Rank" = round((rank(spg)/length(spg))*100), 0)

position_filter <- position_filter %>%
  mutate("Blk Rank" = round((rank(bpg)/length(bpg))*100), 0)

position_filter <- position_filter %>%
  mutate("DRTG Rank" = 100 - round((rank(drtg)/length(drtg))*100), 0)

position_filter <- position_filter %>%
  mutate("Free Throw Rank" = round((rank(ftr)/length(ftr))*100), 0)

position_filter <- position_filter %>%
  mutate("3P% Rank" = round((rank(three_pct)/length(three_pct))*100), 0)

position_filter <- position_filter %>%
  mutate("True Shooting Rank" = round((rank(ts)/length(ts))*100), 0)

position_filter <- position_filter %>%
  mutate("FT% Rank" = round((rank(ft_pct)/length(ft_pct))*100), 0)

player_position_data <- position_filter %>% filter(player == player_var)

#............................................................................................................................................................

conference_level_filter <- position_filter %>% filter(`Conference Level` == conf_level)

conference_level_filter <- conference_level_filter %>%
  mutate("ORTG Rank" = round((rank(ortg)/length(ortg))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("FG% Rank" = round((rank(fg_pct)/length(fg_pct))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("EFG% Rank" = round((rank(efg)/length(efg))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("Usage Rank" = round((rank(usg)/length(usg))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("Ast/TO Rank" = round((rank(ast_to)/length(ast_to))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("SPG Rank" = round((rank(spg)/length(spg))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("Blk Rank" = round((rank(bpg)/length(bpg))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("DRTG Rank" = 100 - round((rank(drtg)/length(drtg))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("Free Throw Rank" = round((rank(ftr)/length(ftr))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("3P% Rank" = round((rank(three_pct)/length(three_pct))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("True Shooting Rank" = round((rank(ts)/length(ts))*100), 0)

conference_level_filter <- conference_level_filter %>%
  mutate("FT% Rank" = round((rank(ft_pct)/length(ft_pct))*100), 0)

conference_level_player_filter <- conference_level_filter %>% filter(player == player_var)

final_table_df <- data.frame(Stat = c("ORTG", "Usage", "Ast/TO", "DRTG", "SPG", "BPG", "EFG%", "FG%", "3P%", "FT%", "FT Rate", "True Shooting %"),
                             Value = c(round(player_position_data$ortg, 1), round(player_position_data$usg, 1), round(player_position_data$ast_to, 1), round(player_position_data$drtg, 1),
                                       round(player_position_data$spg, 1), round(player_position_data$bpg, 1), round(player_position_data$efg, 1), 100*round(player_position_data$fg_pct, 3),
                                       100*round(player_position_data$three_pct, 3), 100*round(player_position_data$ft_pct, 3), round(player_position_data$ftr, 1), round(player_position_data$ts, 3)),
                             `National Rank` = c(player_position_data$`ORTG Rank`, player_position_data$`Usage Rank`, player_position_data$`Ast/TO Rank`,
                                                 player_position_data$`DRTG Rank`, player_position_data$`SPG Rank`, player_position_data$`Blk Rank`,
                                                 player_position_data$`EFG% Rank`, player_position_data$`FG% Rank`, player_position_data$`3P% Rank`,
                                                 player_position_data$`FT% Rank`, player_position_data$`Free Throw Rank`, player_position_data$`True Shooting Rank`),
                             conference = c(conference_level_player_filter$`ORTG Rank`, conference_level_player_filter$`Usage Rank`, conference_level_player_filter$`Ast/TO Rank`,
                                            conference_level_player_filter$`DRTG Rank`, conference_level_player_filter$`SPG Rank`, conference_level_player_filter$`Blk Rank`,
                                            conference_level_player_filter$`EFG% Rank`, conference_level_player_filter$`FG% Rank`, conference_level_player_filter$`3P% Rank`,
                                            conference_level_player_filter$`FT% Rank`, conference_level_player_filter$`Free Throw Rank`, conference_level_player_filter$`True Shooting Rank`))

final_table_df %>%
  gt() %>% 
  tab_header(title = player_var, 
             subtitle = html(web_image(
               url = logo,
               height = px(50)
             ))) %>% 
  gt_plt_bar_pct(column = National.Rank, labels = TRUE, fill = color, scaled = TRUE) %>% 
  gt_plt_bar_pct(column = conference, labels = TRUE, fill = color, scaled = TRUE) %>% 
  cols_label(National.Rank = "National Rank",
             conference = paste(conf_level, "Rank")) %>% 
  cols_align(align = "center")

test <- select(conference_level_filter, player, `FT% Rank`, ft_pct)
test <- arrange(test, `FT% Rank`)