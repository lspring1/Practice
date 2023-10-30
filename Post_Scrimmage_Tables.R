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

post_inter_scrimmage <- read.xlsx(file = "/Users/lukespring/Desktop/Practice Shiny/UMass_Practice_Site/23-24 Practice Stat Sheet.xlsx", "Iona")

team_possessions <- post_inter_scrimmage[19, 3]

columns_to_convert <- c("Points", "Rim Make", "Rim Miss", "Mid Make", "Mid Miss", "Three Make", "Three Miss", "Off", "Def",
                        "Ast", "Stl", "Blk", "Tov", "Charges", "Fouls")

post_inter_scrimmage_player <- post_inter_scrimmage %>% slice(1:16)

post_inter_scrimmage_player[is.na(post_inter_scrimmage_player)] <- 0

colnames(post_inter_scrimmage_player) <- post_inter_scrimmage_player[1, ]

post_inter_scrimmage_player <- post_inter_scrimmage_player[-1, ]

post_inter_scrimmage_player <- post_inter_scrimmage_player[, colSums(is.na(post_inter_scrimmage_player)) == 0]

post_inter_scrimmage_player <- post_inter_scrimmage_player %>%
  mutate_at(vars(columns_to_convert), as.numeric)

post_inter_scrimmage_player <- merge(post_inter_scrimmage_player, player_images_current, by = "Player")

post_inter_scrimmage_player$'Rim%' <- round(post_inter_scrimmage_player$'Rim Make'/(post_inter_scrimmage_player$'Rim Make'
                                                                          + post_inter_scrimmage_player$'Rim Miss')*100, 2)
post_inter_scrimmage_player$'Mid%' <- round(post_inter_scrimmage_player$'Mid Make'/(post_inter_scrimmage_player$'Mid Make'
                                                                          + post_inter_scrimmage_player$'Mid Miss')*100, 2)
post_inter_scrimmage_player$'3%' <- round(post_inter_scrimmage_player$'Three Make'/(post_inter_scrimmage_player$'Three Make'
                                                                          + post_inter_scrimmage_player$'Three Miss')*100, 2)
post_inter_scrimmage_player$'Eff. FG%' <- round(((post_inter_scrimmage_player$'Mid Make'+ post_inter_scrimmage_player$'Three Make'+ post_inter_scrimmage_player$'Rim Make'+
                                               (0.5 * post_inter_scrimmage_player$'Three Make'))/(post_inter_scrimmage_player$'Rim Miss'+ post_inter_scrimmage_player$'Rim Make'+
                                                                                               post_inter_scrimmage_player$'Mid Make'+post_inter_scrimmage_player$'Mid Miss'+post_inter_scrimmage_player$'Three Make'+post_inter_scrimmage_player$'Three Miss'))*100, 2)

post_inter_scrimmage_player$'Rim Attempt Rate (%)' <- round((post_inter_scrimmage_player$`Rim Make`+post_inter_scrimmage_player$`Rim Miss`)/(post_inter_scrimmage_player$`Rim Miss`+ post_inter_scrimmage_player$`Rim Make`+
                                                                                                                                post_inter_scrimmage_player$`Mid Make`+post_inter_scrimmage_player$`Mid Miss`+post_inter_scrimmage_player$`Three Make`+post_inter_scrimmage_player$`Three Miss`)*100, 2)

post_inter_scrimmage_player$'Mid Attempt Rate (%)' <- round((post_inter_scrimmage_player$`Mid Make`+post_inter_scrimmage_player$`Mid Miss`)/(post_inter_scrimmage_player$`Rim Miss`+ post_inter_scrimmage_player$`Rim Make`+
                                                                                                                                post_inter_scrimmage_player$`Mid Make`+post_inter_scrimmage_player$`Mid Miss`+post_inter_scrimmage_player$`Three Make`+post_inter_scrimmage_player$`Three Miss`)*100, 2)

post_inter_scrimmage_player$'Three Attempt Rate (%)' <- round((post_inter_scrimmage_player$`Three Make`+post_inter_scrimmage_player$`Three Miss`)/(post_inter_scrimmage_player$`Rim Miss`+ post_inter_scrimmage_player$`Rim Make`+
                                                                                                                                      post_inter_scrimmage_player$`Mid Make`+post_inter_scrimmage_player$`Mid Miss`+post_inter_scrimmage_player$`Three Make`+post_inter_scrimmage_player$`Three Miss`)*100, 2)

post_inter_scrimmage_player$TRB <- post_inter_scrimmage_player$Off+post_inter_scrimmage_player$Def

post_inter_scrimmage_player$'FG%' <- round((post_inter_scrimmage_player$`Rim Make`+post_inter_scrimmage_player$`Mid Make`+post_inter_scrimmage_player$`Three Make`)/(post_inter_scrimmage_player$'Rim Miss'+ post_inter_scrimmage_player$'Rim Make'+
                                                                                                                                                           post_inter_scrimmage_player$'Mid Make'+post_inter_scrimmage_player$'Mid Miss'+post_inter_scrimmage_player$'Three Make'+post_inter_scrimmage_player$'Three Miss')*100, 2)

post_inter_scrimmage_player$'OREB %' <- round(post_inter_scrimmage_player$Off/(post_inter_scrimmage_player$Off+post_inter_scrimmage_player$Def)*100, 2)

post_inter_scrimmage_player$'Rim' <- paste0(post_inter_scrimmage_player$`Rim Make`, "/", post_inter_scrimmage_player$`Rim Make` + post_inter_scrimmage_player$`Rim Miss`)

post_inter_scrimmage_player$'Mid' <- paste0(post_inter_scrimmage_player$`Mid Make`, "/", post_inter_scrimmage_player$`Mid Make` + post_inter_scrimmage_player$`Mid Miss`)

post_inter_scrimmage_player$'3PT' <- paste0(post_inter_scrimmage_player$`Three Make`, "/", post_inter_scrimmage_player$`Three Make` + post_inter_scrimmage_player$`Three Miss`)

post_inter_scrimmage_player$'FG' <- paste0(post_inter_scrimmage_player$`Three Make` + post_inter_scrimmage_player$`Mid Make` + post_inter_scrimmage_player$`Rim Make`, "/", post_inter_scrimmage_player$`Mid Make` + post_inter_scrimmage_player$`Mid Miss`+post_inter_scrimmage_player$`Rim Make` + post_inter_scrimmage_player$`Rim Miss`+post_inter_scrimmage_player$`Three Make` + post_inter_scrimmage_player$`Three Miss`)

post_inter_scrimmage_player <- arrange(post_inter_scrimmage_player, desc(Points))

post_inter_scrimmage_player[is.na(post_inter_scrimmage_player)] <- 0

# Calculate the totals row
totals_row <- post_inter_scrimmage_player %>%
  summarise(across(c(Points, `TRB`, `Ast`, `Off`, `Def`, `Stl`, `Blk`, `Tov`, `Charges`, `Fouls`, `Rim Make`, `Rim Miss`, `Mid Make`, `Mid Miss`,
                     `Three Make`, `Three Miss`), sum, na.rm = TRUE), Player = "Totals")

# Append the totals row to the dataframe
post_inter_scrimmage_player <- bind_rows(post_inter_scrimmage_player, totals_row)

post_inter_scrimmage_player[15, 17] <- "https://dxbhsrqyrr690.cloudfront.net/sidearm.nextgen.sites/umassathletics.com/images/nextgen_2022/logo_main.svg"

rim_make_sum <- sum(post_inter_scrimmage_player$`Rim Make`[1:14])
rim_miss_sum <- sum(post_inter_scrimmage_player$`Rim Miss`[1:14])
mid_make_sum <- sum(post_inter_scrimmage_player$`Mid Make`[1:14])
mid_miss_sum <- sum(post_inter_scrimmage_player$`Mid Miss`[1:14])
three_make_sum <- sum(post_inter_scrimmage_player$`Three Make`[1:14])
three_miss_sum <- sum(post_inter_scrimmage_player$`Three Miss`[1:14])
off_reb_perc <- round(sum(post_inter_scrimmage_player$Off[1:14])/(sum(post_inter_scrimmage_player$Off[1:14])+sum(post_inter_scrimmage_player$Def[1:14]))*100, 2)

post_inter_scrimmage_player[15, 30] <- paste0(rim_make_sum, "/", rim_miss_sum+rim_make_sum)
post_inter_scrimmage_player[15, 20] <- round(rim_make_sum/(rim_miss_sum+rim_make_sum)*100, 2)
post_inter_scrimmage_player[15, 31] <- paste0(mid_make_sum, "/", mid_miss_sum+mid_make_sum)
post_inter_scrimmage_player[15, 21] <- round(mid_make_sum/(mid_miss_sum+mid_make_sum)*100, 2)
post_inter_scrimmage_player[15, 32] <- paste0(three_make_sum, "/", three_miss_sum+three_make_sum)
post_inter_scrimmage_player[15, 22] <- round(three_make_sum/(three_miss_sum+three_make_sum)*100, 2)
post_inter_scrimmage_player[15, 29] <- round(off_reb_perc, 2)
post_inter_scrimmage_player[15, 33] <- paste0(rim_make_sum+mid_make_sum+three_make_sum, "/", rim_make_sum+mid_make_sum+three_make_sum+rim_miss_sum+mid_miss_sum+three_miss_sum)

team_FG_pct <- round((rim_make_sum+mid_make_sum+three_make_sum)/(rim_make_sum+mid_make_sum+three_make_sum+rim_miss_sum+mid_miss_sum+three_miss_sum)*100, 1)
team_def_FG_pct <- round((rim_make_sum+mid_make_sum+three_make_sum)/(rim_make_sum+mid_make_sum+three_make_sum+rim_miss_sum+mid_miss_sum+three_miss_sum)*100, 1)
team_3_pct <- round((three_make_sum/(three_miss_sum+three_make_sum))*100, 1)

post_inter_scrimmage_player %>% 
  gt() %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Basic Stats- `Sorted by Pts.`")) %>% 
   cols_hide(columns = c(`Rim Make`,`Rim Miss`, `Mid Make`, `Mid Miss`,`Three Make`, `Three Miss`, `Rim%`, `Mid%`, `3%`, `Eff. FG%`,
                         `Rim Attempt Rate (%)`, `Three Attempt Rate (%)`, `Mid Attempt Rate (%)`)) %>% 
   data_color(
     columns = c(Points, TRB, Ast, `FG%`),
     colors = scales::col_numeric(
      palette = c("darkred", "yellow", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "",
             Off = "OREB",
             Def = "DREB") %>% 
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
  # fmt_number(columns = c(FG_pct, three_pct, FT_pct),
  #            decimal = 2
  # ) %>% 
  cols_move_to_start(player_image) %>% 
  cols_move(TRB, Def) %>%
  cols_move(`OREB %`, Off) %>% 
  # cols_move(FG_pct, FG) %>% 
  # cols_move("3PT", FG_pct) %>% 
  # cols_move(three_pct, "3PT") %>%
  # cols_move(FT, three_pct) %>% 
  # cols_move(FT_pct, FT) %>% 
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0('FG%: ', team_def_FG_pct, '%  |  Def FG%: ', team_def_FG_pct, '%  |  3pt%: ', team_3_pct, '%  |  Possessions: ', team_possessions)) %>% 
  gtsave(paste0("UMass_Scrimmage_Stats.png"), path = "/Users/lukespring/Desktop/UMass Basketball Projects")

post_inter_scrimmage_player %>% 
  gt() %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Shooting Stats- `Sorted by Pts.`")) %>% 
  cols_hide(columns = c(Off, Def, Ast, Stl, Blk, Tov, Charges, Fouls, TRB, `Rim Make`, `Rim Miss`, `Mid Make`, `Mid Miss`, `Three Make`, `Three Miss`, `OREB %`)) %>% 
  data_color(
    columns = c(Points, `FG%`, `3%`, `Eff. FG%`),
    colors = scales::col_numeric(
      palette = c("darkred", "yellow", "darkgreen"),
      domain = NULL
    )
  ) %>%
  cols_label(player_image = "",
             `Rim Attempt Rate (%)` = "Rim Att (%)",
             `Mid Attempt Rate (%)` = "Mid Att (%)",
             `Three Attempt Rate (%)` = "Three Att (%)") %>% 
  text_transform(
    locations = cells_body(columns = player_image),
    fn = function(x) {
      web_image(x,
                height = px(30))
    }
  ) %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_text(size = px(8))
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>% 
  # fmt_number(columns = c(FG_pct, three_pct, FT_pct),
  #            decimal = 2
  # ) %>% 
  cols_move_to_start(player_image) %>% 
  cols_move(`FG%`, Points) %>% 
  cols_move(`Eff. FG%`, `FG%`) %>% 
  cols_move(`Rim%`, `FG%`) %>%
  cols_move(`Rim Attempt Rate (%)`, `Rim%`) %>% 
  cols_move(`Mid%`, `Rim Attempt Rate (%)`) %>% 
  cols_move(`Mid Attempt Rate (%)`, `Mid%`) %>% 
  cols_move(`3%`, `Mid Attempt Rate (%)`) %>%
  cols_move(`Three Attempt Rate (%)`, `3%`) %>% 
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0('FG%: ', team_def_FG_pct, '%  |  Def FG%: ', team_def_FG_pct, '%  |  3pt%: ', team_3_pct, '%  |  Possessions: ', team_possessions)) %>% 
  gtsave(paste0("UMass_Scrimmage_Shooting_Stats.png"), path = "/Users/lukespring/Desktop/UMass Basketball Projects")

post_inter_scrimmage_player %>%
  gt() %>%
  gt_theme_538() %>% 
  gt_highlight_rows(rows = c(2, 4, 6, 8, 10, 12, 14), fill = 'gray94', font_weight = 'normal', font_color = 'gray30') %>% 
  tab_header(title = md("**UMass**"), subtitle = md("Practice Stats- `Sorted by Pts.`")) %>%
  cols_hide(columns = c(`Eff. FG%`, `Rim Attempt Rate (%)`, `Mid Attempt Rate (%)`, `Three Attempt Rate (%)`, `FG%`, `Rim Make`, `Rim Miss`, `Mid Make`, 
                        `Mid Miss`, `Three Make`, `Three Miss`, `Rim%`, `Mid%`, `3%`, Rim, Mid)) %>%
  cols_label(player_image = '', Player = md('**Player**'), `FG%` = md('**FG**'), '3PT' = md('**3P**'), Off = md('**OReb**'), `OREB %` = md('**OReb%**'), Def = md('**DReb**'), TRB = md('**TRB**'), Points = md('**PTS**'), Ast = md('**AST**'), Tov = md('**TOV**'), Stl = md('**STL**'), Blk = md('**BLK**')) %>%
  text_transform(
    locations = cells_body(columns = player_image),
    fn = function(x) {
      web_image(x,
                height = px(30))
    }
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_text(size = px(10))
    ),
    locations = cells_body(
      columns = Player
    )
  ) %>%
  cols_move_to_start(player_image) %>%
  cols_move(`TRB`, Points) %>%
  cols_move(`Ast`, `TRB`) %>%
  cols_move(`Rim`, `Ast`) %>%
  cols_move(`Rim%`, `Rim`) %>%
  cols_move(`Mid`, `Rim%`) %>%
  cols_move(`Mid%`, `Mid`) %>%
  cols_move(`3PT`, `Mid%`) %>%
  cols_move(`3%`, `3PT`) %>%
  cols_move(`OREB %`, `Off`) %>%
  cols_align(align = "center") %>% 
  tab_source_note(source_note = paste0('FG%: ', team_def_FG_pct, '%  |  Def FG%: ', team_def_FG_pct, '%  |  3pt%: ', team_3_pct, '%  |  Possessions: ', team_possessions)) %>%
  gtsave(paste0("UMass_Scrimmage_Super_Basic_Stats.png"), path = "/Users/lukespring/Desktop/UMass Basketball Projects")