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

UMass_passes_schedule <- get_team_schedule(team.name = "Massachusetts", season = "2023-24")
UMass_passes_schedule <- UMass_passes_schedule[!is.na(UMass_passes_schedule$Game_ID),]
UMass_passes_ids <- UMass_passes_schedule$Game_ID

UMass_team_stats <- get_team_stats(play_by_play_data = get_play_by_play(UMass_passes_ids), include_transition = T)

UMass_team_stats <- UMass_team_stats %>% filter(Team == "Massachusetts")

UMass_team_stats = UMass_team_stats[UMass_team_stats$Away != "UAlbany", ]
UMass_team_stats = UMass_team_stats[UMass_team_stats$Away != "UMass Lowell", ]
UMass_team_stats = UMass_team_stats[UMass_team_stats$Away != "Quinnipiac", ]

UMass_team_stats$"HC Passes" <- c(201, 183, 206, 184, 247, 218, 224, 207, 158, 197, 213, 191, 159)

UMass_team_stats$"Result" <- ifelse(UMass_team_stats$PTS > UMass_team_stats$dPTS, "W", "L")

ggplot(data = UMass_team_stats, aes(x = `HC Passes`, y = ORTG))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Half Court Passes vs Offensive Rating",
       x = "Half Court Passes",
       y = "Offensive Rating")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))

ggplot(data = UMass_team_stats, aes(x = `HC Passes`, y = NETRTG))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Half Court Passes vs Net Rating",
       x = "Half Court Passes",
       y = "Net Rating")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))

ggplot(data = UMass_team_stats, aes(x = `HC Passes`, y = TO))+ 
  geom_point(color = "#881c1c") + 
  geom_smooth(method=lm, se=FALSE, color = "black")

ggplot(data = UMass_team_stats, aes(x = `HC Passes`, y = FG.))+ 
  geom_point(color = "#881c1c") + 
  geom_smooth(method=lm, se=FALSE, color = "black")

ggplot(data = UMass_team_stats, aes(x = oPOSS_trans, y = ORTG))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Transition Possessions vs Offensive Rating",
       x = "Transition Possessions",
       y = "Offensive Rating")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))

ggplot(data = UMass_team_stats, aes(x = TimePerPoss, y = ORTG))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Time Per Possession vs Offensive Rating",
       x = "Time Per Possession",
       y = "Offensive Rating")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))

ggplot(data = UMass_team_stats, aes(x = oTransPCT, y = ORTG))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Transition % vs Offensive Rating",
       x = "Transition %",
       y = "Offensive Rating")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"))+
  scale_color_manual(values = c("W" = "darkgreen", "L" = "red"))

ggplot(data = UMass_team_stats, aes(x = PTS_trans, y = ORTG))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Transition Points vs Offensive Rating",
       x = "Transition Points",
       y = "Offensive Rating")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"),
        panel.background = element_rect(fill = "lightgrey")
  )+
  scale_color_manual(values = c("W" = "darkgreen", "L" = "red"))

ggplot(data = UMass_team_stats, aes(x = `HC Passes`, y = FTrate_half))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass Half Court Passes vs Half Court FT Rate",
       x = "Half Court Passes",
       y = "Half Court FT Rate")+
  theme(plot.title = element_text(hjust = 1, size = 16, face = "bold.italic"),
        panel.background = element_rect(fill = "lightgrey")
  )+
  scale_color_manual(values = c("W" = "darkgreen", "L" = "red"))

ggplot(data = UMass_team_stats, aes(x = ORTG, y = MIDrate_half))+ 
  geom_point(aes(size = 6, color = Result)) + 
  geom_smooth(method=lm, se=FALSE, color = "black")+
  labs(title = "UMass ORTG vs Half Court Mid Range Rate",
       x = "ORTG",
       y = "Half Court Mid Range Rate")+
  theme(plot.title = element_text(hjust = 1, size = 12, face = "bold.italic"),
        panel.background = element_rect(fill = "lightgrey")
  )+
  scale_color_manual(values = c("W" = "darkgreen", "L" = "red"))

avg_trans_poss <- mean(UMass_team_stats$oPOSS_trans)