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
library(fmsb)

# player_image <- c(
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FDaniel_Hankins_Sanford_d3tPh.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJaylen_HWhjg.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRahsool_Diggins_nOrSz.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRobert_F5SIh.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FKeon_Thompson_j7lVX.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FMarqui_Worthy_fB7nD.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJayden_Ndjigue_uDraA.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FTarique_Foster_JV3ru.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRyan_Marcus_zHwGy.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJackson_Cronin_SdErY.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FJosh_MYSjy.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FMatt_q1H5m.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FMathok_PYwqm.jpg&width=100&height=100&gravity=north&type=webp",
#   "https://images.sidearmdev.com/crop?url=https%3A%2F%2Fdxbhsrqyrr690.cloudfront.net%2Fsidearm.nextgen.sites%2Fumassathletics.com%2Fimages%2F2023%2F10%2F10%2FRollie_Castineyra_CgC7L.jpg&width=100&height=100&gravity=north&type=webp"
# )
# 
# 
# Player <- c("Daniel Hankins-Sanford", "Jaylen Curry", "Rahsool Diggins", "Robert Davis Jr.", "Keon Thompson", "Marqui Worthy", "Jayden Ndjigue",
#             "Tarique Foster", "Ryan Marcus", "Jackson Cronin", "Josh Cohen", "Matt Cross", "Mathok Majok", "Rollie Castineyra")
# 
# player_images_current <- data.frame(player_image, Player)

full_practice_stats <- read.xlsx(file = "23-24 Practice Stat Sheet.xlsx", "Grand Totals")

# full_practice_stats <- merge(full_practice_stats, player_images_current, by = "Player")
# 
# cols_move_to_start(full_practice_stats, player_image)

possessions <- as.numeric(full_practice_stats[18, 3])

practice_stats <- full_practice_stats %>% slice(1:16)

tests <- full_practice_stats[17:17, ]

colnames(practice_stats) <- practice_stats[1, ]

practice_stats <- practice_stats[-1, ]

practice_stats <- practice_stats[ -c(17:26) ]

columns_to_convert <- c("Points", "Rim Make", "Rim Miss", "Mid Make", "Mid Miss", "Three Make", "Three Miss", "Off", "Def",
                        "Ast", "Stl", "Blk", "Tov", "Charges", "Fouls")

practice_stats <- practice_stats %>%
  mutate_at(vars(columns_to_convert), as.numeric)

practice_stats_per_70 <- practice_stats %>% 
  mutate_at(vars(columns_to_convert), list(~ . * 140))

practice_stats_per_70 <- practice_stats_per_70 %>% 
  mutate_at(vars(columns_to_convert), list(~ . / possessions))

practice_stats_per_70 <- practice_stats_per_70 %>%
  mutate_at(vars(columns_to_convert), list(~ round(., 2)))

selected_data_shooting <- practice_stats

selected_data_shooting$'Rim%' <- round(selected_data_shooting$'Rim Make'/(selected_data_shooting$'Rim Make'
                                                                          + selected_data_shooting$'Rim Miss')*100, 2)
selected_data_shooting$'Mid%' <- round(selected_data_shooting$'Mid Make'/(selected_data_shooting$'Mid Make'
                                                                          + selected_data_shooting$'Mid Miss')*100, 2)
selected_data_shooting$'3%' <- round(selected_data_shooting$'Three Make'/(selected_data_shooting$'Three Make'
                                                                          + selected_data_shooting$'Three Miss')*100, 2)
selected_data_shooting$'Eff. FG%' <- round(((selected_data_shooting$'Mid Make'+ selected_data_shooting$'Three Make'+ selected_data_shooting$'Rim Make'+
                                               (0.5 * selected_data_shooting$'Three Make'))/(selected_data_shooting$'Rim Miss'+ selected_data_shooting$'Rim Make'+
                                                                                               selected_data_shooting$'Mid Make'+selected_data_shooting$'Mid Miss'+selected_data_shooting$'Three Make'+selected_data_shooting$'Three Miss'))*100, 2)

selected_data_shooting$'Rim Attempt Rate (%)' <- round((selected_data_shooting$`Rim Make`+selected_data_shooting$`Rim Miss`)/(selected_data_shooting$`Rim Miss`+ selected_data_shooting$`Rim Make`+
                                                                                                                                selected_data_shooting$`Mid Make`+selected_data_shooting$`Mid Miss`+selected_data_shooting$`Three Make`+selected_data_shooting$`Three Miss`)*100, 2)

selected_data_shooting$'Mid Attempt Rate (%)' <- round((selected_data_shooting$`Mid Make`+selected_data_shooting$`Mid Miss`)/(selected_data_shooting$`Rim Miss`+ selected_data_shooting$`Rim Make`+
                                                                                                                                selected_data_shooting$`Mid Make`+selected_data_shooting$`Mid Miss`+selected_data_shooting$`Three Make`+selected_data_shooting$`Three Miss`)*100, 2)

selected_data_shooting$'Three Attempt Rate (%)' <- round((selected_data_shooting$`Three Make`+selected_data_shooting$`Three Miss`)/(selected_data_shooting$`Rim Miss`+ selected_data_shooting$`Rim Make`+
                                                                                                                                      selected_data_shooting$`Mid Make`+selected_data_shooting$`Mid Miss`+selected_data_shooting$`Three Make`+selected_data_shooting$`Three Miss`)*100, 2)



selected_data_shooting[is.na(selected_data_shooting)] <- 0

selected_data_shooting <- selected_data_shooting[ , -c(9:16)]

ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("UMass Basketball"),
                navbarPage("",
                           tabPanel(
                             "UMass Basketball Practice Stats"
                           ),
                           tabPanel("Line Chart",
                                    sidebarPanel(
                                      selectInput(inputId = "player_selector_line", label = "Select a Player:", choices = practice_stats$Player),
                                      selectInput(inputId = "stat_selector_line", label = "Select a Stat Category:", choices = c("Points", "Off", "Def", "Ast", "Stl", "Blk", 
                                                                                                                                 "Tov", "Charges", "Fouls", "FG%", "3P%", "EFG%",
                                                                                                                                 'Rim Attempt Rate (%)', 'Mid Attempt Rate (%)',
                                                                                                                                 'Three Attempt Rate (%)'))
                                    ),
                                    mainPanel(
                                      plotOutput("line_chart")
                                    )),
                           tabPanel("Individual Player Practice Totals",
                                    sidebarPanel(
                                      selectInput(inputId = "player_selector", label = "Select a Player:", choices = practice_stats$Player)
                                    ),
                                    mainPanel(
                                      div(id = "table_buttons",
                                          dateInput("selectedDate1", "Select Date:", value = Sys.Date()),
                                          downloadButton("downloadTable1", "Download as PDF")
                                      ),
                                      dataTableOutput("ind_player_totals"))
                           ),
                           tabPanel("Full Player Practice Stats",
                                    mainPanel(
                                      div(id = "table_buttons1",
                                          actionButton(inputId = "switch_button3", label = "Switch to Original Data"),
                                          actionButton(inputId = "switch_button4", label = "Switch to Per 70 Data"),
                                          dateInput("selectedDate2", "Select Date:", value = Sys.Date()),
                                          downloadButton("downloadTable2", "Download as PDF")
                                      ),
                                      dataTableOutput("player_totals")
                                    )
                           ),
                           tabPanel("Radar Chart",
                                    sidebarPanel(
                                      selectInput(inputId = "player_selector_radar", label = "Select a Player:", choices = practice_stats$Player)                                    ),
                                    mainPanel(
                                      div(id = "table_buttons4",
                                          actionButton(inputId = "switch_button8", label = "Switch to Basic Data"),
                                          actionButton(inputId = "switch_button9", label = "Switch to Shooting Data")),
                                      plotOutput("radar_chart")
                                    )),
                           tabPanel("Grand Total Stats",
                                    mainPanel(
                                      div(id = "table_buttons2",
                                          actionButton(inputId = "switch_button5", label = "Switch to Original Data"),
                                          actionButton(inputId = "switch_button6", label = "Switch to Per 70 Data"),
                                          actionButton(inputId = "switch_button7", label = "Switch to Shooting Stats"),
                                      ),
                                      dataTableOutput("grand_totals")
                                    )),
                           tabPanel("Team Stats",
                                    mainPanel(
                                      div(id = "table_buttons3",
                                          actionButton(inputId = "team_stats_original", label = "Switch to Original Data"),
                                          actionButton(inputId = "team_stats_per_70", label = "Switch to Per 70 Data"),
                                      ),
                                      dataTableOutput("team_stats")
                                    )),
                )
)

server <- function(input, output) {
  
  observeEvent(input$player_selector_line, {
    selected_sheet3 <- as.character(input$player_selector_line)
    
    selected_data4 <- read.xlsx(file = "23-24 Practice Stat Sheet.xlsx", selected_sheet3)
    
    selected_data4 <- selected_data4 %>%
      mutate_at(-1, as.numeric)
    
    selected_data4$'Rim Attempt Rate (%)' <- round((selected_data4$Rim.Make + selected_data4$Rim.Miss)/(selected_data4$Rim.Miss + selected_data4$Rim.Make+
                                                                                                          selected_data4$Mid.Make+selected_data4$Mid.Miss+selected_data4$Three.Make+selected_data4$Three.Miss)*100, 2)
    
    selected_data4$'Mid Attempt Rate (%)' <- round((selected_data4$Mid.Make+selected_data4$Mid.Miss)/(selected_data4$Rim.Miss+ selected_data4$Rim.Make+
                                                                                                        selected_data4$Mid.Make+selected_data4$Mid.Miss+selected_data4$Three.Make+selected_data4$Three.Miss)*100, 2)
    
    selected_data4$'Three Attempt Rate (%)' <- round((selected_data4$Three.Make+selected_data4$Three.Miss)/(selected_data4$Rim.Miss+ selected_data4$Rim.Make+
                                                                                                              selected_data4$Mid.Make+selected_data4$Mid.Miss+selected_data4$Three.Make+selected_data4$Three.Miss)*100, 2)
    
    selected_data4 <- selected_data4[ , -c(3:8)]
    
    colnames(selected_data4)[colnames(selected_data4) == "FG."] <- "FG%"
    colnames(selected_data4)[colnames(selected_data4) == "X3P."] <- "3P%"
    colnames(selected_data4)[colnames(selected_data4) == "EFG."] <- "EFG%"
    
    output$line_chart <- renderPlot({
      ggplot(selected_data4,aes(x= Practice.Number, y=.data[[input$stat_selector_line]]))+ 
        geom_line()+
        geom_point()+
        labs(x = "Practice Number", 
             title = paste0(c(input$player_selector_line, " ", input$stat_selector_line, "Progression")))
    })
  })
  
  observeEvent(input$player_selector_radar, {
    
    radar_filtered_data <- filter(practice_stats, Player == input$player_selector_radar)
    
    radar_filtered_data <- merge(radar_filtered_data, selected_data_shooting, by = "Player")
    
    radar_chart_data_normal <- data.frame(Points = c(max(practice_stats$Points), min(practice_stats$Points), radar_filtered_data[1, "Points.x"]),
                                          Assists = c(max(practice_stats$Ast), min(practice_stats$Ast), radar_filtered_data[1, "Ast"]),
                                          Def = c(max(practice_stats$Def), min(practice_stats$Def), radar_filtered_data[1, "Def"]),
                                          Off = c(max(practice_stats$Off), min(practice_stats$Off), radar_filtered_data[1, "Off"]),
                                          TOV = c(max(practice_stats$Tov), min(practice_stats$Tov), radar_filtered_data[1, "Tov"]))
    
    
    radar_chart_data_shooting <- data.frame(rim_pct = c(max(selected_data_shooting$`Rim%`), min(selected_data_shooting$`Rim%`), radar_filtered_data[1, "Rim%"]),
                                            mid_pct = c(max(selected_data_shooting$`Mid%`), min(selected_data_shooting$`Mid%`), radar_filtered_data[1, "Mid%"]),
                                            three_pct = c(max(selected_data_shooting$`3%`), min(selected_data_shooting$`3%`), radar_filtered_data[1, "3%"]),
                                            eff_fg = c(max(selected_data_shooting$`Eff. FG%`), min(selected_data_shooting$`Eff. FG%`), radar_filtered_data[1, "Eff. FG%"]),
                                            rim_att = c(max(selected_data_shooting$`Rim Attempt Rate (%)`), min(selected_data_shooting$`Rim Attempt Rate (%)`), radar_filtered_data[1, "Rim Attempt Rate (%)"]),
                                            mid_att = c(max(selected_data_shooting$`Mid Attempt Rate (%)`), min(selected_data_shooting$`Mid Attempt Rate (%)`), radar_filtered_data[1, "Mid Attempt Rate (%)"]),
                                            three_att = c(max(selected_data_shooting$`Three Attempt Rate (%)`), min(selected_data_shooting$`Three Attempt Rate (%)`), radar_filtered_data[1, "Three Attempt Rate (%)"]))
    
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "eff_fg"] <- "Eff FG%"
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "rim_pct"] <- "Rim %"
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "mid_pct"] <- "Mid %"
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "rim_att"] <- "Rim Attempt Rate (%)"
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "mid_att"] <- "Mid Attempt Rate (%)"
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "three_att"] <- "Three Attempt Rate (%)"
    colnames(radar_chart_data_shooting)[colnames(radar_chart_data_shooting) == "three_pct"] <- "3P%"
    colnames(radar_chart_data_normal)[colnames(radar_chart_data_normal) == "Def"] <- "Defensive Reb"
    colnames(radar_chart_data_normal)[colnames(radar_chart_data_normal) == "Off"] <- "Offensive Reb"
    
    radar_chart_data <- reactiveVal(radar_chart_data_normal)
    
    observeEvent(input$switch_button8, {
      radar_chart_data(radar_chart_data_normal)
    })
    
    observeEvent(input$switch_button9, {
      radar_chart_data(radar_chart_data_shooting)
    })
    
    output$radar_chart <- renderPlot({
      radarchart(radar_chart_data(),
                 seg = 7,
                 title = paste0(c(input$player_selector_radar, "Radar Chart")),
                 pfcol = scales::alpha("#881c1c", 0.3),
                 plwd = 2)
    })
  })
  
  observeEvent(input$selectedDate1, {
    selected_sheet1 <- as.character(input$selectedDate1)
    
    tryCatch({
      selected_data1 <- read.xlsx(file = "23-24 Practice Stat Sheet.xlsx", selected_sheet1)
      
      if (is.null(selected_data1)) {
        output$ind_player_totals <- renderText({
          "No data"
        })
      } else {
        selected_data1 <- selected_data1 %>% 
          slice(1:16)
        
        colnames(selected_data1) <- selected_data1[1, ]
        
        selected_data1 <- selected_data1[-1, ]
        
        selected_data1 <- selected_data1[, colSums(is.na(selected_data1)) == 0]
        
        selected_data1 <- selected_data1[, 1:16]
        
        output$ind_player_totals <- renderDataTable({
          if (!is.null(input$player_selector)) {
            selected_data1 <- selected_data1 %>% 
              filter(Player == input$player_selector)
          }
          selected_data1
        }, options = list(ordering = TRUE))
      }
    }, error = function(e) {
      output$ind_player_totals <- renderText({
        "Error: Unable to load data"
      })
    })
  })
  
  observeEvent(input$selectedDate2, {
    selected_sheet2 <- as.character(input$selectedDate2)
    
    tryCatch({
      selected_data2 <- read.xlsx(file = "23-24 Practice Stat Sheet.xlsx", selected_sheet2)
      
      if (is.null(selected_data2)) {
        output$player_totals <- renderText({
          "No data"
        })
      } else {
        
        selected_data2_possessions <- as.numeric(selected_data2[18, 3])
        
        selected_data2 <- selected_data2 %>% 
          slice(1:16)
        
        colnames(selected_data2) <- selected_data2[1, ]
        
        selected_data2 <- selected_data2[-1, ]
        
        selected_data2 <- selected_data2[, colSums(is.na(selected_data2)) == 0]
        
        selected_data2 <- selected_data2[, 1:16]
        
        selected_data2 <- selected_data2 %>%
          mutate_at(vars(columns_to_convert), as.numeric)
        
        selected_data2_per_70 <- selected_data2 %>% 
          mutate_at(vars(columns_to_convert), list(~ . * 140))
        
        selected_data2_per_70 <- selected_data2_per_70 %>% 
          mutate_at(vars(columns_to_convert), list(~ . / selected_data2_possessions))
        
        selected_data2_per_70 <- selected_data2_per_70 %>%
          mutate_at(vars(columns_to_convert), list(~ round(., 2)))
        
        selected_data_2 <- reactiveVal(selected_data2)
        
        observeEvent(input$switch_button3, {
          selected_data_2(selected_data2)
        })
        
        observeEvent(input$switch_button4, {
          selected_data_2(selected_data2_per_70)
        })
        
        output$player_totals <- renderDataTable({
          selected_data_2()
        }, options = list(ordering = TRUE))
      }
    }, error = function(e) {
      output$player_totals <- renderText({
        "Error: Unable to load data"
      })
    })
  })
  
  selected_data3 <- reactiveVal(practice_stats)
  
  observeEvent(input$switch_button5, {
    selected_data3(practice_stats)
  })
  
  observeEvent(input$switch_button6, {
    selected_data3(practice_stats_per_70)
  })
  
  observeEvent(input$switch_button7, {
    selected_data3(selected_data_shooting)
  })
  
  output$grand_totals <- renderDataTable({
    selected_data3()
  }, options = list(ordering = TRUE))
  
}

shinyApp(ui, server)