# ------------------------------------------------------------------------------------------------------

# Loading Packages
library(shiny)
library(dplyr)
library(scales)
library(DT)
library(reactable)
library(fivethirtyeight)
library(reactablefmtr)
library(reticulate)
library(bslib)
library(extrafont)
library(shinydashboard)
# reticulate::virtualenv_create(packages = c("pandas", "nba_api"))
# py_install("pandas")
# py_install("nba_api")
# py_run_file('get_nba_data.py', convert = TRUE)
# use_virtualenv("r-reticulate")

# ------------------------------------------------------------------------------------------------------

# Loading Data
players <- read.csv("nba_player_stats.csv")
# players <- py$get_player_data()
players <- players %>% 
  mutate(`HEADSHOT` = paste0('<img src="https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/', PLAYER_ID, '.png"  height="52"></img>'),
         `HEADSHOT_REAC` = paste0('https://ak-static.cms.nba.com/wp-content/uploads/headshots/nba/latest/260x190/', PLAYER_ID, '.png'))
players$MPG <- round(players$MIN/players$GP, 1)
teams_full <- data.frame(c('ATL', 'BKN', 'BOS', 'CHA', 'CHI', 'CLE', 'DAL', 'DEN', 'DET', 'GSW', 'HOU', 'IND', 'LAC', 'LAL', 'MEM', 'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI', 'PHX', 'POR', 'SAC', 'SAS', 'TOR', 'UTA', 'WAS'),
                         c('Atlanta Hawks', 'Brooklyn Nets', 'Boston Celtics', 'Charlotte Hornets', 'Chicago Bulls', 'Cleveland Cavaliers', 'Dallas Mavericks', 'Denver Nuggets', 'Detroit Pistons', 'Golden State Warriors', 'Houston Rockets', 'Indiana Pacers', 'LA Clippers', 'LA Lakers', 'Memphis Grizzlies', 'Miami Heat', 'Milwaukee Bucks', 'Minnesota Timberwolves', 'New Orleans Pelicans', 'New York Knicks', 'Oklahoma City Thunder', 'Orlando Magic', 'Philadelphia 76ers', 'Phoenix Suns', 'Portland Trailblazers', 'Sacramento Kings', 'San Antonio Spurs', 'Toronto Raptors', 'Utah Jazz', 'Washington Wizards'),
                         c('#E03A3E', '#000000', '#007A33', '#1D1160', '#CE1141', '#860038', '#00538C', '#0E2240', '#C8102E', '#1D428A', '#CE1141', '#FDBB30', '#C8102E', '#FDB927', '#5D76A9', '#98002E', '#00471B', '#0C2340', '#85714D', '#F58426', '#007AC1', '#0077C0', '#006BB6', '#1D1160', '#E03A3E', '#5A2D81', '#C4CED4', '#CE1141', '#F9A01B', '#002B5C'))
colnames(teams_full) <- c('TEAM_ABBREVIATION', 'TEAM_FULL', 'TEAM_COLOR')

df <- read.csv("nba_lineups_five_players.csv")
# df <- py$get_lineup_data()

find_team <- function(team) {
  team_full <- teams_full[which(teams_full[, 1] == team), 2]
  team_full
}
find_color <- function(team) {
  team_color <- teams_full[which(teams_full[, 1] == team), 3]
  team_color
}
df$TEAM_FULL <- lapply(df$TEAM_ABBREVIATION, find_team)
players$TEAM_COLOR <- as.character(lapply(players$TEAM_ABBREVIATION, find_color))

# Adjustments for +/-
PPG <- round(((df$PTS/df$MIN) * 48),0)
PLUS_MINUS_PURE <- ((df$PLUS_MINUS/df$MIN) * 48)
P_PLUS_MINUS_PURE <- ((players$PLUS_MINUS/players$MIN) * 48)

df$PPG <- PPG
df$PLUS_MINUS_PURE <- PLUS_MINUS_PURE
players$PLUS_MINUS_PURE <- P_PLUS_MINUS_PURE
df <- df %>% mutate(PLUS_MINUS_STAN = ((PLUS_MINUS_PURE*MIN) + (mean(PLUS_MINUS_PURE)*mean(MIN))) / (MIN + mean(MIN)))
players <- players %>% mutate(PLUS_MINUS_STAN = ((PLUS_MINUS_PURE*MIN) + (mean(PLUS_MINUS_PURE)*mean(MIN))) / (MIN + mean(MIN)))
players$PPG <- round(((players$PTS/players$MIN) * 48),1)

# Lineup Chemistry Calculations
chemistry <- c()
for (row in 1:nrow(df)) {
  # total_pm <- df$PLUS_MINUS_PURE[row]
  total_pm <- df$PLUS_MINUS_STAN[row]
  for (player in df[row,colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')]) {
    # total_pm <- total_pm - players$PLUS_MINUS_PURE[which(players$PLAYER_NAME == player)]
    total_pm <- total_pm - players$PLUS_MINUS_STAN[which(players$PLAYER_NAME == player)]
  }
  chemistry <- append(chemistry, total_pm)
} 
df$TEAM_CHEMISTRY <- chemistry

# Identifying Players in Lineups
player_ids <- data.frame(strsplit(df$GROUP_ID, "-"))
player_ids <- player_ids[-1,]
colnames(player_ids) <- c(1:2000)
player_ids <- t(player_ids)
colnames(player_ids) <- c('P1', 'P2', 'P3', 'P4', 'P5')

for (id in players$PLAYER_ID) {
  player_ids <- gsub(id, players$PLAYER_NAME[which(players$PLAYER_ID == id)], player_ids)
}
player_ids <- data.frame(player_ids)

df$P1 <- player_ids$P1
df$P2 <- player_ids$P2
df$P3 <- player_ids$P3
df$P4 <- player_ids$P4
df$P5 <- player_ids$P5

# Number of Lineups
get_n_lineups <- as.data.frame(table(unlist(as.list(c(df$P1, df$P2, df$P3, df$P4, df$P5)))))
colnames(get_n_lineups) <- c('PLAYER_NAME', 'N_LINEUPS')
players <- players %>% 
  left_join(get_n_lineups, by = c('PLAYER_NAME' = 'PLAYER_NAME'))

all_teams <- sort(unique(df$TEAM_ABBREVIATION))

all_players <- unlist(unique(data.frame(players = unlist(df[, which(colnames(df) %in% c('P1','P2','P3','P4','P5'))]))))

# ------------------------------------------------------------------------------------------------------

# Shiny App

# UI
ui <- fluidPage(
  theme = bs_theme(bg = '#202123', fg = '#B8BCC2', primary = '#EA80FC', base_font = 'Andale Mono'),
  titlePanel("NBA Lineup Analysis App (2022-2023)"),
  tabsetPanel(
    tabPanel("Home", fluid = TRUE,
             fluidRow(
               div("Welcome!", style = "color: #EA80FC; font-size: 30px; font-style: bold"),
               div("This NBA Lineup Analysis App allows users to compare different lineups used in
                   regular-season NBA games for the 2022-23 season. The analysis uses data acquired 
                   from the NBA Stats API to determine how one team's lineup will match up to another
                   team's lineup, as well as displaying individual player accomplishments for each team,
                   and rating a given lineup's court chemistry. The analysis provides results that are 
                   quanitfiably informed from in-game data.")
             ),
             fluidRow(
               div("Matchup Analysis", style = "color: #EA80FC ; font-size: 24px; font-style: bold"),
               div('The Matchup Analysis tool prompts the user to select a five-player lineup from two
                   different NBA teams to predict the outcome of a hypothetical matchup. The tool 
                   calculates the probability that one lineup is "better" than the other; this 
                   probability is determined from the difference between both lineups Plus/Minus (+/-)
                   statistic on a probability distribution. The tool also calculates the expected point
                   differential that one lineup will outscore the other lineup by in a classic 48-minute
                   game.'),
               div('. ', style = 'color: #202123'),
               div('How to use: ', style = 'font-style: italic; font-weight: bold'),
               div('Begin by selecting two teams on the left-hand side panel. The input boxes in the main
                   panel will populate with player options to select, based on the teams selected in the 
                   sidebar. Select five (and only five) players from each team, and the calculations will
                   automatically generate. *NOTE: if the selected lineup has not played in a regular-season
                   game, the user will be prompted to select a different lineup, since there will be no
                   data for the original data, causing an inability to compute the analysis calculations.')
             ),
             fluidRow(
               div("Individual Player Analysis", style = "color: #EA80FC ; font-size: 24px; font-style: bold"),
               div("The Individual Player Analysis tool displays the top five players on each team, ranked by
                   the number of lineups they have participated in for that team in regular games during the 
                   2022-23 season. The first table in this tool displays these rankings, as well as some basic
                   stats for each of these players. The second table (Player Leaderboard) in the tool displays a leaderboard for a 
                   variety of traditional stats for each player on the team."),
               div('. ', style = 'color: #202123'),
               div('How to use: ', style = 'font-style: italic; font-weight: bold'),
               div("Begin by selecting a team on the left-hand side panel. Both tables will automatically populate
                   with data for players from the selected team. The first table (Top Players) is a static table, meaning
                   it is not interactive. The second table (Player Leaderboard) has interactive capabilites, and can be sorted
                   by any of the stat columns. Simply click on the column name to sort the table by the column of interest."
                   )
             ),
             fluidRow(
               div("Team Chemistry Analysis", style = "color: #EA80FC ; font-size: 24px; font-style: bold"),
               div("The Team Chemistry Analysis tool assesses the on-court chemistry of a particular lineup. In this sense, 
                   chemistry is defined as the performance of the lineup as a whole, in comparison to the typical performance 
                   of each of its individual players. Chemistry is therefore calculated as the difference in the Plus/Minus 
                   value of the overall lineup and the aggregation (sum) of Plus/Minus values of each member of the lineup. The
                   table displays some basic stats for each of the members of the selected lineup. The plot below the table displays
                   where the selected lineup's Chemistry value lies in a distribution plot of lineup Chemistry values for all other
                   lineups in the data set."),
               div('. ', style = 'color: #202123'),
               div('How to use: ', style = 'font-style: italic; font-weight: bold'),
               div("Begin by selecting a team on the left-hand side panel. The input box in the main
                   panel will populate with player options to select, based on the team selected in the 
                   sidebar. Select five (and only five) players, and the lineup Chemistry value will automatically 
                   generate in the upper right-hand corner. *NOTE: if the selected lineup has not played in a regular-season
                   game, the user will be prompted to select a different lineup, since there will be no
                   data for the original data, causing an inability to compute the analysis calculations.")
             )
           ),
    tabPanel("Matchup Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("team_choice_1", label = "Choose Team 1:", choices = all_teams, selected = all_teams[[1]]),
                 selectInput("team_choice_2", label = "Choose Team 2:", choices = all_teams, selected = all_teams[[1]]),
                 width = 2
               ),
               mainPanel(
                 fluidRow(
                   column(
                     textOutput("table1_cap"), width = 4
                   ), 
                   column(width = 4), 
                   column(
                     textOutput("table2_cap"), width = 4
                   )
                 ),
                 fluidRow(
                   column(
                     dataTableOutput("tableA"), width = 4
                   ),
                   column(width = 4),
                   column(
                     dataTableOutput("tableB"), width = 4
                   )
                 ),
                 fluidRow(
                   column(width = 12, offset = 0, style='padding:20px;')
                 ),
                 fluidRow(
                   column(
                     align = 'center',
                     textOutput("noLineup"), width = 12
                   )
                 ),
                 fluidRow(
                   column(
                     textOutput("analysis_val1"), width = 8
                   ),
                   column(
                     textOutput("analysis_val2"), width = 4
                   )
                 ),
                 fluidRow(
                   column(
                     textOutput("analysis_text1"), width = 6
                   ),
                   column(width = 2),
                   column(
                     textOutput("analysis_text2"), width = 4
                   )
                 ),
                 fluidRow(
                   column(width = 12, offset = 0, style='padding:20px;')
                 ),
                 fluidRow(
                   column(4,
                          selectInput("players_team_1", label = "Choose Five Players from Team 1: ", choices = all_players, selected = all_players[[1]], multiple = TRUE),
                   ),
                   column(width = 4),
                   column(4,
                          selectInput("players_team_2", label = "Choose Five Players from Team 2: ", choices = all_players, selected = all_players[[1]], multiple = TRUE),
                   )
                 )
               )
               
               
               
               
             ),
             tags$head(tags$style("#table1_cap{color: #EA80FC; font-size: 30px; font-style: bold}", "#table2_cap{color: #EA80FC; font-size: 30px; font-style: bold}", 
                                  "#analysis_val1{color: #EA80FC; font-size: 30px; font-style: bold}", "#analysis_val2{color: #EA80FC; font-size: 30px; font-style: bold}",
                                  "#noLineup{color: red; font-size: 20px}",))
    ),
    tabPanel("Individual Player Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(selectInput("ind_team_choice", label = "Choose Team:", choices = all_teams, selected = all_teams[[1]]),
                            width = 2),
               mainPanel(
                 fluidRow(
                   column(textOutput("indTable_cap"),
                          width = 12)
                 ),
                 fluidRow(
                   column(
                     dataTableOutput("indTable"),
                     width = 12
                   )
                 ),
                 fluidRow(
                   column(textOutput("leaderboard_cap"),
                          width = 12)
                 ),
                 fluidRow(
                   column(
                     reactableOutput("leaderboard"),
                     width = 12)
                 )
               )
             ),
             tags$head(tags$style("#indTable_cap{color: #EA80FC; font-size: 30px; font-style: bold}", "#leaderboard_cap{color: #EA80FC; font-size: 30px; font-style: bold}"))
    ),
    tabPanel("Team Chemistry Analysis", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 selectInput("chem_choice", label = "Choose Team:", choices = all_teams, selected = all_teams[[1]]),
                 width = 2
               ),
               mainPanel(
                 fluidRow(
                   column(
                     width = 8
                   ),
                   column(
                     textOutput("chemText"),
                     width = 4
                   )
                 ),
                 fluidRow(
                   column(
                     textOutput("chemTable_cap"), width = 6
                   ),
                   column(width = 2), 
                   column(
                     textOutput("chemValue"),
                     width = 4
                   )
                 ),
                 fluidRow(
                   column(
                     dataTableOutput("chemTable"), width = 4
                   ),
                   column(width = 4), 
                   column(
                     selectInput("chem_player_choices", label = "Choose Five Players: ", choices = all_players, selected = all_players[[1]], multiple = TRUE),
                     width = 4
                   )
                 ),
                 fluidRow(
                   column(
                     plotOutput("chemHist"),
                     width = 12
                   )
                 )
               )
             ),
             tags$head(tags$style("#chemTable_cap{color: #EA80FC; font-size: 30px; font-style: bold}", "#chemValue{color: #EA80FC; font-size: 30px; font-style: bold}", "#chemText{color: #B8BCC2; font-size: 16px}"))
    )
  )
)

# -----------------------------------------------------------------------------------------------------
# SERVER
server <- function(input, output, session) {
  
  # FIRST TAB: Matchup Analysis
  # Observing Action From First Input
  team_1 <- reactive(input$team_choice_1)
  
  # Updating input options for Second Input based on First Input
  observe({
    team_2_vals <- all_teams[all_teams != team_1()]
    
    updateSelectInput(
      session = session, 
      inputId = "team_choice_2",
      choices = team_2_vals,
      selected = head(team_2_vals, 1)
    )
  })
  team_2 <- reactive(input$team_choice_2)
  
  lineup_1_exists <- FALSE
  lineup_2_exists <<- FALSE
  
  observe({
    team_1_filtered <- unique(data.frame(players = unlist(df[which(df$TEAM_ABBREVIATION == team_1()), which(colnames(df) %in% c('P1','P2','P3','P4','P5'))])))
    
    updateSelectInput(
      session = session, 
      inputId = "players_team_1",
      choices = team_1_filtered
    )
  })
  
  team_1_lineup <- reactive(input$players_team_1)
  
  observe({
    team_2_filtered <- unique(data.frame(players = unlist(df[which(df$TEAM_ABBREVIATION == team_2()), which(colnames(df) %in% c('P1','P2','P3','P4','P5'))])))
    
    updateSelectInput(
      session = session, 
      inputId = "players_team_2",
      choices = team_2_filtered
    )
  })
  
  team_2_lineup <- reactive(input$players_team_2)
  
  output$table1_cap <- renderText({  
    table1_cap <- paste0(teams_full$TEAM_FULL[which(teams_full$TEAM_ABBREVIATION == team_1())], ' Lineup')
    table1_cap
  })
  
  output$table2_cap <- renderText({
    table2_cap <- paste0(teams_full$TEAM_FULL[which(teams_full$TEAM_ABBREVIATION == team_2())], ' Lineup')
    table2_cap
  })
  
  output$tableA <- renderDataTable({
    req(team_1_lineup)
    tableA <- datatable(players %>% 
                          select(HEADSHOT, PLAYER_NAME, AGE, MPG, W_PCT, PLUS_MINUS) %>% 
                          filter(PLAYER_NAME %in% team_1_lineup()) %>% 
                          rename(c(' ' = 'HEADSHOT', 'Player' = 'PLAYER_NAME', 'Age' = 'AGE', 'Win%' = 'W_PCT', 'Min/Game' = 'MPG', '+/-' = 'PLUS_MINUS')), 
                        escape = FALSE, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames= FALSE)
    tableA
  })
  
  output$tableB <- renderDataTable({
    req(team_2_lineup)
    tableB <- datatable(players %>% 
                          select(HEADSHOT, PLAYER_NAME, AGE, MPG, W_PCT, PLUS_MINUS) %>% 
                          filter(PLAYER_NAME %in% team_2_lineup()) %>% 
                          rename(c(' ' = 'HEADSHOT', 'Player' = 'PLAYER_NAME', 'Age' = 'AGE', 'Win%' = 'W_PCT', 'Min/Game' = 'MPG', '+/-' = 'PLUS_MINUS')), 
                        escape = FALSE, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames= FALSE)
    tableB
  })
  
  
  output$noLineup <- renderText({
    req(team_1_lineup)
    req(team_2_lineup)
    
    req(team_1)
    req(team_2)
    
    if (length(team_1_lineup()) == 5 & length(team_2_lineup()) == 5) {
      for (n1 in 1:nrow(df)) {
        if (all(
          team_1_lineup() %in% 
          df[n1, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_1_exists <- TRUE
          final_n1 <- n1
          break
        }
      }
      
      for (n2 in 1:nrow(df)) {
        if (all(
          team_2_lineup() %in% 
          df[n2, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_2_exists <- TRUE
          final_n2 <- n2
          break
        }
      }
      
      
      if (lineup_1_exists == FALSE & lineup_2_exists == FALSE) {
        paste0("Lineup data not available for ", team_1(), " & ", team_2(), ". Please select another lineup.")
      }
      else if (lineup_1_exists == FALSE & lineup_2_exists == TRUE) {
        paste0("Lineup data not available for ", team_1(), ". Please select another lineup.")
      }
      else if (lineup_1_exists == TRUE & lineup_2_exists == FALSE) {
        paste0("Lineup data not available for ", team_2(), ". Please select another lineup.")
      }
      else {
        print(' ')
      }
    }
    else{
      print("Please select five players for each lineup.")
    }
    
  })
  
  output$analysis_val1 <- renderText({
    req(team_1_lineup)
    req(team_2_lineup)
    
    req(team_1)
    req(team_2)
    
    if (length(team_1_lineup()) == 5 & length(team_2_lineup()) == 5) {
      for (n1 in 1:nrow(df)) {
        if (all(
          team_1_lineup() %in% 
          df[n1, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_1_exists <- TRUE
          final_n1 <- n1
          break
        }
      }
      
      for (n2 in 1:nrow(df)) {
        if (all(
          team_2_lineup() %in% 
          df[n2, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_2_exists <- TRUE
          final_n2 <- n2
          break
        }
      }
      
      if (lineup_1_exists == TRUE & lineup_2_exists == TRUE) {
        plusMinus1 <- df$PLUS_MINUS_STAN[final_n1]
        plusMinus2 <- df$PLUS_MINUS_STAN[final_n2]
        games1 <- (df$MIN[final_n1])/48
        games2 <- (df$MIN[final_n2])/48
        sigma <- sqrt((144/games1) + (144/games2))
        prob <- pnorm((plusMinus1 - plusMinus2), mean = 0, sd = sigma)
        if (prob != 0.5) {
          paste0(round(prob*100, 1), '%')
        }
        else {
          paste0(' ')
        }
      }
    }
    
  })
  
  output$analysis_val2 <- renderText({
    req(team_1_lineup)
    req(team_2_lineup)
    
    req(team_1)
    req(team_2)
    
    if (length(team_1_lineup()) == 5 & length(team_2_lineup()) == 5) {
      for (n1 in 1:nrow(df)) {
        if (all(
          team_1_lineup() %in% 
          df[n1, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_1_exists <- TRUE
          final_n1 <- n1
          break
        }
      }
      
      for (n2 in 1:nrow(df)) {
        if (all(
          team_2_lineup() %in% 
          df[n2, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_2_exists <- TRUE
          final_n2 <- n2
          break
        }
      }
      
      if (lineup_1_exists == TRUE & lineup_2_exists == TRUE) {
        plusMinus1 <- df$PLUS_MINUS_STAN[final_n1]
        plusMinus2 <- df$PLUS_MINUS_STAN[final_n2]
        games1 <- (df$MIN[final_n1])/48
        games2 <- (df$MIN[final_n2])/48
        sigma <- sqrt((144/games1) + (144/games2))
        prob <- pnorm((plusMinus1 - plusMinus2), mean = 0, sd = sigma)
        if (prob > 0.5) {
          paste0(round((plusMinus1 - plusMinus2),1)) 
        }
        else if (prob < 0.5) {
          paste0(round((plusMinus2 - plusMinus1),1)) 
        }
        else {
          paste0(' ')
        }
      }
    }
    
  })
  
  output$analysis_text1 <- renderText({
    req(team_1_lineup)
    req(team_2_lineup)
    
    req(team_1)
    req(team_2)
    
    if (length(team_1_lineup()) == 5 & length(team_2_lineup()) == 5) {
      for (n1 in 1:nrow(df)) {
        if (all(
          team_1_lineup() %in% 
          df[n1, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_1_exists <- TRUE
          final_n1 <- n1
          break
        }
      }
      
      for (n2 in 1:nrow(df)) {
        if (all(
          team_2_lineup() %in% 
          df[n2, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_2_exists <- TRUE
          final_n2 <- n2
          break
        }
      }
      
      if (lineup_1_exists == TRUE & lineup_2_exists == TRUE) {
        plusMinus1 <- df$PLUS_MINUS_STAN[final_n1]
        plusMinus2 <- df$PLUS_MINUS_STAN[final_n2]
        games1 <- (df$MIN[final_n1])/48
        games2 <- (df$MIN[final_n2])/48
        sigma <- sqrt((144/games1) + (144/games2))
        prob <- pnorm((plusMinus1 - plusMinus2), mean = 0, sd = sigma)
        if (prob > 0.5) {
          paste0("Chance that ", team_1(), " has a better lineup than ", team_2())
        }
        else if (prob < 0.5) {
          paste0("Chance that ", team_1(), " has a better lineup than ", team_2())
        }
        else if (prob == 0.5) {
          paste(final_n1, final_n2)
          
        }
      }
    }
    
  })
  
  output$analysis_text2 <- renderText({
    req(team_1_lineup)
    req(team_2_lineup)
    
    req(team_1)
    req(team_2)
    
    if (length(team_1_lineup()) == 5 & length(team_2_lineup()) == 5) {
      for (n1 in 1:nrow(df)) {
        if (all(
          team_1_lineup() %in% 
          df[n1, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_1_exists <- TRUE
          final_n1 <- n1
          break
        }
      }
      
      for (n2 in 1:nrow(df)) {
        if (all(
          team_2_lineup() %in% 
          df[n2, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          lineup_2_exists <- TRUE
          final_n2 <- n2
          break
        }
      }
      
      if (lineup_1_exists == TRUE & lineup_2_exists == TRUE) {
        plusMinus1 <- df$PLUS_MINUS_STAN[final_n1]
        plusMinus2 <- df$PLUS_MINUS_STAN[final_n2]
        games1 <- (df$MIN[final_n1])/48
        games2 <- (df$MIN[final_n2])/48
        sigma <- sqrt((144/games1) + (144/games2))
        prob <- pnorm((plusMinus1 - plusMinus2), mean = 0, sd = sigma)
        if (prob > 0.5) {
          paste0("Points per 48 minutes ", team_1(), " is expected to outscore ", team_2(), " by while both lineups are on the court")
        }
        else if (prob < 0.5) {
          paste0("Points per 48 minutes ", team_2(), " is expected to outscore ", team_1(), " by while both lineups are on the court")
        }
        else if (prob == 0.5) {
          paste0("The Selected Lineups have Equal Scoring Ability.")
        }
      }
    }
    
  })
  
  # ---------------------------------------------------------------------------------------------------
  # TAB 2: Individual Analysis
  
  indTeam <- reactive(input$ind_team_choice)
  
  output$indTable <- renderDataTable({
    req(indTeam) 
    
    indTable_df <<- players %>% 
      filter(TEAM_ABBREVIATION == indTeam()) %>% 
      select(HEADSHOT, PLAYER_NAME, AGE, MPG, W_PCT, PLUS_MINUS, N_LINEUPS) %>% 
      arrange(desc(N_LINEUPS)) %>% 
      rename(c(' ' = 'HEADSHOT', 'Player' = 'PLAYER_NAME', 'Age' = 'AGE', 'Win%' = 'W_PCT', 'Min/Game' = 'MPG', '+/-' = 'PLUS_MINUS', 'Number of Lineups' = 'N_LINEUPS')) %>% 
      top_n(5)
    
    indTable <- datatable(indTable_df, 
                          escape = FALSE, options = list(lengthChange = FALSE, searching = FALSE), rownames= FALSE)
    indTable
  })
  
  output$indTable_cap <- renderText({
    indTable_cap <- paste0('Top Players for ', teams_full$TEAM_FULL[which(teams_full$TEAM_ABBREVIATION == indTeam())], ', by Number of Participating Lineups')
    indTable_cap
  })
  
  output$leaderboard <- renderReactable({
    
    req(indTeam)
    
    reactable_df <- players %>%
      filter(TEAM_ABBREVIATION == indTeam()) %>%
      select(PLAYER_NAME, HEADSHOT_REAC, PPG, FG_PCT, FT_PCT, OREB, DREB, AST, TOV, STL, BLK, PTS, TEAM_COLOR)
    
    reactable(
      reactable_df,
      defaultPageSize = 5,
      theme=fivethirtyeight(font_size=12,header_font_color="#B8BCC2",font_color="#B8BCC2",cell_padding=25),
      defaultColDef = colDef(headerStyle = list(background = "#202123", color = "#B8BCC2"),
                             footerStyle = list(background = "#202123", color = "#B8BCC2")),
      columns = list(
        HEADSHOT_REAC = colDef(show = FALSE),
        TEAM_COLOR = colDef(show = FALSE),
        PLAYER_NAME = colDef(name = "Player",align = "center", width = 150, style = cell_style(font_color = "#B8BCC2",
                                                                                               font_size = 12,
                                                                                               background_color = "#202123")),
        PPG = colDef(name = 'Points per Game', align = "center",
                     style = cell_style(background_color = "#202123"),
                     cell = data_bars(reactable_df, 
                                      fill_color_ref = "TEAM_COLOR",
                                      fill_opacity = 0.3,
                                      brighten_text = FALSE, 
                                      text_position = "above",
                                      img_ref = "HEADSHOT_REAC",
                                      text_color = "#B8BCC2",
                                      img_height = 30, 
                                      img_width = 30)
        ),
        FG_PCT = colDef(name = 'Field Goal %', align = "center",
                        style = cell_style(background_color = "#202123"),
                        cell = data_bars(reactable_df, 
                                         fill_color_ref = "TEAM_COLOR",
                                         fill_opacity = 0.3,
                                         brighten_text = FALSE, 
                                         text_position = "above",
                                         number_fmt = scales::percent_format(accuracy = .1L),
                                         img_ref = "HEADSHOT_REAC",
                                         text_color = "#B8BCC2",
                                         img_height = 30, 
                                         img_width = 30)
        ),
        FT_PCT = colDef(name = 'Free Throw %', align = "center",
                        style = cell_style(background_color = "#202123"),
                        cell = data_bars(reactable_df, 
                                         fill_color_ref = "TEAM_COLOR",
                                         fill_opacity = 0.3,
                                         brighten_text = FALSE, 
                                         text_position = "above",
                                         number_fmt = scales::percent_format(accuracy = .1L),
                                         img_ref = "HEADSHOT_REAC",
                                         text_color = "#B8BCC2",
                                         img_height = 30, 
                                         img_width = 30)
        ),
        OREB = colDef(name = 'Offensive Rebounds', align = "center",
                      style = cell_style(background_color = "#202123"),
                      cell = data_bars(reactable_df, 
                                       fill_color_ref = "TEAM_COLOR",
                                       fill_opacity = 0.3,
                                       brighten_text = FALSE, 
                                       text_position = "above",
                                       img_ref = "HEADSHOT_REAC",
                                       text_color = "#B8BCC2",
                                       img_height = 30, 
                                       img_width = 30)
        ),
        DREB = colDef(name = 'Defensive Rebounds', align = "center",
                      style = cell_style(background_color = "#202123"),
                      cell = data_bars(reactable_df, 
                                       fill_color_ref = "TEAM_COLOR",
                                       fill_opacity = 0.3,
                                       brighten_text = FALSE, 
                                       text_position = "above",
                                       img_ref = "HEADSHOT_REAC",
                                       text_color = "#B8BCC2",
                                       img_height = 30, 
                                       img_width = 30)
        ),
        AST = colDef(name = 'Assists', align = "center",
                     style = cell_style(background_color = "#202123"),
                     cell = data_bars(reactable_df, 
                                      fill_color_ref = "TEAM_COLOR",
                                      fill_opacity = 0.3,
                                      brighten_text = FALSE, 
                                      text_position = "above",
                                      img_ref = "HEADSHOT_REAC",
                                      text_color = "#B8BCC2",
                                      img_height = 30, 
                                      img_width = 30)
        ),
        TOV = colDef(name = 'Turnovers', align = "center",
                     style = cell_style(background_color = "#202123"),
                     cell = data_bars(reactable_df, 
                                      fill_color_ref = "TEAM_COLOR",
                                      fill_opacity = 0.3,
                                      brighten_text = FALSE, 
                                      text_position = "above",
                                      img_ref = "HEADSHOT_REAC",
                                      text_color = "#B8BCC2",
                                      img_height = 30, 
                                      img_width = 30)
        ),
        `STL` = colDef(name = 'Steals', align = "center",
                       style = cell_style(background_color = "#202123"),
                       cell = data_bars(reactable_df, 
                                        fill_color_ref = "TEAM_COLOR",
                                        fill_opacity = 0.3,
                                        brighten_text = FALSE, 
                                        text_position = "above",
                                        img_ref = "HEADSHOT_REAC",
                                        text_color = "#B8BCC2",
                                        img_height = 30, 
                                        img_width = 30)
        ),
        BLK = colDef(name = 'Blocks', align = "center",
                     style = cell_style(background_color = "#202123"),
                     cell = data_bars(reactable_df, 
                                      fill_color_ref = "TEAM_COLOR",
                                      fill_opacity = 0.3,
                                      brighten_text = FALSE, 
                                      text_position = "above",
                                      img_ref = "HEADSHOT_REAC",
                                      text_color = "#B8BCC2",
                                      img_height = 30, 
                                      img_width = 30)
        ),
        PTS = colDef(name = 'Total Points', align = "center",
                     style = cell_style(background_color = "#202123"),
                     cell = data_bars(reactable_df, 
                                      fill_color_ref = "TEAM_COLOR",
                                      fill_opacity = 0.3,
                                      brighten_text = FALSE, 
                                      text_position = "above",
                                      img_ref = "HEADSHOT_REAC",
                                      text_color = "#B8BCC2",
                                      img_height = 30, 
                                      img_width = 30)
        )
      )
    )
  })
  
  output$leaderboard_cap <- renderText({
    leaderboard_cap <- paste0(teams_full$TEAM_FULL[which(teams_full$TEAM_ABBREVIATION == indTeam())], ' Player Leaderboard')
    leaderboard_cap
  })
  
  # ---------------------------------------------------------------------------------------------------
  # TAB 3: Chemistry Analysis
  
  chem_team <- reactive(input$chem_choice)
  
  observe({
    filtered_players_chem <- unique(data.frame(players = unlist(df[which(df$TEAM_ABBREVIATION == chem_team()), which(colnames(df) %in% c('P1','P2','P3','P4','P5'))])))
    
    updateSelectInput(
      session = session, 
      inputId = "chem_player_choices",
      choices = filtered_players_chem
    )
  })
  
  players_chem <- reactive(input$chem_player_choices)
  
  chem_lineup_exists <- FALSE
  
  output$chemTable_cap <- renderText({
    chemTable_cap <- paste0(teams_full$TEAM_FULL[which(teams_full$TEAM_ABBREVIATION == chem_team())], ' Lineup')
    chemTable_cap
  })
  
  output$chemTable <- renderDataTable({
    req(players_chem)
    chemTable <- datatable(players %>% 
                             select(HEADSHOT, PLAYER_NAME, AGE, MPG, W_PCT, PLUS_MINUS) %>% 
                             filter(PLAYER_NAME %in% players_chem()) %>% 
                             rename(c(' ' = 'HEADSHOT', 'Player' = 'PLAYER_NAME', 'Age' = 'AGE', 'Win%' = 'W_PCT', 'Min/Game' = 'MPG', '+/-' = 'PLUS_MINUS')), 
                           escape = FALSE, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), rownames= FALSE)
    chemTable
  })
  
  lineup_chem <- 0
  
  output$chemValue <- renderText({
    req(players_chem)
    
    if (length(players_chem()) == 5) {
      for (n in 1:nrow(df)) {
        if (all(
          players_chem() %in% 
          df[n, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          chem_lineup_exists <- TRUE
          break
        }
      }
      
      req(chem_team)
      
      team_chemistry <<- df$TEAM_CHEMISTRY[n]
      
      if (!chem_lineup_exists) {
        chemValue <- ' '
        chemValue
      }
      else {
        chemValue <- ifelse(team_chemistry >= 0, paste0('+', round(team_chemistry, 1)), round(team_chemistry, 1))
        chemValue
      }
    }
    else {
      chemValue <- ' '
      chemValue
    }
    
  })
  
  output$chemText <- renderText({
    req(players_chem)
    
    if (length(players_chem()) == 5) {
      for (n in 1:nrow(df)) {
        if (all(
          players_chem() %in% 
          df[n, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          chem_lineup_exists <- TRUE
          break
        }
      }
      
      req(chem_team)
      
      team_chemistry <<- df$TEAM_CHEMISTRY[n]
      
      if (!chem_lineup_exists) {
        chemText <- paste0("Lineup data not available for ", chem_team(), ". Please select another lineup.")
        chemText
      }
      else {
        chemText <- paste0("Lineup Chemistry Value for ", chem_team(), ':')
        chemText
      }
    }
    else if (length(players_chem()) > 5 | length(players_chem()) < 5) {
      chemText <- "Please select a five-player lineup."
      chemText
    }
    
  })
  
  output$chemHist <- renderPlot({
    req(players_chem)
    
    if (length(players_chem()) == 5) {
      for (n in 1:nrow(df)) {
        if (all(
          players_chem() %in% 
          df[n, colnames(df) %in% c('P1', 'P2', 'P3', 'P4', 'P5')])) {
          chem_lineup_exists <- TRUE
          lineup_chem <- df$TEAM_CHEMISTRY[n]
          break
        }
      }
    }
    
    req(chem_team)
    
    par(bg = '#202123', lwd=3)
    hist(df$TEAM_CHEMISTRY,
         xlim = c(-40,40),
         col = '#202123',
         border = '#B8BCC2',
         family = 'Andale Mono',
         main = 'Distribution Plot of Team Chemistry Differential',
         xlab = 'Team Chemistry Value',
         col.axis = "#202123",
         col.lab = "#B8BCC2",
         col.main = "#B8BCC2"
    )
    axis(1,col="#B8BCC2", col.axis = "#B8BCC2", family = 'Andale Mono')
    axis(2,col="#B8BCC2", col.axis = "#B8BCC2", family = 'Andale Mono')
    
    if (chem_lineup_exists) {
      abline(v = lineup_chem, col='#EA80FC', lty = 2)
      op <- par(family = "Andale Mono")
      legend("topright", legend = c(paste0(chem_team(), ', \n', 
                                           round(pnorm(q=lineup_chem,mean=mean(df$TEAM_CHEMISTRY),sd=sd(df$TEAM_CHEMISTRY))*100,0), 'th Percentile \n')), 
             col = c('#EA80FC'), box.lwd = 1, text.col = "#B8BCC2",  box.col = "#B8BCC2", bg = "#202123", lty = 2)
    }
    
  })
  
}
shinyApp(ui, server)