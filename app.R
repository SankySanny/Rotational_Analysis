#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#---------Just upload a .dvw file to see the app content---------
#
#    https://shiny.posit.co/
#

library(shiny)
library(datavolley)
library(dplyr)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #1e1e2f; font-family: 'Inter', sans-serif; color: #ffffff; overflow-x: hidden; position: relative; min-height: 100vh; }
      .dash-header { 
        background: #27293d; padding: 15px 30px; margin-bottom: 20px; 
        border-bottom: 2px solid #b21f1f; display: flex; justify-content: space-between; align-items: center; gap: 20px;
      }
      .score-container { display: flex; align-items: center; gap: 30px; text-align: center; }
      .team-score-group { display: flex; flex-direction: column; }
      .header-team-name { font-size: 0.9em; font-weight: 700; text-transform: uppercase; color: #8e8e8e; letter-spacing: 1px; }
      .score-display { font-size: 2.5em; font-weight: 900; color: #e14eca; line-height: 1; margin-top: 5px; }
      .score-separator { font-size: 2em; font-weight: 300; color: #3d3f51; padding-top: 20px; }
      .team-column { padding: 10px; height: 82vh; overflow-y: auto; }
      .team-pane { background: #27293d; padding: 20px; border-radius: 12px; height: 100%; border: 1px solid #3d3f51; }
      .tactical-box { background: #1d1d2b; padding: 12px; border-radius: 8px; margin-bottom: 15px; border-left: 5px solid #00f2c3; }
      .peak-text { color: #00f2c3; font-weight: 800; }
      .low-text { color: #ff8d72; font-weight: 800; }
      .mvp-badge { float: right; background: linear-gradient(135deg, #f5d020 0%, #f59831 100%); color: #1e1e2f; padding: 5px 15px; border-radius: 20px; font-weight: 800; font-size: 0.8em; }
      .table { color: #ffffff !important; font-size: 0.72em; margin-bottom: 20px; }
      .table thead th { background: #3d3f51 !important; color: #ffffff !important; border: none !important; text-transform: uppercase; padding: 6px !important; }
      .table td { border-top: 1px solid #3d3f51 !important; vertical-align: middle !important; padding: 6px !important; }
      .table tbody tr:last-child { background: #b21f1f !important; font-weight: 800; }
      .rot-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 10px; }
      .rot-box { background: #1d1d2b; padding: 10px; border-radius: 6px; text-align: center; border-bottom: 3px solid #b21f1f; }
      .rot-label { font-size: 0.7em; color: #8e8e8e; font-weight: 700; }
      .rot-player { display: block; font-size: 0.85em; font-weight: 700; margin: 3px 0; color: #e14eca; }
      .filter-panel { display: flex; gap: 15px; align-items: center; }
      .analyst-footer { position: fixed; bottom: 10px; right: 20px; font-size: 0.7em; color: #6c757d; text-align: right; font-style: italic; }
    "))
  ),
  
  div(class = "dash-header",
      div(h2("Match Rotational Analysis", style="margin:0; font-weight:800; color:#ffffff;")),
      uiOutput("top_scoreboard"),
      div(class = "filter-panel",
          selectInput("set_filter", NULL, choices = c("Full Match" = 0), width = "120px"),
          div(style="width:200px", fileInput("file1", NULL, accept = ".dvw", buttonLabel = "Upload Data", placeholder = "No file..."))
      )
  ),
  
  fluidRow(style = "margin: 0 15px;",
           column(width = 6, class = "team-column",
                  div(class = "team-pane",
                      uiOutput("home_header"),
                      uiOutput("home_tactical"),
                      tableOutput("home_table"),
                      h5("ROTATION TOP SCORERS", style="color:#8e8e8e; font-weight:700; margin-top:20px;"),
                      uiOutput("home_tiles")
                  )
           ),
           column(width = 6, class = "team-column",
                  div(class = "team-pane",
                      uiOutput("away_header"),
                      uiOutput("away_tactical"),
                      tableOutput("away_table"),
                      h5("ROTATION TOP SCORERS", style="color:#8e8e8e; font-weight:700; margin-top:20px;"),
                      uiOutput("away_tiles")
                  )
           )
  ),
  
  div(class = "analyst-footer", p(style="margin:0;", "Created by: Sanky Sanny"), p(style="margin:0;", "Performance Analyst"))
)

server <- function(input, output, session) {
  
  v_data <- reactive({
    req(input$file1)
    dv <- dv_read(input$file1$datapath)
    updateSelectInput(session, "set_filter", choices = c("Full Match" = 0, setNames(1:max(dv$plays$set_number, na.rm=T), paste("Set", 1:max(dv$plays$set_number, na.rm=T)))))
    dv
  })
  
  filtered_plays <- reactive({
    req(v_data())
    if(input$set_filter == 0) return(v_data()$plays)
    v_data()$plays %>% filter(set_number == as.numeric(input$set_filter))
  })
#----------American Rotation vs International------------  
  map_rotation <- function(z) {
    case_when(z == 1 ~ "ROT 1", z == 6 ~ "ROT 2", z == 5 ~ "ROT 3",
              z == 4 ~ "ROT 4", z == 3 ~ "ROT 5", z == 2 ~ "ROT 6", TRUE ~ NA_character_)
  }
#-----This is how the MVP badge for each team is calculated---------  
  get_mvp <- function(is_home) {
    dvw <- v_data(); plays <- filtered_plays(); teams <- dvw$meta$teams
    target <- if(is_home) teams$team[teams$home_away_team == "*"] else teams$team[teams$home_away_team == "a"]
    leader <- plays %>% filter(team == target & evaluation_code == "#" & skill %in% c("Attack", "Block", "Serve")) %>%
      group_by(player_name) %>% summarize(pts = n()) %>% slice_max(pts, n = 1, with_ties = FALSE)
    if(nrow(leader) > 0) return(paste0(leader$player_name, " (", leader$pts, " P)"))
    return("N/A")
  }
  
  calculate_rotational_stats <- function(is_home = TRUE, return_raw = FALSE) {
    dvw <- v_data(); plays <- filtered_plays(); meta_teams <- dvw$meta$teams
    target_team <- if(is_home) meta_teams$team[meta_teams$home_away_team == "*"] else meta_teams$team[meta_teams$home_away_team == "a"]
    opp_team <- if(is_home) meta_teams$team[meta_teams$home_away_team == "a"] else meta_teams$team[meta_teams$home_away_team == "*"]
    setter_col <- if(is_home) "home_setter_position" else "visiting_setter_position"
#-------Basic Stats calculations-------------    
    rot_stats <- plays %>% rename(Zone = !!sym(setter_col)) %>% filter(!is.na(Zone)) %>%
      mutate(RotLabel = factor(map_rotation(Zone), levels = paste("ROT", 1:6))) %>%
      group_by(Rotation = RotLabel) %>%
      summarize(Atts = sum(skill == "Attack" & team == target_team, na.rm = TRUE),
                Kills = sum(skill == "Attack" & team == target_team & evaluation_code == "#", na.rm = TRUE),
                Errors = sum(skill == "Attack" & team == target_team & evaluation_code == "=", na.rm = TRUE),
                Blocked = sum(skill == "Attack" & team == target_team & evaluation_code == "/", na.rm = TRUE),
                FB_Atts = sum(skill == "Attack" & team == target_team & phase == "Reception", na.rm = TRUE),
                FB_K = sum(skill == "Attack" & team == target_team & phase == "Reception" & evaluation_code == "#", na.rm = TRUE),
                FB_E = sum(skill == "Attack" & team == target_team & phase == "Reception" & evaluation_code %in% c("=", "/"), na.rm = TRUE),
                TR_Atts = sum(skill == "Attack" & team == target_team & phase == "Transition", na.rm = TRUE),
                TR_K = sum(skill == "Attack" & team == target_team & phase == "Transition" & evaluation_code == "#", na.rm = TRUE),
                TR_E = sum(skill == "Attack" & team == target_team & phase == "Transition" & evaluation_code %in% c("=", "/"), na.rm = TRUE),
                Receptions = sum(skill == "Reception" & team == target_team, na.rm = TRUE),
                Pos_Pass = sum(skill == "Reception" & team == target_team & evaluation_code %in% c("#", "+"), na.rm = TRUE),
                Pos_Pass_Sideouts = sum(skill == "Reception" & team == target_team & evaluation_code %in% c("#", "+") & point_won_by == target_team, na.rm = TRUE),
                OppServes = sum(skill == "Serve" & team == opp_team, na.rm = TRUE),
                SOWon = sum(skill == "Serve" & team == opp_team & point_won_by == target_team, na.rm = TRUE),
                OurServes = sum(skill == "Serve" & team == target_team, na.rm = TRUE),
                BPWon = sum(skill == "Serve" & team == target_team & point_won_by == target_team, na.rm = TRUE))
    
    if(return_raw) return(rot_stats)
    totals <- rot_stats %>% summarize(Rotation = "TOTAL", across(Atts:BPWon, sum))
    bind_rows(rot_stats, totals) %>%
      mutate(`SO%` = paste0(round((SOWon / pmax(OppServes, 1)) * 100), "%"),
             `Kill%` = paste0(round((Kills / pmax(Atts, 1)) * 100), "%"),
             `Err%` = paste0(round((Errors / pmax(Atts, 1)) * 100), "%"),
             `Blk%` = paste0(round((Blocked / pmax(Atts, 1)) * 100), "%"),
             `Eff` = sprintf("%.3f", (Kills - Errors - Blocked) / pmax(Atts, 1)),
             `FB-Eff` = sprintf("%.3f", (FB_K - FB_E) / pmax(FB_Atts, 1)),
             `Tr-Eff` = sprintf("%.3f", (TR_K - TR_E) / pmax(TR_Atts, 1)),
             `Pos%` = paste0(round((Pos_Pass / pmax(Receptions, 1)) * 100), "%"),
             `Pos-SO%` = paste0(round((Pos_Pass_Sideouts / pmax(Pos_Pass, 1)) * 100), "%"),
             `BP%` = paste0(round((BPWon / pmax(OurServes, 1)) * 100), "%")) %>%
      select(Rotation, `SO%`, `Kill%`, `Err%`, `Blk%`, `Eff`, `FB-Eff`, `Tr-Eff`, `Pos%`, `Pos-SO%`, `BP%`)
  }
  
  output$top_scoreboard <- renderUI({
    req(v_data())
    teams <- v_data()$meta$teams
    home_name <- teams$team[teams$home_away_team == "*"]
    away_name <- teams$team[teams$home_away_team == "a"]
    
    if (input$set_filter == 0) {
      # FULL MATCH: Count Sets Won
      set_results <- v_data()$plays %>%
        filter(!is.na(point_won_by)) %>%
        group_by(set_number) %>%
        filter(row_number() == n()) %>%
        ungroup()
      
      home_score <- sum(set_results$point_won_by == home_name, na.rm = TRUE)
      away_score <- sum(set_results$point_won_by == away_name, na.rm = TRUE)
    } else {
      # SPECIFIC SET: Count Rallies Won
      score_data <- filtered_plays() %>% 
        filter(!is.na(point_won_by)) %>%
        group_by(point_id) %>%
        filter(row_number() == n()) %>% 
        ungroup()
      
      home_score <- sum(score_data$point_won_by == home_name, na.rm = TRUE)
      away_score <- sum(score_data$point_won_by == away_name, na.rm = TRUE)
    }
    
    div(class = "score-container",
        div(class = "team-score-group", span(class = "header-team-name", home_name), span(class = "score-display", home_score)),
        span(class = "score-separator", "VS"),
        div(class = "team-score-group", span(class = "header-team-name", away_name), span(class = "score-display", away_score))
    )
  })
  
  output$home_header <- renderUI({ div(span(class="mvp-badge", get_mvp(TRUE)), h3(v_data()$meta$teams$team[v_data()$meta$teams$home_away_team == "*"], style="margin:0; font-weight:800;")) })
  output$away_header <- renderUI({ div(span(class="mvp-badge", get_mvp(FALSE)), h3(v_data()$meta$teams$team[v_data()$meta$teams$home_away_team == "a"], style="margin:0; font-weight:800;")) })
#----------Peak Rotation logic calculation------calculating based on attack eff not sideo out-----------  
  render_tactical <- function(is_home) {
    raw <- calculate_rotational_stats(is_home, TRUE); f <- raw %>% filter(Atts >= 1)
    if(nrow(f)==0) return(div(class="tactical-box", "Gathering data..."))
    best <- (f %>% arrange(desc((Kills-Errors-Blocked)/Atts)))$Rotation[1]
    worst <- (f %>% arrange((Kills-Errors-Blocked)/Atts))$Rotation[1]
    div(class="tactical-box", span(class="peak-text", "PEAK: "), best, span(style="margin-left:15px;", span(class="low-text", "LOW: "), worst))
  }
  output$home_tactical <- renderUI({ render_tactical(TRUE) })
  output$away_tactical <- renderUI({ render_tactical(FALSE) })
  
  render_tiles <- function(is_home) {
    dvw <- v_data(); plays <- filtered_plays(); target <- if(is_home) dvw$meta$teams$team[dvw$meta$teams$home_away_team == "*"] else dvw$meta$teams$team[dvw$meta$teams$home_away_team == "a"]
    setter_col <- if(is_home) "home_setter_position" else "visiting_setter_position"
    impact <- plays %>% filter(team == target & evaluation_code == "#" & skill %in% c("Attack", "Block", "Serve")) %>%
      rename(Zone = !!sym(setter_col)) %>% filter(!is.na(Zone)) %>%
      mutate(Rotation = factor(map_rotation(Zone), levels = paste("ROT", 1:6))) %>%
      group_by(Rotation, player_name) %>% summarize(pts = n(), .groups = "drop") %>%
      group_by(Rotation) %>% slice_max(pts, n = 1, with_ties = FALSE)
#--------top point scorer by rotation and the court layout based on actual zones on the court----------    
    court_order <- c("ROT 4", "ROT 5", "ROT 6", "ROT 3", "ROT 2", "ROT 1")
    
    div(class="rot-grid", lapply(court_order, function(r) {
      d <- impact %>% filter(Rotation == r)
      div(class="rot-box", div(class="rot-label", r), if(nrow(d)>0) tagList(span(class="rot-player", d$player_name), span(class="rot-pts", d$pts)) else span(class="rot-player", "-"))
    }))
  }
  output$home_tiles <- renderUI({ render_tiles(TRUE) })
  output$away_tiles <- renderUI({ render_tiles(FALSE) })
  
  output$home_table <- renderTable({ calculate_rotational_stats(TRUE) }, align = 'c', class = "table")
  output$away_table <- renderTable({ calculate_rotational_stats(FALSE) }, align = 'c', class = "table")
}

shinyApp(ui = ui, server = server)
