library(shiny)
library(dplyr)
library(plotly)
library(shinyWidgets)
# import utility functions from within the same directory
source("DBUtils.R")

quick_stats_data <-get_quick_stats_data()
offense_dat <- get_offensive_data()
defense_data <- get_defensive_data()


# this is the user interface part of the app
ui <- fluidPage(
  
  skin = "blue",
  
  titlePanel("College Football Stats"),
  
  # Sidebar
  sidebarLayout(
    
    # Sidebar Panel - include user inputs in here
    sidebarPanel(
      
      # Select a team using an input box - https://shiny.rstudio.com/reference/shiny/0.12.2/selectInput.html
      selectInput("select_choice_A", "Select Home Team", choices = c("All", sort(offense_dat$Name)), selected = "Alabama"), 
      selectInput("select_choice_B", "Select Away Team", choices = c("All", sort(offense_dat$Name)), selected = "LSU")
    ),
    
    # Main Panel - include outputs such as tables and plots in here
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  # add a tab for offensive data
                  # br() represents a line break
                  tabPanel("Quick Stats", br(), br(), reactableOutput("quick_stats_table"), br(),reactableOutput("quick_stats_table_b")),
                  tabPanel("Turnover Data", br(), br(), reactableOutput("defense_data_table"),br(),reactableOutput("defense_data_table_b")),
                  tabPanel("Offense Data", br(),br(), reactableOutput("offense_data_table"),br(), reactableOutput("offense_data_table_b")),
                  tabPanel("Players",br(),br(),"Home Team",br(),reactableOutput("players_home_data_table"), br(),"Away Team",br(), reactableOutput("players_road_data_table"))
      )
      
    )
  )
  )
  
# this is the backend part of the app
server <- function(input, output, session) {
  filtered_quick_stats_data <- reactiveVal(quick_stats_data)
  filtered_offensive_data <- reactiveVal(offense_dat)
  filtered_defensive_data <- reactiveVal(defense_data)
  filtered_quick_stats_data_b <- reactiveVal(quick_stats_data)
  filtered_offensive_data_b <- reactiveVal(offense_dat)
  filtered_defensive_data_b <- reactiveVal(defense_data)
  players_home_reactive_data <- reactiveVal()
  players_road_reactive_data <- reactiveVal()
  
  # get filtered offensive data inside a reactive environment -> recomputes each time inputs are changed
  observe({
    # filter the data by team - use input$select_choice_A (select_choice_A here is the first argument inside selectInput (*))
    if (input$select_choice_A == "All") {
      filtered_quick_stats_data() # just return the whole data table if all teams
      filtered_offensive_data(offense_dat)  # just return the whole data table if all teams
      filtered_defensive_data(defense_data)  # just return the whole data table if all teams
      players_home_reactive_data()
    }
    
    else {  # if not all teams, return all data which has team equal to select_choice
      filtered_quick_stats_data(add_team_columns(quick_stats_data[which(quick_stats_data[,'Name']==input$select_choice_A),], quick_stats_data[which(quick_stats_data[,'Name']==input$select_choice_B),]))
      filtered_offensive_data(offense_dat[which(offense_dat[, "Name"] == input$select_choice_A), ])
      filtered_defensive_data(defense_data[which(defense_data[, "Name"] == input$select_choice_A), ])
      players_home_reactive_data(get_player_data(input$select_choice_A))
    }
  })
  observe({
    if (input$select_choice_B == "All") {
      filtered_quick_stats_data_b() # just return the whole data table if all teams
      filtered_offensive_data_b(offense_dat)  # just return the whole data table if all teams
      filtered_defensive_data_b(defense_data)  # just return the whole data table if all teams
      players_road_reactive_data(player_data)
    }
    
    else {  # if not all teams, return all data which has team equal to select_choice
      filtered_quick_stats_data_b(add_team_columns(quick_stats_data[which(quick_stats_data[,'Name']==input$select_choice_B),], quick_stats_data[which(quick_stats_data[,'Name']==input$select_choice_A),]))
      filtered_offensive_data_b(offense_dat[which(offense_dat[, "Name"] == input$select_choice_B), ])
      filtered_defensive_data_b(defense_data[which(defense_data[, "Name"] == input$select_choice_B), ])
      players_road_reactive_data(get_player_data(input$select_choice_B))
    }
    
  })
  
  # display data
  output$offense_data_table <- renderReactable({
    
    create_offense_table(filtered_offensive_data())
    
  })
  
  
  output$defense_data_table <- renderReactable({
    
    create_defense_table(filtered_defensive_data())
    
  })
  
  output$quick_stats_table <- renderReactable({
    
    create_quick_stats_table(filtered_quick_stats_data())
    
  })
  
  output$quick_stats_table_b <- renderReactable({
    
    create_quick_stats_table(filtered_quick_stats_data_b())
  })
  
  output$offense_data_table_b <- renderReactable({
    
    create_offense_table(filtered_offensive_data_b())
  })
  
  output$defense_data_table_b <- renderReactable({
    
    create_defense_table(filtered_defensive_data_b())
  })
  
  output$players_home_data_table <- renderReactable({
    create_players_table(players_home_reactive_data())
  })
  
  output$players_road_data_table <- renderReactable({
    create_players_table(players_road_reactive_data())
  })
  
}
# Run the application 
shinyApp (ui = ui, server = server)