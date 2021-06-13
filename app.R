library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(shinydashboard)
library(DT)
library(shinyalert)
library(shinythemes)
library(lubridate)
library(readr)
library(xml2)
library(rvest)
library(kableExtra)
library(cluster)
library(factoextra)
library(gridExtra)
library(fmsb)
library(wordcloud)
library(MASS)
library(gganimate)
library(gifski)

# load api helper functions
source("api_wrappers_football.R")
# load clustering functions
source("clustering.R")

select <- dplyr::select

Country.League = c(rep("Premier League", 5), rep("Primera Division", 5), rep("Bundesliga 1", 5))
Season = rep(c(2020, 2019, 2018, 2017, 2016), 3)
id = c(2790, 524, 2, 37, 56, 2833, 775, 87, 30, 64, 2755, 754, 8, 35, 54)

league_id_match = data.frame(Country.League, Season, id)

# user interface
ui <- fluidPage(theme = shinytheme("sandstone"),
                useShinyalert(),
                shinyjs::useShinyjs(),
                navbarPage(title = "Football Center", id = "inTabset",
                           # Panel1
                           tabPanel("Recent Fixtures", value = "panel1",
                                    fluidRow(
                                      column(12,
                                             h5(textOutput("currentTime", container = span))
                                             ),
                                      column(9,
                                             uiOutput("title", align = "left")),
                                      column(3,
                                             uiOutput("leage_logo")
                                             ),
                                      column(12, hr()),
                                      column(12,
                                             h3("Now")),
                                      column(12,
                                             uiOutput("test"), align = "center")
                                      ),
                                    DT::dataTableOutput("fixture_now"),
                                    fluidRow(
                                      column(12, hr()),
                                      column(12,
                                             h3("Last"))
                                      ),
                                    DT::dataTableOutput("fixture_last"),
                                    fluidRow(
                                      column(12, hr()),
                                      column(12,
                                             h3("Next"))
                                      ),
                                    DT::dataTableOutput("fixture_next"),
                                    fluidRow(
                                      column(12, br()),
                                      column(12, br())
                                    )
                           ),
                           # Panel 2
                           tabPanel(title = "Team and player Info", 
                                    id = "Information", 
                                    value = "panel2", 
                                    tabsetPanel(

                                      tabPanel(width = 3, 
                                               title = "Team Information", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput(inputId = "team_season", 
                                                               label = "Select Season", 
                                                               choices = c("2020-2021", 
                                                                           "2019-2020", 
                                                                           "2018-2019", 
                                                                           "2017-2018", 
                                                                           "2016-2017")), 
                                                   div(align = "right", 
                                                       actionButton(inputId = "get_team_info", 
                                                                    label = "Search Teams")),
                                                   conditionalPanel(condition = "input.get_team_info",
                                                                    hr(style ="border-color: grey"),
                                                                    div(align = "center",
                                                                        # downloadButton
                                                                        downloadButton("downloadData_team", "Download")
                                                                        )
                                                                    )
                                                 ), 
                                                 mainPanel(
                                                   dataTableOutput(outputId = "team_table")
                                                 )
                                               )
                                               ), 

                                      tabPanel(width = 3, 
                                               title = "Player Information", 
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput(inputId = "player_season", 
                                                               label = "Select Season", 
                                                               choices = c("2020-2021", 
                                                                           "2019-2020", 
                                                                           "2018-2019", 
                                                                           "2017-2018", 
                                                                           "2016-2017")), 
                                                   div(align = "right", 
                                                       actionButton(inputId = "set_season", 
                                                                    label = "Select This Season")), 
                                                   hr(), 
                                                   conditionalPanel(condition = "input.set_season", 
                                                                    selectInput(inputId = "player_team", 
                                                                                label = "Select Team", 
                                                                                choices = c("Select Team")), 
                                                                    div(align = "right", 
                                                                        actionButton(inputId = "player_search", 
                                                                                     label = "Search Players")),
                                                                    conditionalPanel(condition = "input.player_search",
                                                                                     hr(style ="border-color: grey"),
                                                                                     div(align = "center",
                                                                                         # downloadButton
                                                                                         downloadButton("downloadData_player", "Download")
                                                                                     )
                                                                    )
                                                                    ), 
                                                   hr(), 
                                                   div(align = "center", uiOutput("team_logo"))
                                                   
                                                 ), 
                                                 mainPanel(
                                                   dataTableOutput(outputId = "player_table")
                                                 )
                                               )
                                               )
                             )
                           ),                           
                           # Panel3
                           tabPanel(title = "Data Visualization",
                                    id = "EDA", 
                                    value="panel3",
                                    tabsetPanel(
                                      tabPanel(title = "Team", 
                                               fluidRow(
                                                 column(width = 4, 
                                                   selectInput(inputId = "teamvis_season", 
                                                               label = "Select Season", 
                                                               choices = c("2019-2020", 
                                                                           "2018-2019", 
                                                                           "2017-2018", 
                                                                           "2016-2017")), 
                                                   div(align = "left", 
                                                       actionButton(inputId = "teamvis_setseason", 
                                                                    label = "Select This Season"))), 
                                                 column(width = 4, 
                                                        conditionalPanel(condition = "input.teamvis_setseason", 
                                                                         selectInput(inputId = "teamvis_team", 
                                                                                     label = "Select Team", 
                                                                                     choices = c("Select Team")), 
                                                                         div(align = "left", 
                                                                             actionButton(inputId = "teamvis_ex", 
                                                                                          label = "Show Team Record"))))
                                                 ), 
                                               hr(), 
                                               fluidRow(
                                                 navlistPanel(widths = c(2, 8), 
                                                   tabPanel(title = "Goals", 
                                                            column(6, plotOutput(outputId = "team_goal_plot")), 
                                                            column(6, plotOutput(outputId = "team_concede_plot"))
                                                   ), 
                                                   tabPanel(title = "Game Results", 
                                                            column(6, plotOutput(outputId = "home_win_plot")), 
                                                            column(6, plotOutput(outputId = "away_win_plot"))
                                                   ), 
                                                   tabPanel(title = "Team Record", 
                                                            plotOutput(outputId = "team_record_plot")
                                                   )
                                                   
                                                 )
                                               )

                                               ),
                                      tabPanel(title = "Top Scorers",
                                               
                                               fluidRow(
                                                 br(),
                                                 column(width = 3,
                                                   selectInput(inputId = "p3_scorers_season", 
                                                               label = "Select Season", 
                                                               choices = c("2020-2021", 
                                                                           "2019-2020", 
                                                                           "2018-2019", 
                                                                           "2017-2018", 
                                                                           "2016-2017"))
                                                   ),
                                                 column(width = 9,
                                                   div(align = "right", 
                                                       actionButton(inputId = "p3_scorers_setseason", 
                                                                    label = "Select This Season"))
                                                 )
                                               ),
                                               hr(),
                                               navlistPanel(
                                                 well = FALSE,
                                                 widths = c(3, 9),
                                                 tabPanel(
                                                   title = 'Word Cloud',
                                                   fluidRow(
                                                     column(width = 6,
                                                       plotOutput('wordcloud', height = '470px', width = '500px')
                                                       ),
                                                     column(width = 12,
                                                            div(align = "center", 
                                                                span("The 'frequency' is based on the total number of goals of each scorer during this season."))
                                                            )
                                                     ),
                                                   br()
                                                   ),
                                                 
                                                 tabPanel(
                                                   title = 'Radar Chart',
                                                   fluidRow(
                                                     br(),
                                                     conditionalPanel(condition = "input.p3_scorers_setseason",
                                                     column(width = 12,                                       
                                                       selectInput(inputId = "best_scorers_radar", 
                                                                   label = "See Which Scorer?",
                                                                   choices = c("Select one scorer")
                                                                   )
                                                       )
                                                       ),
                                                     column(width = 6,
                                                       plotOutput('radar_plot', height = '400px', width = '550px')
                                                       ),
                                                     column(width = 12,
                                                            div(align = "center", 
                                                            span("The maximum value depends on the best performance on"))
                                                            ),
                                                     column(width = 12,
                                                            div(align = "center", 
                                                            span("each dimension during this season.The minimum value is 0."))
                                                            )
                                                     ),
                                                   br()
                                                   )
                                                 )
                                               )
                                      )
                                    ),

                           # Panel4
                           tabPanel("Clustering",
                                    id = "cluster", 
                                    value = "panel4",
                                    tabsetPanel(
                                      tabPanel(title="Team Cluster",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput(inputId = "cluster_team_season", 
                                                               label = "Select Season", 
                                                               choices = c("2020-2021", 
                                                                           "2019-2020", 
                                                                           "2018-2019", 
                                                                           "2017-2018", 
                                                                           "2016-2017")),
                                                   div(align = "left", 
                                                       actionButton(inputId = "cluster_team_setseason", 
                                                                    label = "Select This Season")),
                                                   br(),
                                                   conditionalPanel(condition = "input.cluster_team_setseason",
                                                   selectInput(inputId="team_group",
                                                               label="How Many Team Clusters?",
                                                               choices = list("2" = 2,
                                                                              "3" = 3,
                                                                              "4" = 4,
                                                                              "5" = 5)), 
                                                   actionButton(inputId = "group_team", 
                                                                label="Cluster")
                                                   ),
                                                   conditionalPanel(condition = "input.group_team",
                                                                    hr(style ="border-color: grey"),
                                                                    span("Remark:"),
                                                                    br(),
                                                                    span("1. We choose some features to evaluate each team."),
                                                                    br(),
                                                                    span("2. We use PCA to reduce dimension, so you can see
                                                                         the result in two-dimensional animation."),
                                                                    br(),
                                                                    span("3. The results of animation and table are corresponding.")
                                                   )
                                                 ),
                                                 mainPanel(
                                                   fluidRow(column(width=12, 
                                                                   imageOutput(outputId="team_group_img", inline=TRUE),
                                                                   br()
                                                            ),
                                                            column(width=12, 
                                                                   dataTableOutput(outputId = "team_cluster_table")))
                                                 )
                                               )
                                      ),
                                      tabPanel(title="Scorers Cluster",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   selectInput(inputId = "cluster_player_season", 
                                                               label = "Select Season", 
                                                               choices = c("2020-2021", 
                                                                           "2019-2020", 
                                                                           "2018-2019", 
                                                                           "2017-2018", 
                                                                           "2016-2017")),
                                                   div(align = "left", 
                                                       actionButton(inputId = "cluster_player_setseason", 
                                                                    label = "Select This Season")),
                                                   br(),
                                                   conditionalPanel(condition = "input.cluster_player_setseason",
                                                   selectInput(inputId="player_group",
                                                               label="How Many Scorers Clusters?",
                                                               choices = list("2" = 2, 
                                                                              "3" = 3,
                                                                              "4" = 4)), 
                                                   actionButton(inputId = "group_player", 
                                                                label="Cluster")
                                                   ),
                                                   conditionalPanel(condition = "input.group_player",
                                                                    hr(style ="border-color: grey"),
                                                                    span("Remark:"),
                                                                    br(),
                                                                    span("1. We choose some features to evaluate each scorer."),
                                                                    br(),
                                                                    span("2. We use PCA to reduce dimension, so you can see
                                                                         the result in two-dimensional animation."),
                                                                    br(),
                                                                    span("3. The results of animation and table are corresponding.")
                                                   )
                                                 ),
                                                 mainPanel(
                                                   fluidRow(column(width=12, 
                                                                   imageOutput(outputId="player_group_img", inline=TRUE),
                                                                   br()),
                                                            column(width=12, 
                                                                   dataTableOutput(outputId = "player_cluster_table")))
                                                   )
                                                 )
                                               )
                                    )
                                    
                           ),
                           # Panel More
                           navbarMenu("More",
                                      tabPanel("Guide",
                                               includeMarkdown("AppDocument/Guide.md")),
                                      tabPanel("References",
                                               includeMarkdown("AppDocument/References.md")),
                                      tabPanel("About Clustering",
                                               span(str_c("Here we showcase our clustering functions step by step for better understanding the process! ",
                                               "The materials are not relevant to the football.")),
                                               br(),
                                               br(),
                                               tabsetPanel(
                                                 tabPanel(title="Demo Cluster", 
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              selectInput(inputId = "cluster_full_view", 
                                                                          label = "Show All the Clusters?",
                                                                          choices = list(
                                                                            " " = 2,
                                                                            "Show All the Clusters!" = 1,
                                                                            "Let me select!" = 0
                                                                          )),
                                                              conditionalPanel(condition="input.cluster_full_view==1|input.cluster_full_view==0",
                                                                               conditionalPanel(condition="input.cluster_full_view==0",
                                                                                                selectInput(inputId="k_crime_cluster", 
                                                                                                            label="How Many Clusterss?", 
                                                                                                            choices = list("2" = 2, 
                                                                                                                           "3" = 3, 
                                                                                                                           "4" = 4,
                                                                                                                           "5" = 5))
                                                                               ),
                                                                               actionButton(inputId="cluster_crime", 
                                                                                            label="Cluster")
                                                              )
                                                              
                                                            ),
                                                            mainPanel(
                                                              plotOutput(outputId ="crime_cluster_plt")
                                                            )
                                                          )),
                                                 tabPanel(title="Cluster Process",
                                                          sidebarLayout(
                                                            sidebarPanel(
                                                              selectInput(inputId="k_demo_cluster", 
                                                                          label="How many clusters do you want to create?",
                                                                          choices = list( "2" = 2,
                                                                                          "3" = 3,
                                                                                          "4" = 4)),
                                                              actionButton(inputId ="demo_cluster", 
                                                                           label="Reveal Cluster")),
                                                            mainPanel(
                                                              imageOutput(outputId="cluster_demo_img")
                                                            )
                                                          ))
                                               ))
                           )
                )
)


# server function
server <- function(input, output, session) {
  # let the user to choose League with a modal dialog box 

  showModal(modalDialog(
    selectInput(
      inputId = "league_choice",
      label   = "League Choice",
      choices = c("Premier League", "Primera Division", "Bundesliga 1")
    ),
    span('All the contents will be related to the league of your choice.'),
    footer = tagList(
      actionButton(inputId = "confirm_league", label = "Confirm")
    )
  ))
  # remove the modal once selected
  observeEvent(input$confirm_league, {
    removeModal()
  })
  
  # Panel 1: welcome page
  # current time
  output$currentTime <- renderText({
    invalidateLater(as.integer(1000), session)
    str_c("UTC Time: ", format(lubridate::now("UTC")))
  })

  # Recent Fixtures
  
  observeEvent(input$confirm_league, {
    # get_id
    fixtures_league_id <- league_id_match %>%
        filter(Country.League == input$league_choice,
               Season == 2020) %>%
        pull(id)
    
    # Get Last and Next
    last_fixture <- get_last_fixture(league_id = fixtures_league_id)
    next_fixture <- get_next_fixture(league_id = fixtures_league_id)
    
    # title
    output$title <- renderUI({
      tags$h2(input$league_choice)
    })
    
    # League logo
    output$leage_logo <- renderUI({
      tags$img(src = last_fixture$league$logo[1], height="50%", width="50%")
    })
    
    output$test <- renderUI({
      tags$h2("No fixtures for now.")
    })
    shinyjs::hide("fixture_now")
    
    # Check Now
    if(any(last_fixture$elapsed < 90)) {
      shinyjs::hide("test")
      shinyjs::show("fixture_now")
        output$fixture_now <- DT::renderDataTable({
          fixture_now <- last_fixture %>%
            filter(elapsed < 90) %>%
            mutate(Elapsed = str_c(elapsed, " min"),
                   UTC = format(ymd_hms(event_date)),
                   HomeLogo = str_c("<img src='", hometeam_logo,"' height = '60'></img>"),
                   HomeTeam = hometeam_name,
                   Score = str_c(goalsHomeTeam, " - ", goalsAwayTeam),
                   AwayLogo = str_c("<img src='", awayteam_logo,"' height = '60'></img>"),
                   AwayTeam = awayteam_name) %>%
            select(UTC, Venue = venue, Elapsed, HomeLogo, HomeTeam, Score, AwayTeam, AwayLogo)
          
          datatable(fixture_now, escape = FALSE,
                    options = list(info = FALSE,
                                   ordering = FALSE,
                                   searching = FALSE,
                                   processing = FALSE,
                                   paging = FALSE,
                                   scrollY = 230,
                                   scrollCollapse= TRUE),
                    rownames = FALSE)
        })
      }
    
    # Last
    
    output$fixture_last <- DT::renderDataTable({
      fixture_last <- last_fixture %>%
        filter(elapsed == 90) %>%
        mutate(UTC = format(ymd_hms(event_date)),
               HomeLogo = str_c("<img src='", hometeam_logo,"' height = '60'></img>"),
               Score = str_c(goalsHomeTeam, " - ", goalsAwayTeam),
               AwayLogo = str_c("<img src='", awayteam_logo,"' height = '60'></img>"),
               AwayTeam = awayteam_name) %>%
        select(UTC, Venue = venue, HomeLogo, HomeTeam = hometeam_name, Score, AwayTeam, AwayLogo)
      
      datatable(fixture_last, escape = FALSE,
                options = list(info = FALSE,
                               ordering = FALSE,
                               searching = FALSE,
                               processing = FALSE,
                               paging = FALSE,
                               scrollY = 230,
                               scrollCollapse= TRUE),
                rownames = FALSE)
    })
    
    # Next
    
    output$fixture_next <- DT::renderDataTable({
      fixture_next <- next_fixture %>%
        mutate(UTC = format(ymd_hms(event_date)),
               HomeLogo = str_c("<img src='", hometeam_logo,"' height = '60'></img>"),
               AwayLogo = str_c("<img src='", awayteam_logo,"' height = '60'></img>"),
               AwayTeam = awayteam_name) %>%
        select(UTC, Venue = venue, HomeLogo, HomeTeam = hometeam_name, Round = round, AwayTeam, AwayLogo)
      
      datatable(fixture_next, escape = FALSE,
                options = list(info = FALSE,
                               ordering = FALSE,
                               searching = FALSE,
                               processing = FALSE,
                               paging = FALSE,
                               scrollY = 230,
                               scrollCollapse= TRUE),
                rownames = FALSE)
    })
    
  })
  
  
  
  
  
  
  # Panel 2
  
  #Team Info Table
  
  league_id_team <- eventReactive(input$get_team_info, {
    league_id_match %>% 
      filter(Country.League == input$league_choice) %>% 
      filter(Season == substr(input$team_season, 1, 4)) %>% 
      pull(id)
  })
  
  data_team <- eventReactive(input$get_team_info, {
    left_join(by = "team_id", 
              get_team(league_id = league_id_team()), 
              get_standings(league_id = league_id_team()), 
              suffix = c("", ".y")) %>% 
      mutate(team_logo = str_c("<img src='", logo,"' height = '25'></img>")) %>% 
      select(Rank = rank, 
             `LOGO` = team_logo, 
             Name = name, 
             Country = country, 
             `Win` = all.win, 
             `Lose` = all.lose, 
             `Draw` = all.draw, 
             `Goals` = all.goalsFor, 
             `Founded Year` = founded, 
             Venue = venue_name, 
             `Venue Capacity` = venue_capacity) %>% 
      arrange(Rank)
  })
  
  output$team_table <- renderDataTable({
    data_team()
    datatable(data_team(), escape = FALSE, rownames = FALSE)})
  
  # Download csv of the team
  output$downloadData_team <- downloadHandler(
    filename = function() {
      str_c(
        str_c(str_replace_na(input$league_choice), 
              str_replace_na(input$team_season),
              "info",
              sep = "_"),
        ".csv")
    },
    content = function(file) {
      write_csv(data_team(), file)
    }
  )
  
  #Update select input
  
  league_id_team2 <- eventReactive(input$set_season, {
    league_id_match %>% 
      filter(Country.League == input$league_choice) %>% 
      filter(Season == substr(input$player_season, 1, 4)) %>% 
      pull(id)
  })
  
  teams_selector <- eventReactive(input$set_season, {
    get_team(league_id = league_id_team2())
  })
  
  observeEvent(input$set_season, {
    shinyjs::hide("player_table")
    updateSelectInput(session = session, 
                      inputId = "player_team", 
                      choices = teams_selector() %>% 
                        pull(name))
  })
  
  observeEvent(input$player_search, {
    shinyjs::show("player_table")
  })
  
  team_logo <- eventReactive(input$player_search, {
    teams_selector() %>% 
      filter(name == input$player_team) %>% 
      pull(logo)
  })
  
  output$team_logo <- renderUI({
    tags$img(src = team_logo(), height = "50%", width = "50%")
  })
  
  teamid <- eventReactive(input$player_search, {
    teams_selector() %>% 
      filter(name == input$player_team) %>% 
      pull(team_id)
  })
  
  player_data <- eventReactive(input$player_search, {
    get_player_by_team_season(team = teamid(), season = input$player_season) %>% 
      filter(league == input$league_choice) %>% 
      select(`Player Name` = player_name, 
             Position = position, 
             Age = age, 
             Nationality = nationality, 
             Height = height, 
             Weight = weight, 
             Goals = goals.total, 
             Tackles = tackles.total, 
             Passes = passes.total)
  })
  
  
  output$player_table <- renderDataTable({player_data()})
  
  # Download csv of the player
  output$downloadData_player <- downloadHandler(
    filename = function() {
      str_c(
        str_c(str_replace_na(input$league_choice), 
              str_replace_na(input$team_season),
              str_replace_na(input$player_team),
              "info",
              sep = "_"),
        ".csv")
    },
    content = function(file) {
      write_csv(player_data(), file)
    }
  )
  
  # Panel 3
  
  #Team
  
  league_id_team_panel3 <- eventReactive(input$teamvis_setseason, {
    league_id_match %>% 
      filter(Country.League == input$league_choice) %>% 
      filter(Season == substr(input$teamvis_season, 1, 4)) %>% 
      pull(id)
  })
  
  teams_selector_panel3 <- eventReactive(input$teamvis_setseason, {
    get_team(league_id = league_id_team_panel3())
  })
  
  observeEvent(input$teamvis_setseason, {
    updateSelectInput(session = session, 
                      inputId = "teamvis_team", 
                      choices = teams_selector_panel3() %>% 
                        pull(name))
  })
  
  teamid_panel3 <- eventReactive(input$teamvis_ex, {
    teams_selector_panel3() %>% 
      filter(name == input$teamvis_team) %>% 
      pull(team_id)
  })
  
  team_stat_panel3 <- eventReactive(input$teamvis_ex, {
    get_team_stat(league_id = league_id_team_panel3(), team_id = teamid_panel3()) %>% 
      .[,1:19] %>% 
      rename(`Home Goal` = goals_for_home) %>% 
      rename(`Away Goal` = goals_for_away) %>% 
      rename(`Home Conceded` = goals_against_home) %>% 
      rename(`Away Conceded` = goals_against_away) %>% 
      rename(`Home Wins` = wins_home) %>% 
      rename(`Home Draws` = draws_home) %>% 
      rename(`Home Loses` = loses_home) %>% 
      rename(`Away Wins` = wins_away) %>% 
      rename(`Away Draws` = draws_away) %>% 
      rename(`Away Loses` = loses_away) %>% 
      pivot_longer(., col = !api_results, names_to = "label", values_to = "number")
  })
  
  team_fixture_panel3 <- eventReactive(input$teamvis_ex, {
    get_team_fixture(league_id = league_id_team_panel3(), team_id = teamid_panel3()) %>% 
      mutate(Result = case_when(
        (goals_home_team - goals_away_team > 0)&(home_team_team_name == input$teamvis_team) ~ "Win", 
        (goals_home_team - goals_away_team < 0)&(home_team_team_name == input$teamvis_team) ~ "Lose", 
        (goals_home_team - goals_away_team > 0)&(away_team_team_name == input$teamvis_team) ~ "Lose", 
        (goals_home_team - goals_away_team < 0)&(away_team_team_name == input$teamvis_team) ~ "Win", 
        (goals_home_team - goals_away_team == 0) ~ "Draw"
      )) %>% 
      mutate(Index = c(1:length(Result)))
  })
  
  team_donut_goal <- eventReactive(input$teamvis_ex, {
    team_stat_panel3() %>% 
      .[13:14,] %>% 
      mutate(fraction = number/sum(number)) %>% 
      mutate(ymax = cumsum(fraction)) %>% 
      mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(labelposition = (ymax + ymin)/2) %>% 
      mutate(lab = paste0(label, "\n value: ", number))
  })
  
  output$team_goal_plot <- renderPlot(
    ggplot(data = team_donut_goal(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label)) + 
      geom_rect() + 
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      geom_text(x = 4.5, aes(y = labelposition, label = number), size = 4)+
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) + 
      theme_void() + 
      theme(legend.position = "bottom") + 
      labs(fill = "Goal Type") + 
      ggtitle("Total Goals")
  )
  
  team_donut_conceded <- eventReactive(input$teamvis_ex, {
    team_stat_panel3() %>% 
      .[16:17, ] %>% 
      mutate(fraction = number/sum(number)) %>% 
      mutate(ymax = cumsum(fraction)) %>% 
      mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(labelposition = (ymax + ymin)/2) %>% 
      mutate(lab = paste0(label, "\n value: ", number))
  })
  
  output$team_concede_plot <- renderPlot(
    ggplot(data = team_donut_conceded(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label)) + 
      geom_rect() + 
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      geom_text(x = 4.5, aes(y = labelposition, label = number), size = 4)+
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) + 
      theme_void() + 
      theme(legend.position = "bottom") + 
      labs(fill = "Goal Type") + 
      ggtitle("Total Goals Conceded")
  )
  
  team_donut_home <- eventReactive(input$teamvis_ex, {
    team_stat_panel3() %>% 
      .[c(4, 7, 10), ] %>% 
      mutate(fraction = number/sum(number)) %>% 
      mutate(ymax = cumsum(fraction)) %>% 
      mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(labelposition = (ymax + ymin)/2) %>% 
      mutate(lab = paste0(label, "\n value: ", number))
  })
  
  output$home_win_plot <- renderPlot(
    ggplot(data = team_donut_home(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label)) + 
      geom_rect() + 
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      geom_text(x = 4.5, aes(y = labelposition, label = number), size = 4)+
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) + 
      theme_void() + 
      theme(legend.position = "bottom") + 
      labs(fill = "Result") + 
      ggtitle("Total Home Games")
  )
  
  team_donut_away <- eventReactive(input$teamvis_ex, {
    team_stat_panel3() %>% 
      .[c(5, 8, 11),] %>% 
      mutate(fraction = number/sum(number)) %>% 
      mutate(ymax = cumsum(fraction)) %>% 
      mutate(ymin = c(0, head(ymax, n=-1))) %>% 
      mutate(labelposition = (ymax + ymin)/2) %>% 
      mutate(lab = paste0(label, "\n value: ", number))
  })
  
  output$away_win_plot <- renderPlot(
    ggplot(data = team_donut_away(), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=label)) + 
      geom_rect() + 
      scale_fill_brewer(palette=3) +
      scale_color_brewer(palette=3) +
      geom_text(x = 4.5, aes(y = labelposition, label = number), size = 4)+
      coord_polar(theta="y") + 
      xlim(c(-1, 4)) + 
      theme_void() + 
      theme(legend.position = "bottom") + 
      labs(fill = "Result") + 
      ggtitle("Total Away Games")
  )
  
  output$team_record_plot <- renderPlot(
    team_fixture_panel3() %>%
      mutate(Result = factor(Result, 
                             levels = c("Lose", "Draw", "Win"))) %>%
    ggplot() + 
      geom_path(aes(x = Index, y = Result, group = 1, color = Result)) + 
      geom_point(aes(x = Index, y = Result, color = Result)) + 
      ylab("Game Result") + 
      xlab("Game Round") + 
      ggtitle("Team Record vs Game Round") + 
      theme_bw()
  )
  
  #Player
  
  observeEvent(input$p3_scorers_setseason, {
    # get_id
    r_league_id <- league_id_match %>%
      filter(Country.League == input$league_choice,
             Season == substr(input$p3_scorers_season, 1, 4)) %>%
      pull(id)
    # get top_scorers
    top_scorers <- get_top_scorers(league_id = r_league_id)
    
    updateSelectInput(session = session, 
                      inputId = "best_scorers_radar", 
                      choices = top_scorers$player_name,
                      selected = top_scorers$player_name[1])
    
    if (any(top_scorers$shots_total == 0)) {
      top_scorers[top_scorers$shots_total == 0,]$shots_total <- mean(top_scorers$shots_total)
    }
    
    top_scorers <- top_scorers %>%
      replace_na(list(goals_assists = 0)) %>%
      mutate(gpg = goals_total / games_appearences,
             apg = goals_assists / games_appearences,
             gps = goals_total / shots_total,
             rypg = (cards_yellow + cards_red) / games_appearences,
             gph = goals_total / games_minutes_played) 
    
    top_scorers_plot <- top_scorers %>%
      select("Goals per Game" = gpg,
             "Assist per Game" = apg,
             "Goals per Shot" = gps,
             "Red/Yellow Card per Game" = rypg,
             "Goals per Hour" = gph)
    
    max_scorers_plot <- as.list(apply(top_scorers_plot, 2, max)) %>% as_tibble()
    min_scorers_plot <- tibble("Goals per Game" = 0,
                               "Assist per Game" = 0,
                               "Goals per Shot" = 0,
                               "Red/Yellow Card per Game" = 0,
                               "Goals per Hour" = 0)
    
    # Word Cloud
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$wordcloud <- renderPlot({
      wordcloud_rep(top_scorers$player_name, top_scorers$goals_total,
                    scale=c(2.7,0.2),
                    colors=brewer.pal(8, "Dark2"))
    })
    
    # Radar Plot
    observeEvent(input$best_scorers_radar,{
      top_scorers_radar <- top_scorers %>%
        filter(player_name == input$best_scorers_radar)  %>%
        select("Goals per Game" = gpg,
               "Assist per Game" = apg,
               "Goals per Shot" = gps,
               "Red/Yellow Card per Game" = rypg,
               "Goals per Hour" = gph) %>% 
        tibble()
      
      bind_radar <- bind_rows(max_scorers_plot,
                              min_scorers_plot,
                              top_scorers_radar)
      
      output$radar_plot <- renderPlot({
        bind_radar %>%
          round(3) %>%
          radarchart(axistype=2, 
                     #custom polygon
                     pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                     #custom the grid
                     cglcol="black", cglty=1, axislabcol="red", cglwd=0.8,
                     #custom labels
                     vlcex = 1.2, palcex = 1.5
                     )
      })
    })
    

  })
  
  
  # Panel 4
  
  league_id_player_cluster <- eventReactive(input$group_player, {
    league_id_match %>% 
      filter(Country.League == input$league_choice) %>% 
      filter(Season == substr(input$cluster_player_season,1,4)) %>% 
      pull(id)
  })
  
  player_cluster <- eventReactive(input$group_player, {
    player.data <- get_data(object="player", league_id=as.numeric(league_id_player_cluster()))
    k <- as.numeric(input$player_group)
    return(cluster(player.data, k))
  })
  
  observeEvent(input$group_player, {
     output$player_group_img <- renderImage ({
        outfile <- tempfile(fileext=".gif")
        p <- animation(player_cluster()$cluster_history)
        anim_save("outfile.gif", animate(p, renderer=gifski_renderer(loop=FALSE)))
        list(src = "outfile.gif", contentType = "image/gif")
        }, deleteFile=TRUE)
      })
  
  observeEvent(input$group_player, {
    output$player_cluster_table <- DT::renderDataTable({
      data <- player_cluster()$group_history 
      names(data) <- c("Cluster", "Player Name", "Position", "Nationality", "Team")
      datatable(data, rownames = FALSE)})
  })
 
 
   league_id_team_cluster <- eventReactive(input$group_team, {
     league_id_match %>% 
       filter(Country.League == input$league_choice) %>% 
       filter(Season == substr(input$cluster_team_season,1,4)) %>% 
       pull(id)
   })
 
   team_cluster <- eventReactive(input$group_team, {
     team.data <- get_data(object="team", league_id=as.numeric(league_id_team_cluster()))
     k <- input$team_group
     return(cluster(team.data, k))
     
   })
 
   observeEvent(input$group_team, {
     output$team_group_img <- renderImage ({
       outfile <- tempfile(fileext=".gif")
       p <- animation(team_cluster()$cluster_history)
       anim_save("outfile.gif", animate(p, renderer=gifski_renderer(loop=FALSE)))
       list(src = "outfile.gif", contentType = "image/gif")}, deleteFile=TRUE)})
   
   observeEvent(input$group_team, {
     output$team_cluster_table <- DT::renderDataTable({
       data <- team_cluster()$group_history %>% 
         mutate(team_logo = str_c("<img src='", logo,"' height = '25'> </img>")) %>%
       select(-c("logo", "teamid"))
       names(data) <- c("Cluster", "Team", "Rank", "Goal Diff", "Points", "Logo")
       datatable(data, escape=FALSE, rownames = FALSE)})
   })
 

  
  observeEvent(input$cluster_crime, {
      output$crime_cluster_plt  <- renderPlot({
        if (input$cluster_full_view == 1) {
          show_cluster(full=TRUE)
        } else {
          show_cluster(input$k_crime_cluster, full=FALSE)
        }
      })
  })
  
  observeEvent(input$demo_cluster, {
    shinyalert(title = "Information",
               text  = str_c("This process takes about a few minutes! ",
                             "Please wait in patience!"),
               type  = "info")
    })
  
  demo_cluster <- eventReactive(input$demo_cluster, {
    k <- as.numeric(input$k_demo_cluster)
    demo.data <- get_data(object="random", k=k)
    return(cluster(demo.data, k=k))
  })
  
  observeEvent(input$demo_cluster, {
      output$cluster_demo_img <- renderImage ({
        outfile <- tempfile(fileext=".gif")
        p <- animation(demo_cluster()$cluster_history)
        anim_save("outfile.gif", animate(p, renderer=gifski_renderer(loop=FALSE)))
        list(src = "outfile.gif", 
             contentType = "image/gif")
      }, deleteFile=TRUE)
      })
  
}

# run the application 
shinyApp(ui = ui, server = server)
