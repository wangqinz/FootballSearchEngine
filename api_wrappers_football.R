apiKey <- "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15"

########## get fixtures ##########


get_last_fixture = function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15", 
  league_id = 2, 
  number = 10){
  url_fixture <- 
    paste0("https://api-football-v1.p.rapidapi.com/v2/fixtures/league/",
           league_id, "/last/", number)
  response_fixture <- VERB("GET", 
                           url_fixture, 
                           add_headers(
                             'x-rapidapi-host'='api-football-v1.p.rapidapi.com',
                                       'x-rapidapi-key'= apiKey),
                           content_type("application/octet-stream"))
  raw_fixture <- content(response_fixture, "text",encoding = "UTF-8")
  raw_table_fixture <- tibble(fromJSON(raw_fixture)$api$fixtures)
  raw_table_fixture <- raw_table_fixture %>%
    mutate(hometeam_name = .$homeTeam$team_name,
           hometeam_logo = .$homeTeam$logo,
           awayteam_name = .$awayTeam$team_name,
           awayteam_logo = .$awayTeam$logo)
  return(raw_table_fixture)
}

get_next_fixture = function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15", 
  league_id = 2, 
  number = 10){
  url_next_fixture <- 
    paste0("https://api-football-v1.p.rapidapi.com/v2/fixtures/league/",
           league_id, "/next/", number)
  response_next_fixture <- VERB("GET", 
                           url_next_fixture, 
                           add_headers('x-rapidapi-host'=
                                         'api-football-v1.p.rapidapi.com',
                                       'x-rapidapi-key'= apiKey),
                           content_type("application/octet-stream"))
  raw_next_fixture <- content(response_next_fixture, "text",encoding = "UTF-8")
  raw_table_next_fixture <- tibble(fromJSON(raw_next_fixture)$api$fixtures)
  
  raw_table_next_fixture <- raw_table_next_fixture %>%
    mutate(hometeam_name = .$homeTeam$team_name,
           hometeam_logo = .$homeTeam$logo,
           awayteam_name = .$awayTeam$team_name,
           awayteam_logo = .$awayTeam$logo)
  
  return(raw_table_next_fixture)
}

########## search function ##########

##support self-api

## coach
get_coach <- function(
  # apikey
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15",
  # parmaters
  firstname ="unai"){
  firstname <-tolower(firstname)
  url_coach <- paste0(
    "https://api-football-v1.p.rapidapi.com/v2/coachs/search/",firstname)
  response_coach <- VERB("GET", 
              url_coach, 
              add_headers('x-rapidapi-host'='api-football-v1.p.rapidapi.com',
              'x-rapidapi-key'= apiKey),
              content_type("application/octet-stream"))
  
  raw_coach <- content(response_coach, "text",encoding = "UTF-8")
  raw_table_coach <- tibble(fromJSON(raw_coach)$api$coachs)
  return(raw_table_coach)
}


## get all leagues
get_league <- function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15"){
  url_league <- "https://api-football-v1.p.rapidapi.com/v2/leagues"
  response_league <- VERB("GET", 
              url_league, 
              add_headers('x-rapidapi-host'='api-football-v1.p.rapidapi.com',
              'x-rapidapi-key'= apiKey),
              content_type("application/octet-stream"))
  
  raw_league <- content(response_league, "text",encoding = "UTF-8")
  raw_table_league <- tibble(fromJSON(raw_league)$api$league)
  return(raw_table_league)
}


## get player data by player id
get_player <- function(
  # apikey
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15",
  # parmaters
  player = 253){
  url_player <- paste0(
    "https://api-football-v1.p.rapidapi.com/v2/players/player/",player)
  response_player <- VERB("GET", 
              url_player, 
              add_headers('x-rapidapi-host'='api-football-v1.p.rapidapi.com',
              'x-rapidapi-key'= apiKey),
              content_type("application/octet-stream"))
  raw_player <- content(response_player, "text",encoding = "UTF-8")
  raw_table_player <- tibble(fromJSON(raw_player)$api$players)
  return(raw_table_player)
}

# get all players data for all seasons from one team

get_player_by_team_season <- function(
  # apikey
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15",
  # parmaters
  team = 33, 
  season = "2018-2019"){
  url_player <- paste0(
    "https://api-football-v1.p.rapidapi.com/v2/players/team/", 
    team, "/", season)
  response_player_team <- VERB("GET", 
                          url_player, 
                          add_headers(
                            'x-rapidapi-host'='api-football-v1.p.rapidapi.com',
                                      'x-rapidapi-key'= apiKey),
                          content_type("application/octet-stream"))
  raw_player_team <- content(response_player_team, "text",encoding = "UTF-8")
  raw_table_player_team <- tibble(fromJSON(raw_player_team)$api$players)
  table_player_team = flatten(raw_table_player_team)
  return(table_player_team)
}


## get teams by league
get_team <- function(
  # apikey
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15",
  # parmaters
  league_id = 2){
  
  url_team <- paste0(
    "https://api-football-v1.p.rapidapi.com/v2/teams/league/",league_id)
  response_team <- VERB("GET", 
              url_team, 
              add_headers('x-rapidapi-host'='api-football-v1.p.rapidapi.com',
                          'x-rapidapi-key'= apiKey),
              content_type("application/octet-stream"))
  raw_team <- content(response_team, "text",encoding = "UTF-8")
  raw_table_team <- tibble(fromJSON(raw_team)$api$teams)
  return(raw_table_team)
}

get_team_stat <- function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15", 
  league_id = 2, 
  team_id = 33){
  url_team_stat = paste0(
    "https://api-football-v1.p.rapidapi.com/v2/statistics/",
    league_id, "/", team_id)
  response_team_stat <- VERB("GET", 
                             url_team_stat, 
                             add_headers(
                               'x-rapidapi-host'='api-football-v1.p.rapidapi.com',
                               'x-rapidapi-key'= apiKey
                               ),
                             content_type("application/octet-stream"))
  team_stat <- content(response_team_stat, "text", encoding = "UTF-8")
  team_stat_matchs <- as.data.frame(fromJSON(team_stat))
  team_stat_matchs <- team_stat_matchs %>% 
    janitor::clean_names(replace = c("api.statistics" = ""))
  cname <- colnames(team_stat_matchs) %>%
    str_replace("goals_", "") %>%
    str_replace("matchs_", "")
  colnames(team_stat_matchs) <- cname
  return(team_stat_matchs)
}

get_team_fixture <- function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15", 
  league_id = 2, 
  team_id = 33){
  url_team_fixture = paste0(
    "https://api-football-v1.p.rapidapi.com/v2/fixtures/team/",
    team_id, "/", league_id)
  response_team_fixture <- VERB("GET", 
                                url_team_fixture, 
                                add_headers(
                                  'x-rapidapi-host'='api-football-v1.p.rapidapi.com',
                                  'x-rapidapi-key'= apiKey
                                  ),
                                content_type("application/octet-stream"))
  team_fixture <- content(response_team_fixture, "text", encoding = "UTF-8")
  team_fixture_stat <- as.data.frame(fromJSON(team_fixture))
  team_fixture_stat = team_fixture_stat %>%
    janitor::clean_names(replace = c("api.fixtures" = "")) %>% 
    flatten(.) %>% 
    janitor::clean_names(replace = c("_" = "."))
  return(flatten(team_fixture_stat))
}


get_top_scorers <- function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15", 
  league_id = 2){
  url_top_scorers <- paste0(
    "https://api-football-v1.p.rapidapi.com/v2/topscorers/", league_id)
  response_top_scorers <- VERB("GET", 
                               url_top_scorers, 
                               add_headers('x-rapidapi-host'=
                                             'api-football-v1.p.rapidapi.com',
                                           'x-rapidapi-key'= apiKey),
                               content_type("application/octet-stream"))
  top_scorers <- content(response_top_scorers, "text", encoding = "UTF-8")
  top_scorers <- fromJSON(top_scorers)
  top_scorers <- top_scorers$api$topscorers
  top_scorers <- top_scorers %>%
    flatten() %>%
    janitor::clean_names()
  return(top_scorers)
}

get_standings <- function(
  apiKey = "65563b231bmsh5e04da3f6a146c5p1b1625jsn0e31c1d79a15", 
  league_id = 2){
  url_standing <- paste0(
    "https://api-football-v1.p.rapidapi.com/v2/leagueTable/", league_id)
  response_standing <- VERB("GET", 
                            url_standing, 
                            add_headers(
                              'x-rapidapi-host'='api-football-v1.p.rapidapi.com',
                              'x-rapidapi-key'= apiKey),
                            content_type("application/octet-stream"))
  standing <- content(response_standing, "text", encoding = "UTF-8")
  standing_stat <- fromJSON(standing)$api$standings
  standing_stat_table <- as.data.frame(standing_stat)
  return(flatten(standing_stat_table))
}



