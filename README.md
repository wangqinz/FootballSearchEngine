# STA 523 :: Project - Team: outliers

# Welcome to Football Center!

https://haoliangzheng.shinyapps.io/Football/

## API and our wrapper functions

The API we use to get data is [football API](https://www.api-football.com/).

We constructed multiple API wrapper functions to use in the Shiny App. They have 
similar structure but different parameters and returned values based on the 
structure of the API. 

The `API-Football` uses JSON format for all endpoints. Thus, for every wrapper 
function, after the API call, we use `fromJSON()` and `tibble()` or 
`as.data.frame()` to convert the data into workable format. 

The following is a list of our wrapper functions: 

1. `get_last_fixture()`: Get a number of recent finished matches in the specified 
league and their corresponding data. The function takes `league_id` as parameter. 

2. `get_next_fixtures()`: Get a number of future upcoming matches in the specified 
league and their corresponding information. The function takes `league_id` as 
parameter. 

3. `get_league()`: Search all available leagues in the `API-Football`. 

4. `get_player()`: Retrieve the data of a player by specifying `player_id`. 

5. `get_player_by_team_season()`: Retrieve the data for all players of a team in 
a season by specifying the `season` and `team_id`. 

6. `get_team()`: Retrieve team information for all teams in the specified league. 
The function takes `league_id` as parameter. 

7. `get_team_data()`: Retrieve match data for specified team by input `league_id` 
and `team_id`. 

8. `get_team_fixture()`: Retrieve data for each game that the specified team 
participated by input `league_id` and `team_id`. 

9. `get_top_scorers()`: Get the player data for top 20 scorers in the specified 
league by input `league_id`. 

10. `get_standings()`: Get the final ranking and cumulative game results for 
each team in the specified league by input `league_id`. 

A noticeable property of the commonly used parameter `league_id` is that due to 
the fact the teams in a league may change across different years, the `league_id` 
will be different for the same league in different years. For example, Premier 
League in 2018 and Premier League in 2019 have different `league_id`. We also 
constructed a league-year-ID matching table for the leagues we are working with. 

## Introduction to our clustering function 

We construct this K-means clustering algorithm to group the players and teams 
given certain football match seasons and leagues with a hyperparameter k. And we 
also create animations showing how the players and teams are graduated clustered 
using this algorithm. By showing the clustering result of the player and team, 
people might discover some pattern among the players and teams which are not 
revealed before.  

We use the `assist_per_game`, `goals_per_shot`, `ry_card_per_game`, 
`goals_per_hour`, and `goals_per_shot` to characterize and cluster the players. 
We use the `win`, `draw`, `lose`, `goals`, and other 11 matching related features 
to character and cluster the teams. We run Principle Component Analysis (PCA) 
on the features to reduce the number of features to two that they could be 
displayed and animated on a two-dimensional plot. Here is a list of functions 
developed for K-means clustering and visualization.  
1. `show_cluster()`: show cluster results with varied K centroids on th `USArrest` 
dataset for a simple demonstration of K-means clustering algorithm.  

2. `get_data()`: retrieve "player" and "team" data given the certain league and 
season. It also could generate data randomly. The principle component analysis and 
data transformatino is done in this function if necessary.  

3. `myKeans()`: this function implements K-means clustering algorithm on the dataset. 
It takes a list of parameters including, the number of centroids (k), the stopping 
criteria etc. It also returns a list of variables charactering the K-means clustering 
result such as, cluster result, cluster history, centroid history, withcluster 
sum of squares, and etc.  

4. `cluster()`: this is the function wraps the `myKmeans()` function to prepare the 
data for animation in plots and summary in tables.  

5. `animation()`: this function complete the animation of the data, in which the 
K-means clustering process is shown step by step so that the reader could easily 
understand how the results are obtained.  

As for the performance on the football data on our APP. The clustering results
sometimes could be unstable because the number of the observations (teams) are
limited to 20. The last four functions shown above serve as a modeling and 
visualization pipeline, which is adapted to "player", "team" and "random" data.
The visualization is a step-by-step clustering and assignment process, shown in
a two-dimensional plot with clusters labeled by different colors. The metadata 
results related to the clusters are also summarized in the tables for the 
readers to explore.  


## Introduction to our APP

This the place where you can not only get the information of football fixtures,
but also:

- Find and download information of both teams and players.

- Get some insight into the data with visualizations we've prepared for you.

- Grouping teams or players with our own clustering functions also with
  visualizations.

Except for the `More` Panel, there are other four main Panels for you to play 
around: `Recent Fixtures`, `Team and Player Info`, `Data Visualization` and
`Clustering`. 

The `Recent Fixtures` Panel  will first show you football fixtures which are 
going on, finished and coming. The `Team and Player Info` Panel is the place 
where you can find data regarding teams or players. The `Data Visualization` 
Panel is the place where you can see visualization results we've prepared for 
you. There are both team-level analysis and player-level analysis. For 
`Clustering` Panel, we have written our own functions for clustering algorithm.
We implement them here on football data, and users can try different number of
clusters by themselves.

There are also clustering demo displayed in the `More` Pabel, one for the 
USArrest crime dataset clustering and one for the clustering on a randomly 
generated dataset. They are used to showcase our functions, which is irrelevant 
to football.

## Reference


First, thanks to the [football API](https://www.api-football.com/) where we get
data from.

Seconds, there are few examples and Q&As that inspire us to achieve some 
functions in my website, I will list them below:

- https://gallery.shinyapps.io/010-download/

  From this website, we learned how to achieve the download function.

- https://shiny.rstudio.com/gallery/word-cloud.html
  
  From this website, we learned how to draw word cloud.

- https://www.r-graph-gallery.com/spider-or-radar-chart.html
  
  From this website, we learned how to draw radar chart.
  
- https://datatables.net/reference/option/
  
  From this website, we learned how to set detailed options for the datatable.
  
- https://www.r-graph-gallery.com/128-ring-or-donut-plot.html

  From this website, we learned how to make donut/ring charts using ggplot. 

- https://shiny.rstudio.com/gallery/word-cloud.html
  
  From this website, we learned how to draw word cloud.

- https://www.r-graph-gallery.com/spider-or-radar-chart.html
  
  From this website, we learned how to draw radar chart.
  
- https://datatables.net/reference/option/
  
  From this website, we learned how to set detailed options for the datatable. 

- https://smorbieu.gitlab.io/animate-intermediate-results-of-your-algorithm/
  
  From this website, we learned how to implement some K-means algorithm in details 
  some related animation in R. 

- Gareth James, Daniela Witten, Trevor Hastie, Robert Tibshirani. An Introduction 
to Statistical Learning : with Applications in R. New York :Springer, 2013.  
  
  From this book, we learned how the K-means algorithm works and how to implement 
  the K-means clustering algorithm in R.  

- https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/  
  
  From this website, we learned how to create animations.  

- https://stackoverflow.com/questions/35421923/how-to-create-and-display-an-animated-gif-in-shiny  
  
  From this website, we learned how to implete the animation in the R-shiny app.  


