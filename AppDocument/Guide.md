# Guide

## Introduction

**Hi!** Welcome to Football Center, the place where you can not only get the 
information of football fixtures, but also:

- Find and download information of both teams and players.

- Get some insight into the data with visualizations we've prepared for you.

- Grouping teams or players with our own clustering functions also with
  visualizations.

Except for the `More` Panel, there are other four main Panels for you to play 
around: `Recent Fixtures`, `Team and Player Info`, `Data Visualization` and
`Clustering`. 

As we've asked you to choose one league you are interested in, the following 
contents will all be related to the information in this specific league.

Now let's see what you can do in these four Panels.

## "Recent Fixtures" Panel

The `Recent Fixtures` Panel is what you first see when you visit our website. 
This will first show you football fixtures which are going on (under Now), 
finished (under Last) and coming (under Next) in the league you choose. If 
there is no football fixture going on when you visit, you will see a line of
words "No fixtures for now". If there are fixtures going on, you can see how 
many minutes elapsed in this game when you first visit the website.


## "Team and Player Info" Panel

The `Team and Player Info` Panel is the place where you can find data regarding
teams or players.

For teams information, the only thing you need to do is to select the season 
you want to check. The table will show you two kind of information: basic 
information and season result for the team. Basic information includes things 
like LOGO, team name and founded year, while season result includes such as 
goals, ranks and so on.

For players information, you need to first select season and second select the 
team you are interested in. Then we will show you all the players' information 
in this team during this season. This will also include both basic and season 
information.

Want to see star players across teams instead of just in one team? Don't worry, 
you can get this in our "Data Visualization" panel with "top scorers" region.

## "Data Visualization" Panel

Here is the place where you can see visualization results we've prepared for 
you. There are both team-level analysis and player-level analysis.

In "team" region, first select season and team, then there are three different 
plots for you: donut chart for goals, donut chart for game results and a plot 
for game result vs round. For goals, you can see total goals and total goals 
conceded away or at home.For results, we split situations into home games and 
away games. As for game result vs round, you can see it as a kind of time series
plot.

In "top scorers" region, you can see who are the first 20 best scorers in this 
league during the season you choose. The first visualization you will see is a 
word cloud. The "frequency" is based on the total number of goals during this 
season. The second one is radar chart for the player you choose to see. You
can evaluate the player from five dimensions: Goals per Game, Assist per Game, 
Goals per Shot, Red/Yellow Card per Game, Goals per Hour.


## "Clustering" Panel

We have written our own functions for clustering algorithm. We implement them 
here on football data, and users can try different number of clusters by 
themselves. There are also both team-level analysis and player-level analysis. 
You can get the result from table also with visualizations below. The running 
time may be a little bit long because we're making animation. Please wait for 
a second!

## "More" Panel

In this panel, we want to point out that we also showcase some functions of 
our clustering. The materials are not relevant to the football. This includes 
crime cluster and cluster process.