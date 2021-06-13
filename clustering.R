# load api helper functions
source("api_wrappers_football.R")
select <- dplyr::select
############################# Cluster #########################################

show_cluster <- function(k=2, full=FALSE) {
  df <- USArrests
  df <- na.omit(df)
  df <- scale(df)
  
  k0 <- kmeans(df, centers=k, nstart=25)
  
  k2 <- kmeans(df, centers = 2, nstart = 25)
  k3 <- kmeans(df, centers = 3, nstart = 25)
  k4 <- kmeans(df, centers = 4, nstart = 25)
  k5 <- kmeans(df, centers = 5, nstart = 25)
  
  p1 <- fviz_cluster(k2, geom = "point", ggtheme = theme(legend.position="none"), data = df) + ggtitle("k = 2") 
  p2 <- fviz_cluster(k3, geom = "point", ggtheme = theme(legend.position="none"), data = df) + ggtitle("k = 3")
  p3 <- fviz_cluster(k4, geom = "point", ggtheme = theme(legend.position="none"), data = df) + ggtitle("k = 4")
  p4 <- fviz_cluster(k5, geom = "point", ggtheme = theme(legend.position="none"), data = df) + ggtitle("k = 5")
  
  if (full) {
    
    return(grid.arrange(p1, p2, p3, p4, nrow = 2))
  } else {
    return(fviz_cluster(k0, geom = "point", ggtheme = theme(legend.position="none"), data=df) + 
             ggtitle(str_c("k = ", k)))
  }
}


get_data <- function(object, league_id=2, k=4) {
  player <- FALSE 
  team <- FALSE 
  random <- FALSE
  if (object == "player") {
    player <- TRUE
  } else if (object == "team") {
    team <- TRUE
  } else {
    random <- TRUE
  }
  if (team) {
    
    team_stands <- get_standings(league_id = league_id) 
    data <- team_stands %>% 
      select(win = all.win,
             draw = all.draw,
             lose = all.lose,
             goalsFor = all.goalsFor, 
             goalsAgainst = all.goalsAgainst,
             home_win = home.win,
             home_draw = home.draw,
             home_lose = home.lose, 
             home_goalsFor = home.goalsFor, 
             home_goalsAgainst = home.goalsAgainst, 
             away_win = away.win, 
             away_draw = away.draw, 
             away_lose = away.lose, 
             away_goalsFor = away.goalsFor, 
             away_goalsAgainst = away.goalsAgainst,
             "teamName" = teamName, 
             "teamid"  = team_id, 
             "rank" = rank, 
             goalsDiff = goalsDiff,
             points = points,
             logo = logo)
    pca_result <- prcomp(as.matrix(data[1:15]))
    dim_red_data <- as.matrix(data[,1:15]) %*% pca_result$rotation[,c(1,2)]
    return(data.frame(X1 = dim_red_data[,1],
                      X2 = dim_red_data[,2], 
                      "Team_Name" = data[,16],
                      "teamid" = data[,17],
                      "rank" = data[,18],
                      "goals_diff" = data[,19],
                      "points" = data[,20],
                      "logo" = data[,21]))
    
    
  } else if (player) {
    top_scorers <- get_top_scorers(league_id=league_id)
    data <- top_scorers %>%
      replace_na(list(goals_assists = 0)) %>%
      mutate(gpg = goals_total / games_appearences,
             apg = goals_assists / games_appearences,
             gps = goals_total / shots_total,
             rypg = (cards_yellow + cards_red) / games_appearences,
             gph = goals_total / games_minutes_played) %>% 
      select("goals_per_game" = gpg,
             "assist_per_game" = apg,
             "goals_per_shot" = gps,
             "ry_card_per_game" = rypg,
             "goals_per_hour" = gph,
             "name" = player_name,
             "position" = position,
             "nationality" = nationality,
             "team" = team_name)
    pca_result <- prcomp(as.matrix(data[,1:5]))
    dim_red_data <- as.matrix(data[,1:5]) %*% pca_result$rotation[,c(1,2)]
    
    return(data.frame(X1 = dim_red_data[,1],
                      X2 = dim_red_data[,2], 
                      "Player_Name" = data[,6],
                      "Position" = data[,7],
                      "Nationality" = data[,8],
                      "Team" = data[,9]))
  } else  {
    
    cov.collect <- list(c(1.5,1,1,1.5), 
                        c(1,2,2,6), 
                        c(4,0,0,4), 
                        c(5,2,2,5),
                        c(9,0,0,9), 
                        c(4.5, 1.5, 4.5, 1.5))
    df <- map_dfr(1:k, function(k) {
      cluster.s <- sample(1:k, size=300, replace=TRUE)
      
      mu.s <- sample.int(21, size=2, replace=TRUE) - 11
      cov.index <- sample.int(6, size=1)
      
      cov.mat <-matrix(cov.collect[[cov.index]], 2)
      set <- mvrnorm(n=300, mu.s, cov.mat)
      
      return(data.frame(X1 = set[,1],
                        X2 = set[,2],
                        cluster=0))})
    
  }
}




myKmeans <- function (data , k, centers = NULL, stop.crit = 10e-5, seed = 326) {
  # k is the cluster number of clusters 
  
  # set.seed(seed)
  n <- nrow(data)
  
  # Initialization
  if(is.null(centers)) {
    centers <- sample(n, k)
  }
  
  centroid <- data[centers, ]
  centroid.hist <- data.frame(centroid)
  
  cluster <- c(sample(k, n, replace=TRUE))
  
  withinss <- c() 
  
  size <- c() 
  converge <- FALSE 
  
  dist_crit <- 10e5 
  
  iter <- 0
  data.hist <- data.frame(data, cluster, iter, isCentroid=FALSE)
  centroid.hist <- data.frame(centroid, cluster=0, iter, isCentroid=TRUE)
  
  data.hist <- data.frame()
  centroid.hist <- data.frame()
  
  while (!converge) {
    iter <- iter + 1
    centroid.prev <- centroid 
    
    # assignment 
    for (row in 1:n) {
      dist <- apply(centroid, 1, function(centroid) {return(sum((centroid - data[row,])^2))})
      cluster[row] <- which.min(dist)
    }
    
    # update the assignment 
    for (cen in 1:k) {
      centroid[cen, ] = apply(data[cluster == cen, ], 2, mean)
    }
    
    centroid.hist <- rbind(centroid.hist, data.frame(centroid, cluster = 1:k, iter, isCentroid=TRUE))
    data.hist = rbind(data.hist, data.frame(data, cluster, iter, isCentroid=FALSE))
    
    dist_crit <- mean(apply((centroid.prev - centroid)^2, 1, sum))
    if (dist_crit <= stop.crit) {converge <- TRUE}
    
    
  }
  
  for (m in 1:k) {
    withinss[m] = sum((apply(data[cluster == m, ], 1,
                             function(x) {
                               x - apply(data[cluster==m, ], 2, mean)}))^2)
    size[m] = sum(cluster == m)
  }
  
  totss = sum(apply(data, 1, function(x) sum((x - apply(data,2,mean))^2)))
  tot.withinss = sum(withinss)
  
  return(list(data = data.frame(data,cluster),
              cluster = cluster,
              centroid = centroid,
              totss = totss,
              withinss = withinss,
              tot.withinss = tot.withinss,
              betweenss = totss - tot.withinss,
              size = size,
              cen_hist = centroid.hist,
              data_hist = data.hist,
              iterations = iter))
  
}


cluster <- function(data, k=4) {
  res <- myKmeans(data[1:2], k=k)
  
  data.hist <- rbind(res$data_hist, res$cen_hist) %>% 
    mutate(iter = as.integer(iter)) %>% 
    mutate(size = case_when(
      isCentroid ~ 1,
      !isCentroid ~ 0.9)) %>% 
    mutate(alpha = case_when(
      isCentroid ~ 2,
      !isCentroid ~ 1))
  group.result <- cbind(group = res$cluster, 
                        data[,-c(1,2)])
  cluster.history <- data.hist
  
  return(list(group_history = group.result,
              cluster_history = cluster.history))
}

animation <- function(data) {
  data_plot3 <- data
  if (nrow(data) <= 200) {
    base <-  ggplot(data_plot3,aes(x=X1,y=X2,color=as.factor(cluster), 
                                   size=size)) + geom_point() +
      guides(size=FALSE) + 
      labs(color="cluster")
  } else {
    base <- ggplot(data_plot3,aes(x=X1,y=X2,color=as.factor(cluster), 
                                  size=size, alpha=alpha)) + geom_point() + 
      guides(size=FALSE, 
             alpha=FALSE) + 
      labs(color="cluster")
  }
  
  p <- base + 
    # labs(subtitle="Iter: {next_state}") + 
    theme_classic() + 
    # theme(legend.position="none") + 
    transition_manual(iter) + 
    ease_aes("linear")
  return(p)
}



# animate(p, renderer=gifski_renderer(loop=FALSE))






