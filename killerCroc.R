# Authours: Isidor Löwbäck, Markus Eriksson

library("WheresCroc")

#Bfs search
shortestPath <- function(start, goal, edges, n_nodes = 40) {
  start <- start; goal <- goal
  if (start == goal) {
    return(c(start))
  }
  getNeigh <- function(v) sort(unique(c(edges[edges[,1] == v, 2], edges[edges[,2] == v, 1])))
  
  visited <- rep(FALSE, n_nodes) # replicates FALSE n_nodes amount of times
  parent  <- rep(NA_integer_, n_nodes)
  
  # queue
  q <- integer(n_nodes); head <- 1; tail <- 1
  q[tail] <- start; tail <- tail + 1
  visited[start] <- TRUE
  
  # search
  found <- FALSE
  while (head < tail) {
    v <- q[head]; head <- head + 1
    for (u in getNeigh(v)) {
      if (!visited[u]) {
        visited[u] <- TRUE
        parent[u]  <- v
        if (u == goal) { found <- TRUE; break }
        q[tail] <- u; tail <- tail + 1
      }
    }
    if (found) break
  }
  if (!found) {
    return(integer(0))
  }
  path <- integer(0); cur <- goal
  while (cur) {
    path <- c(cur, path)
    if (cur == start) break
    cur <- parent[cur]
  }
  return(path)
}


myFunction <- function(info, readings, positions, edges, params) {
  # amount of waterholes
  n <- 40
  
  if (is.null(info$mem)) {
    info$mem <- list(status = 0)
  }
  if (is.null(info$mem$turn)) {
    info$mem$turn <- 0
  }
  info$mem$turn <- info$mem$turn + 1
  
  # checks new game
  if (!is.null(info$mem$status) && (info$mem$status == 0 || info$mem$status == 1)) {
    info$mem$belief     <- NULL
    info$mem$neighbors  <- NULL
    info$mem$best_hole  <- NA_integer_
  }
  
  if (is.null(info$mem$neighbors)) {
    neigh_list <- vector("list", n)
    for (i in 1:n) {
      neigh <- c(edges[edges[,1] == i, 2], edges[edges[,2] == i, 1], i)
      neigh_list[[i]] <- neigh
    }
    info$mem$neighbors <- neigh_list
  }
  neighbors <- info$mem$neighbors
  
  if (is.null(info$mem$belief)) {
    info$mem$belief <- rep(1/n, n)
  }
  belief <- info$mem$belief
  
  my_pos <- positions[3]
  
  # if a tourist is eaten
  eaten_hole <- NA_integer_
  if (!is.na(positions[1]) && positions[1] < 0) eaten_hole <- -(positions[1])
  if (!is.na(positions[2]) && positions[2] < 0) eaten_hole <- -(positions[2])
  
  if (!is.na(eaten_hole) && eaten_hole >= 1 && eaten_hole <= n) {
    post <- rep(0, n); post[eaten_hole] <- 1
    info$mem$belief <- post
    info$mem$best_hole <- eaten_hole
    
    if (my_pos == eaten_hole) {
      info$moves <- c(my_pos, 0)  # search the space you are on
      info$mem$status <- 2
      return(info)
    }
    path <- shortestPath(my_pos, eaten_hole, edges)
    if (length(path) >= 2) {
      # take the first step along the path
      m1 <- path[2]
      path2 <- shortestPath(m1, eaten_hole, edges) # calculate BFS from m1 to eaten_hole
      if (length(path2) >= 2) {
        m2 <- path2[2] 
      } else {
        m2 <- 0
      }
    } else {
      m1 <- min(neighbors[[my_pos]])
      m2 <- 0
    }
    info$moves <- c(m1, m2)
    info$mem$status <- 2
    return(info)
  }
  
  # transition matrix applied to current belief
  prior_pred <- rep(0, n)
  for (j in 1:n) {
    nj <- neighbors[[j]]
    w <- 1 / length(nj)
    prior_pred[nj] <- prior_pred[nj] + belief[j] * w
  }
  s_pred <- sum(prior_pred)
  if (s_pred > 0) {
    prior_pred <- prior_pred / s_pred 
  } else {
    prior_pred <- rep(1/n, n)
  }

  # emission
  salinity <- dnorm(readings[1], mean = params[[1]][,1], sd = params[[1]][,2])
  phosphate <- dnorm(readings[2], mean = params[[2]][,1], sd = params[[2]][,2])
  nitrogen <- dnorm(readings[3], mean = params[[3]][,1], sd = params[[3]][,2])
  like <- salinity * phosphate * nitrogen
  
  # living tourist excluded
  alive_tourists <- positions[1:2]
  alive_tourists <- alive_tourists[!is.na(alive_tourists) & alive_tourists > 0]
  if (length(alive_tourists)) like[alive_tourists] <- 0
  
  post <- like * prior_pred
  s <- sum(post)
  if (s <= 0 || !is.finite(s)) {
    s2 <- sum(prior_pred)
    post <- if (s2 > 0) prior_pred / s2 else rep(1/n, n)
  } else {
    post <- post / s
  }
  
  info$mem$belief <- post
  info$mem$best_hole <- which.max(post)
  path <- shortestPath(my_pos, info$mem$best_hole, edges)
  
  if (length(path) >= 2) {
    m1 <- path[2]
    if (length(path) >= 3) {
      if (belief[path[3]] > 0.95) { # 0.95 was the best value for changing path from testing
        m2 <- 0
      } else {
        m2 <- path[3]
      }
    } else {
      m2 <- 0
    }
  } else {
    m1 <- my_pos
    m2 <- 0
  } 
  info$moves <- c(m1, m2)
  info$mem$status <- 2
  return(info)
}