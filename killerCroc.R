
#' @export 
myFunction <- function(info, readings, positions, edges, params) {
  if (is.null(info$mem)) info$mem <- list(turn = 0)
  info$mem$turn <- info$mem$turn + 1
  
  if (is.null(info$mem$belief)) {
    info$mem$belief <- rep(1/40, 40)
  }
  belief <- info$mem$belief
  n <- length(belief)
  my_pos <- positions[3]
  # --- hard evidence: eaten tourist -> exact waterhole ---
  eaten_hole <- NA_integer_
  if (!is.na(positions[1]) && positions[1] < 0) eaten_hole <- -as.integer(positions[1])
  if (!is.na(positions[2]) && positions[2] < 0) eaten_hole <- -as.integer(positions[2])
  if (!is.na(eaten_hole) && eaten_hole >= 1 && eaten_hole <= n) {
    post <- rep(0, n); post[eaten_hole] <- 1
    info$mem$belief <- post
    info$mem$best_hole <- eaten_hole
    
    path <- shortestPath(my_pos, info$mem$best_hole, edges)
    
    if (length(path) >= 2) {
      m1 <- as.integer(path[2])
      if (length(path) >= 3) {
        m2 <- as.integer(path[3])
      } else {
        m2 <- 0
      }
    } else {
      # fallback: if already at target or no path, stay on a legal node
      neigh <- unique(c(edges[edges[,1] == my_pos, 2],
                        edges[edges[,2] == my_pos, 1], my_pos))
      m1 <- as.integer(sample(neigh, 1))
      m2 <- 0
    }
    info$moves <- c(as.integer(m1), as.integer(m2))
    return(info)
  }
  
  # --- neighbors (including staying put) ---
  if (is.null(info$mem$neighbors)) {
    neigh_list <- vector("list", n)
    for (i in 1:n) {
      neigh <- unique(c(edges[edges[,1] == i, 2],
                        edges[edges[,2] == i, 1], i))
      neigh_list[[i]] <- as.integer(neigh)
    }
    info$mem$neighbors <- neigh_list
  }
  neighbors <- info$mem$neighbors
  
  # --- transition (prediction) ---
  prior_pred <- rep(0, n)
  for (j in 1:n) {
    nj <- neighbors[[j]]
    w <- 1 / length(nj)
    prior_pred[nj] <- prior_pred[nj] + belief[j] * w
  }
  
  # --- emission (likelihood) ---
  s1 <- dnorm(readings[1], mean = params[[1]][,1], sd = params[[1]][,2])
  s2 <- dnorm(readings[2], mean = params[[2]][,1], sd = params[[2]][,2])
  s3 <- dnorm(readings[3], mean = params[[3]][,1], sd = params[[3]][,2])
  like <- s1 * s2 * s3
  
  # living tourist excluded
  alive_tourists <- as.integer(positions[1:2])
  alive_tourists <- alive_tourists[!is.na(alive_tourists) & alive_tourists > 0]
  if (length(alive_tourists)) {
    like[alive_tourists] <- 0
  }
  
  post <- like * prior_pred
  s <- sum(post)
  if (s <= 0 || !is.finite(s)) {
    s2 <- sum(prior_pred)
    post <- if (s2 > 0) prior_pred / s2 else rep(1/n, n)
  } else {
    post <- post / s
  }
  
  info$mem$belief <- post
  info$mem$best_hole <- as.integer(which.max(post))
  path <- shortestPath(my_pos, info$mem$best_hole, edges)
  
  if (length(path) >= 2) {
    m1 <- as.integer(path[2])
    if (length(path) >= 3) {
      m2 <- as.integer(path[3])
    } else {
      m2 <- 0
    }
  } else {
    # fallback: if already at target or no path, stay on a legal node
    neigh <- unique(c(edges[edges[,1] == my_pos, 2],
                      edges[edges[,2] == my_pos, 1], my_pos))
    m1 <- as.integer(sample(neigh, 1))
    m2 <- 0
  }
  info$moves <- c(as.integer(m1), as.integer(m2))
  return(info)
}

shortestPath <- function(start, goal, edges, n_nodes = 40L) {
  if (is.na(start) || is.na(goal)) return(integer(0))
  start <- as.integer(start); goal <- as.integer(goal)
  if (start == goal) return(c(start))
  
  # adjacency via edges
  getNeigh <- function(v) {
    unique(c(edges[edges[,1] == v, 2], edges[edges[,2] == v, 1]))
  }
  
  visited <- rep(FALSE, n_nodes)
  parent  <- rep(NA_integer_, n_nodes)
  
  # simple queue using head/tail indices
  q <- integer(n_nodes); head <- 1L; tail <- 1L
  q[tail] <- start; tail <- tail + 1L
  visited[start] <- TRUE
  
  found <- FALSE
  while (head < tail) {
    v <- q[head]; head <- head + 1L
    for (u in getNeigh(v)) {
      u <- as.integer(u)
      if (!visited[u]) {
        visited[u] <- TRUE
        parent[u]  <- v
        if (u == goal) { found <- TRUE; break }
        q[tail] <- u; tail <- tail + 1L
      }
    }
    if (found) break
  }
  
  if (!found) return(integer(0))
  
  # reconstruct path goal -> start
  path <- integer(0); cur <- goal
  while (!is.na(cur)) {
    path <- c(cur, path)  # prepend
    if (cur == start) break
    cur <- parent[cur]
  }
  return(path)
}