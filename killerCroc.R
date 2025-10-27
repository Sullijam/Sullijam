library("WheresCroc")

#' @export 
myFunction <- function(info, readings, positions, edges, params) {
  if (is.null(info$mem)) info$mem <- list(turn = 0)
  info$mem$turn <- info$mem$turn + 1
  
  #print("Current readings are: ")
  #print(readings[])
  #print("params are: ")
  #print(params[])
  
  # Curr pos
  my_pos <- as.integer(positions[3])
  
  # neighbors
  neigh <- unique(c(edges[edges[,1] == my_pos, 2],
                    edges[edges[,2] == my_pos, 1],
                    my_pos))
  neigh <- as.integer(neigh)
  # TODO: perhaps move logic into forwardAlgorithm
  # step 1: check if a tourist has died
  waterhole <- 0
  if (!is.na(positions[1]) && positions[1] < 0) {
    waterhole <- positions[1]*-1
    m1 <- route()
    m2 <- 0
  
    info$moves <- c(as.integer(m1), as.integer(m2))
  }
  if (!is.na(positions[2]) && positions[2] < 0) {
    waterhole <- positions[2]*-1
  }
  #Forward algordom (six seeven)
  # step 2: calculate dnorm on all nodes except those where tourists are
  maximum <- 0
  for (i in 1:40) {
    salinity <- dnorm(readings[1], params[[1]][i,1], params[[1]][i,2]) #salin
    phosphate <- dnorm(readings[2], params[[2]][i,1], params[[2]][i, 2]) # vit fosfor(senapsgas)
    nitrogen <- dnorm(readings[3], params[[3]][i,1], params[[3]][i,2]) # salpetersyra(gunpowder) this croc is gonna blow!!!
    product <- salinity*phosphate*nitrogen
    if (product > maximum) {
      maximum <- product
      waterhole <- i
    }
  }
  print("Waterhole based on product")
  print(waterhole)
  # step 3: with the calculated guess, move in direction

  # move 1: random
  m1 <- sample(neigh, 1)
  
  # move 2: search at pos
  m2 <- 0
  
  # write moves to list info
  info$moves <- c(as.integer(m1), as.integer(m2))
  return(info)
}

forwardAlgorithm <- function(positions, info) {
  # TODO: understand and implement forward algorithm
  # LAST STEP!!!
}