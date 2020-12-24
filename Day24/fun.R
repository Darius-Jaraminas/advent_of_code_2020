read_tiles <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- lapply(out, function(x){
    x <- substring(x, 1:nchar(x), 1:nchar(x))
    x2 <- x
    w <- which(x %in% c("s", "n"))
    x2[w] <- paste0(x2[w], x2[w + 1])
    x2 <- x2[-(w + 1)]
    return(x2)
  })
  return(out)
}

flip_tiles <- function(x){
  l <- max(sapply(x, length))
  N <- (5 * l) * 2 + 1
  g <- matrix(0, nrow = N, ncol = N)
  ref_tile <- ceiling(c(nrow(g) / 2, ncol(g) / 2))
  for (i in 1:length(x)){
    dir <- x[[i]]
    tile <- ref_tile
    for (j in 1:length(dir)){
      tile <- case_when(
        dir[j] == "e" ~ tile + c(0, 1),
        dir[j] == "w" ~ tile + c(0, -1),
        (dir[j] == "se") & (tile[1] %% 2 == 1) ~ tile + c(1, 0),
        (dir[j] == "se") & (tile[1] %% 2 == 0) ~ tile + c(1, 1),
        (dir[j] == "sw") & (tile[1] %% 2 == 1) ~ tile + c(1, -1),
        (dir[j] == "sw") & (tile[1] %% 2 == 0) ~ tile + c(1, 0),
        (dir[j] == "ne") & (tile[1] %% 2 == 1)  ~ tile + c(-1, 0),
        (dir[j] == "ne") & (tile[1] %% 2 == 0)  ~ tile + c(-1, 1),
        (dir[j] == "nw") & (tile[1] %% 2 == 1) ~ tile + c(-1, -1),
        (dir[j] == "nw") & (tile[1] %% 2 == 0) ~ tile + c(-1, 0)
      )
    }
    g[tile[1], tile[2]] <- ifelse(g[tile[1], tile[2]] == 0, 1, 0)
  }
  return(g)
}

living_art_exhibit <- function(g, days){
  flip <- g == 2
  for (d in 1:days){
    for (i in 1:nrow(g)){
      for (j in 1:ncol(g)){
        flip[i, j] <- check_if_flips(g = g, i = i, j = j)
      }
    }
    g[flip] <- ifelse(g[flip] == 0, 1, 0)
  }
  return(g)
}

check_if_flips <- function(g, i, j){
  adj <- list()
  if (i %% 2 == 1){
    adj[[1]] <- c(i - 1, j - 1)
    adj[[2]] <- c(i - 1, j)
    adj[[3]] <- c(i, j + 1)
    adj[[4]] <- c(i + 1, j)
    adj[[5]] <- c(i + 1, j - 1)
    adj[[6]] <- c(i, j - 1)
  } else{
    adj[[1]] <- c(i - 1, j)
    adj[[2]] <- c(i - 1, j + 1)
    adj[[3]] <- c(i, j + 1)
    adj[[4]] <- c(i + 1, j + 1)
    adj[[5]] <- c(i + 1, j)
    adj[[6]] <- c(i, j - 1)
  }
  valid <- rep(TRUE, length(adj))
  for (k in 1:length(adj)){
    a <- adj[[k]]
    if (a[1] < 1 | a[1] > nrow(g)){
      valid[k] <- FALSE
    }
    if (a[2] < 1 | a[2] > ncol(g)){
      valid[k] <- FALSE
    }
  }
  adj <- adj[valid]
  b <- lapply(adj, function(x, g){
    g[x[1], x[2]] == 1
  }, g = g)
  b_sum <- sum(unlist(b))
  if (length(adj) == 2 & b_sum == 2){
      b_sum <- 3
  }
  flip <- FALSE
  if (g[i, j] == 1){
    if (b_sum == 0 | b_sum > 2){
      flip <- TRUE
    }
  }
  if (g[i, j] == 0){
    if (b_sum == 2){
      flip <- TRUE
    }
  }
  return(flip)
}
