read_starting <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines() %>%
    strsplit(",") %>%
    `[[`(1) %>%
    as.numeric()
  close(con)
  return(out)
}

play_game <- function(starting, end){
  x <- numeric(end)
  x <- x * NA
  k <- length(starting)
  x[1:k] <- starting
  for (i in (k + 1):end){
    x <- next_number(x = x, i = i)
  }
  return(x)
}

next_number <- function(x, i){
  prev <- x[i-1]
  w_prev <- which(x == prev)
  if (length(w_prev) == 1){
    x[i] <- 0
  } else{
    w_prev <- w_prev[(length(w_prev) - 1):length(w_prev)]
    x[i] <- diff(w_prev)
  }
  return(x)
}

play_game2 <- function(starting, end){
  prev <- integer()
  for (i in unique(starting)){
    w <- which(starting == i)
    prev[paste(i)] <- as.integer(w[length(w)])
  }
  k <- length(starting)
  dt <- list(x = starting[k], prev = prev)
  for (i in (k + 1):end){
    if (i %% 1e06 == 0){
      print(i)
    }
    dt <- next_number2(dt = dt, i = i)
  }
  return(dt)
}

next_number2 <- function(dt, i){
  x_prev <- dt$x
  first <- (i - dt$prev[paste(x_prev)]) == 1
  if (first){
    dt$x <- 0
  } else{
    x_i <- i - 1 - dt$prev[[paste(x_prev)]]
    dt$x <- x_i
    dt$prev[paste(x_prev)] <- as.integer(i - 1)
    if (!(paste(x_i) %in% names(dt$prev))){
      dt$prev[paste(x_i)] <- as.integer(i)
    }
  }
  return(dt)
}

play_game3 <- function(starting, end){
  # prev <- integer(end) * NA
  prev <- integer()
  for (i in unique(starting)){
    w <- which(starting == i)
    prev[i+1] <- as.integer(w[length(w)])
  }
  k <- length(starting)
  dt <- list(x = starting[k], prev = prev)
  for (i in (k + 1):end){
    if (i %% 1e05 == 0){
      print(i)
    }
    dt <- next_number3(dt = dt, i = i)
  }
  return(dt)
}

next_number3 <- function(dt, i){
  x_prev <- dt$x
  first <- (i - dt$prev[x_prev + 1]) == 1
  if (first){
    dt$x <- 0
  } else{
    x_i <- i - 1 - dt$prev[x_prev + 1]
    dt$x <- x_i
    dt$prev[x_prev + 1] <- as.integer(i - 1)
    if (is.na(dt$prev[x_i + 1])){
      dt$prev[x_i + 1] <- as.integer(i)
    }
  }
  return(dt)
}

play_game4 <- function(starting, end){
  prev <- lapply(1:end, function(y){
    NA
  })
  for (i in unique(starting)){
    w <- which(starting == i)
    prev[[i+1]] <- w[length(w)]
  }
  k <- length(starting)
  dt <- list(x = starting[k], prev = prev)
  for (i in (k + 1):end){
    if (i %% 1e06 == 0){
      print(i)
    }
    dt <- next_number4(dt = dt, i = i)
  }
  return(dt)
}

next_number4 <- function(dt, i){
  x_prev <- dt$x
  first <- (i - dt$prev[[x_prev + 1]]) == 1
  if (first){
    dt$x <- 0
  } else{
    x_i <- i - 1 - dt$prev[[x_prev + 1]]
    dt$x <- x_i
    dt$prev[[x_prev + 1]] <- i - 1
    if (is.na(dt$prev[[x_i + 1]])){
      dt$prev[[x_i + 1]] <- i
    }
  }
  return(dt)
}

play_game5 <- function(starting, end){
  x <- integer(end)
  x <- x * NA
  k <- length(starting)
  x[1:k] <- as.integer(starting)
  for (i in (k + 1):end){
    x <- next_number5(x = x, i = i)
  }
  return(x)
}

next_number5 <- function(x, i){
  prev <- x[i-1]

  found <- FALSE
  j <- i - 2
  while (!found){
    found <- x[j] == prev
    if (!found){
      j <- j - 1
    }
    if (j == 0){
      found <- TRUE
    }
  }
  
  if (j == 0){
    x[i] <- 0L
  } else{
    x[i] <- as.integer(i - 1 - j)
  }
  return(x)
}



play_game_rec <- function(x, i){
  if (i > length(x) + 1){
    play_game_rec(x = x, i = i - 1)
  } else{
    if ()
    browser()
  }
  return(x)
}
