play_cups <- function(x, k){
  x <- list(cups = x, current = x[1])
  for (i in 1:k){
    x <- one_move(x)
  }
  w1 <- which(x$cups == 1)
  if (w1 == length(x$cups)){
    r <- paste(x$cups[-w1], collapse = "")
  } else{
    cups <- c(x$cups[(w1 + 1):length(x$cups)], x$cups[1:(w1 - 1)])
    r <- paste(cups, collapse = "")
  }
  return(r)
}

one_move <- function(x){
  x <- pick_up_cups(x)
  x <- find_destination(x)
  picked_up_cups <- x$cups[x$pick_up]
  x$cups <- x$cups[-x$pick_up]
  w_des <- which(x$cups == x$destination)
  if (w_des == length(x$cups)){
    x$cups <- c(x$cups, picked_up_cups)
  } else{
    x$cups <- c(x$cups[1:w_des], picked_up_cups,
                x$cups[(w_des + 1):length(x$cups)])
  }
  w_cur <- which(x$cups == x$current) + 1
  if (w_cur > length(x$cups)){
    w_cur <- 1
  }
  x$current <- x$cups[w_cur]
  return(x)
}

pick_up_cups <- function(x){
  w <- which(x$cups == x$current)
  pick_up <- (w + 1):(w + 3)
  if (any(pick_up > length(x$cups))){
    w_over <- which(pick_up > length(x$cups))
    add <- 1:length(w_over)
    pick_up <- c(pick_up[pick_up <= length(x$cups)], add)
  }
  x$pick_up <- pick_up
  return(x)
}

find_destination <- function(x){
  destination <- x$current - 1
  found <- FALSE
  while (!found){
    if (destination %in% x$cups[x$pick_up]){
      destination <- destination - 1
    } else if (destination < min(x$cups)){
      destination <- max(x$cups)
    } else{
      found <- TRUE
    }
  }
  x$destination <- destination
  return(x)
}

play_cups_for_real4 <- function(x, k, add = 0){
  if (add > max(x)){
    x <- as.integer(c(x, (max(x) + 1):add))
  } else{
    x <- as.integer(x)
  }
  N <- length(x)
  y <- vector("integer", N)
  y[x[1:(N - 1)]] <- x[-1]
  y[x[N]] <- x[1]
  cur <- x[1]
  for(i in 1:k){
    a1 <- y[cur]
    a2 <- y[a1]
    a3 <- y[a2]
    if(cur == 1L){
      des <- N
    } else{
      des <- cur - 1L
    } 
    while(des %in% c(a1, a2, a3)){
      if(des == 1L) {
        des <- N
      } else{
        des <- des - 1L
      }
    }
    z <- y[a3]
    y[cur] <- z
    y[a3] <- y[des]
    y[des] <- a1
    cur <- z
  }
  r <- prod(y[1], y[y[1]])
  return(r)
}
