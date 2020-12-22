read_cards <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  w <- which(out == "")
  p1 <- out[2:(w - 1)]
  p2 <- out[(w + 2):length(out)]
  o <- list(p1 = as.numeric(p1), p2 = as.numeric(p2))
  return(o)
}

play_combat <- function(x){
  finished <- FALSE
  while (!finished){
    x <- play_round(x = x)
    finished <- (length(x$p1) == 0) | (length(x$p2) == 0)
  }
  if (length(x$p1) == 0){
    winner <- x$p2
  } else{
    winner <- x$p1
  }
  score <- sum(winner * (length(winner):1))
  return(score)
}

play_round <- function(x){
  c1 <- x$p1[1]
  c2 <- x$p2[1]
  winner <- ifelse(c1 > c2, "p1", "p2")
  looser <- ifelse(winner == "p1", "p2", "p1")
  x[[winner]] <- c(x[[winner]][-1], x[[winner]][1], x[[looser]][1])
  x[[looser]] <- x[[looser]][-1]
  return(x)
}

play_recursive_combat <- function(x){
  finished <- FALSE
  mem <- list()
  while (!finished){
    inf_loop <- FALSE
    if (length(mem) > 0){
      il <- NULL
      i <- 1
      while (!inf_loop & i <= length(mem)){
        equal_p1 <- length(x$p1) == length(mem[[i]]$p1)
        if (equal_p1){
          equal_p1 <- all(x$p1 == mem[[i]]$p1)
        }
        equal_p2 <- length(x$p2) == length(mem[[i]]$p2)
        if (equal_p2){
          equal_p2 <- all(x$p2 == mem[[i]]$p2)
        }
        inf_loop <- equal_p1 & equal_p2
        i <- i + 1
      }
    }
    mem[[length(mem) + 1]] <- x
    
    if (inf_loop){
      x$winner <- "p1"
      finished <- TRUE
    } else{
      c1 <- x$p1[1]
      c2 <- x$p2[1]
      sub_game <- (c1 <= (length(x$p1) - 1)) & (c2 <= (length(x$p2) - 1))
      if (sub_game){
        x_sub <- list(p1 = x$p1[2:(1 + c1)], p2 = x$p2[2:(1 + c2)])
        x_sub <- play_recursive_combat(x = x_sub)
        winner <- x_sub$winner
      } else{
        winner <- ifelse(c1 > c2, "p1", "p2")
      }
      looser <- ifelse(winner == "p1", "p2", "p1")
      x[[winner]] <- c(x[[winner]][-1], x[[winner]][1], x[[looser]][1])
      x[[looser]] <- x[[looser]][-1]
      finished <- (length(x$p1) == 0) | (length(x$p2) == 0)
    }
  }
  if (length(x$p1) == 0){
    x$winner <- "p2"
  }
  if (length(x$p2) == 0){
    x$winner <- "p1"
  }
  return(x)
}
