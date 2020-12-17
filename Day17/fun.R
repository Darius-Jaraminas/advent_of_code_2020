read_dimension <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines() %>%
    strsplit("")
  close(con)
  out <- do.call(rbind, out)
  return(out)
}

create_cube <- function(x, d){
  a <- array(".", dim = c(nrow(x) + 2 * d, ncol(x) + 2 * d, 1 + 2 * d))
  a[(d + 1):(d + nrow(x)), (d + 1):(d + ncol(x)), d + 1] <- x
  return(a)
}

create_cube_4 <- function(x, d){
  a <- array(".", dim = c(nrow(x) + 2 * d, ncol(x) + 2 * d,
                          1 + 2 * d, 1 + 2 * d))
  a[(d + 1):(d + nrow(x)), (d + 1):(d + ncol(x)), d + 1, d + 1] <- x
  return(a)
}

change_state <- function(cube){
  new_state <- cube
  d <- dim(cube)
  for (x in 1:d[1]){
    nbh_x <- make_range(i = x, i_max = d[1], dif = 1)
    for (y in 1:d[2]){
      nbh_y <- make_range(i = y, i_max = d[2], dif = 1)
      for (z in 1:d[3]){
        nbh_z <- make_range(i = z, i_max = d[3], dif = 1)
        nbh <- cube[nbh_x, nbh_y, nbh_z]
        sum_active_nbh <- sum(nbh == "#")
        if (cube[x, y, z] == "#"){
          sum_active_nbh <- sum_active_nbh - 1
          if (!(sum_active_nbh %in% c(2, 3))){
            new_state[x, y, z] <- "."
          }
        }
        if (cube[x, y, z] == "."){
          if (sum_active_nbh == 3){
            new_state[x, y, z] <- "#"
          }
        }
      }
    }
  }
  return(new_state)
}

make_range <- function(i, i_max, dif = 1){
  r <- (i - dif):(i + dif)
  r <- r[r > 0 & r <= i_max]
  return(r)
}

change_state_4 <- function(cube){
  new_state <- cube
  d <- dim(cube)
  for (x in 1:d[1]){
    nbh_x <- make_range(i = x, i_max = d[1], dif = 1)
    for (y in 1:d[2]){
      nbh_y <- make_range(i = y, i_max = d[2], dif = 1)
      for (z in 1:d[3]){
        nbh_z <- make_range(i = z, i_max = d[3], dif = 1)
        for (g in 1:d[4]){
          nbh_g <- make_range(i = g, i_max = d[4], dif = 1)
          
          nbh <- cube[nbh_x, nbh_y, nbh_z, nbh_g]
          sum_active_nbh <- sum(nbh == "#")
          if (cube[x, y, z, g] == "#"){
            sum_active_nbh <- sum_active_nbh - 1
            if (!(sum_active_nbh %in% c(2, 3))){
              new_state[x, y, z, g] <- "."
            }
          }
          if (cube[x, y, z, g] == "."){
            if (sum_active_nbh == 3){
              new_state[x, y, z, g] <- "#"
            }
          }
          
        }
      }
    }
  }
  return(new_state)
}
