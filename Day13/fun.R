read_schedule <- function(fnm, remx = TRUE){
  con <- file(fnm)
  out <- con %>%
    readLines() %>%
    strsplit(",")
  out[[1]] <- as.numeric(out[[1]])
  if (remx){
    out[[2]] <- as.numeric(out[[2]][out[[2]] != "x"])
  } else{
    out[[2]][out[[2]] == "x"] <- -1
    out[[2]] <- as.numeric(out[[2]])
  }
  names(out) <- c("time", "schedule")
  close(con)
  return(out)
}

find_closest_bus <- function(x){
  mult <- ceiling(x$time / x$schedule)
  arr <- mult * x$schedule
  wait <- arr - x$time
  w <- which.min(wait)
  w_min <- wait[w]
  id <- x$schedule[w]
  r <- w_min * id
  return(r)
}

find_t <- function(x){
  add <- 0:(length(x) - 1)
  add <- add[x != -1]
  x <- x[x != -1]
  
  f <- find_first_between_two(x = x[1:2], add = add[1:2])
  cm <- Lcm(x[1], x[2])

  for (k in 3:length(x)){
    i <- 1
    by <- 1000
    found <- FALSE
    while (!found){
      by_i <- ((i - 1) * by + 1):(i * by)
      ns <- f + by_i * cm
      ns <- (ns + add[k]) / x[k]
      res <- ns - floor(ns)
      if (any(res == 0)){
        found <- TRUE
        wr <- which(res == 0)[1]
        f <- f + by_i[wr] * cm
        cm <- Lcm(cm, x[k])
      } else{
        i <- i + 1
      }
    }
  }
  return(f)
}

find_first_between_two <- function(x, add){
  wm <- which.max(x)
  found <- FALSE
  i <- 1
  while (!found){
    ns <- (x[wm] * i - add[wm] + add) / x
    res <- ns - floor(ns)
    if (all(res == 0)){
      found <- TRUE
    } else{
      i <- i + 1
    }
  }
  r <- x[wm] * i - add[wm]
  return(r)
}
