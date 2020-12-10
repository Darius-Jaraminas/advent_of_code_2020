get_differences <- function(x){
  x <- c(0, x, max(x) + 3)
  x <- sort(x)
  x <- diff(x)
  return(x)
}

count_combinations <- function(x){
  w <- which(x == 3)
  n1 <- c()
  for (i in w){
    n1 <- c(n1, count_ones_before(x = x, i = i))
  }
  combs <- ones_to_combs(x = n1)
  n_combs <- prod(combs)
  return(n_combs)
}

count_ones_before <- function(x, i){
  x2 <- x[1:i]
  w3 <- which(x2 == 3)
  if (length(w3) == 1){
    n1 <- length(1:(i - 1))
  } else{
    from <- w3[length(w3) - 1] + 1
    to <- w3[length(w3)] - 1
    if (from <= to){
      ones <- from:to
    } else{
      ones <- c()
    }
    n1 <- length(ones)
  }
  return(n1)
}

ones_to_combs <- function(x){
  combs <- numeric(length(x))
  for (i in 1:length(x)){
    if (x[i] <= 1){
      combs[i] <- 1
    } else{
      co <- 1 + cumsum(1:(x[i] - 1))
      combs[i] <- co[length(co)]
    }
  }
  return(combs)
}
