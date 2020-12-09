check_sums <- function(x, preamble){
  w <- (preamble + 1):length(x)
  valid <- logical(length(w))
  for (i in 1:length(w)){
    x_i <- x[i:(i + preamble - 1)]
    y_i <- x[i + preamble]
    valid[i] <- check_one(x = x_i, y = y_i)
  }
  first_not_valid <- x[w[which(!valid)[1]]]
  return(first_not_valid)
}

check_one <- function(x, y){
  all_combs <- expand.grid(x, x)
  all_combs <- all_combs[all_combs$Var1 != all_combs$Var2, ]
  all_sums <- unique(rowSums(all_combs))
  valid <- y %in% all_sums
  return(valid)
}

find_sum <- function(x, invalid_number){
  sum_found <- FALSE
  i <- 1
  while (!sum_found){
    x_i <- x[i:length(x)]
    cs <- cumsum(x_i)
    sum_found <- invalid_number %in% cs
    if (sum_found){
      nn <- which(invalid_number == cs)
      s <- x[i:(i + nn - 1)]
    }
    i <- i + 1
    if (i > length(x)){
      stop("sum not found")
    }
  }
  return(s)
}
