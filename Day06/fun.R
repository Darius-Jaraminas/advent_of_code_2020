clean_cdf <- function(x){
  w <- which(x == "")
  n_gr <- length(w) + 1
  from <- c(1, w + 1)
  to <- c(w - 1, length(x))
  cdf <- list()
  for (i in 1:n_gr){
    cdf[[i]] <- x[from[i]:to[i]]
  }
  return(cdf)
}

count_uniques <- function(x){
  cx <- lapply(x, function(y){
    y <- paste(y, collapse = "")
    y <- substring(y, 1:nchar(y), 1:nchar(y))
    uy <- unique(y)
    nuy <- length(uy)
    return(nuy)
  })
  cx <- unlist(cx)
  return(cx)
}

count_repeats <- function(x){
  cx <- lapply(x, function(y){
    n_ppl <- length(y)
    y <- paste(y, collapse = "")
    y <- substring(y, 1:nchar(y), 1:nchar(y))
    tb <- table(y)
    repeats <- tb[tb == n_ppl]
    nry <- length(repeats)
    return(nry)
  })
  cx <- unlist(cx)
  return(cx)
}
