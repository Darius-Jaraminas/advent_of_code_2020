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

play_game <- function(starting, turn){
  k <- length(starting)
  prev <- integer()
  for (i in 1:(k-1)){
    prev[starting[i] + 1] <- as.integer(i)
  }
  last <- starting[k]
  for (i in (k + 1):turn){
    new <- ifelse(is.na(prev[last + 1]), 0, i - 1 - prev[last + 1])
    prev[last + 1] <- as.integer(i - 1)
    last <- new
  }
  return(last)
}
