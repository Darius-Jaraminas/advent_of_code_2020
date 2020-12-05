clean_coord <- function(board, subset){
  co <- substring(board, subset[1], subset[2])
  co <- gsub("F|L", "0", co)
  co <- gsub("B|R", "1", co)
  co <- substring(co, 1:nchar(co), 1:nchar(co))
  co <- as.numeric(co)
  return(co)
}


find_binary <- function(set, coord){
  r <- set
  for (i in coord){
    if (i == 0){
      r <- r[1:(length(r) / 2)]
    } else{
      r <- r[(length(r) / 2 + 1):length(r)]
    }
  }
  return(r)
}