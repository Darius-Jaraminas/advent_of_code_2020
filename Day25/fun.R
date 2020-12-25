transform_number <- function(x, k){
  v <- numeric(k)
  r <- 1
  for (i in 1:k){
    r <- r * x
    r <- r %% 20201227
    v[i] <- r
  }
  return(v)
}
