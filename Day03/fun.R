prep_map <- function(x){
  n <- nchar(x[1])
  l <- length(x)
  r <- matrix(NA, nrow = l, ncol = n)
  for (i in 1:l){
    r[i, ] <- substring(x[i], 1:n, 1:n)
  }
  return(r)
}

check_angle <- function(map, right, down){
  steps_down <- ceiling(nrow(map) / down)
  repeat_map <- ceiling(right * steps_down / ncol(map))
  ext_map <- list()
  for (i in 1:repeat_map){
    ext_map[[i]] <- data.frame(map, check.names = FALSE,
                               stringsAsFactors = FALSE)
  }
  ext_map <- bind_cols(ext_map, .name_repair = "minimal")
  down_all <- seq(1, nrow(ext_map), length.out = steps_down)
  right_all <- seq(1, by = right, length.out = steps_down)
  path <- character(steps_down)
  for (s in 1:steps_down){
    path[s] <- ext_map[down_all[s], right_all[s]]
  }
  return(path)
}