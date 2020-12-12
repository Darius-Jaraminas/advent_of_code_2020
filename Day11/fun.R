prep_seats <- function(x){
  n <- nchar(x[1])
  l <- length(x)
  r <- matrix(NA, nrow = l, ncol = n)
  for (i in 1:l){
    r[i, ] <- substring(x[i], 1:n, 1:n)
  }
  return(r)
}

run_simulation <- function(x, how = "adjecent", n_occ = 4){
  states_changed <- TRUE
  while (states_changed){
    x_ref <- x
    x <- change_states(x = x, how = how, n_occ = n_occ)
    states_changed <- !all(x == x_ref)
  }
  return(x)
}

change_states <- function(x, how = "adjecent", n_occ = 4){
  occ <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  emp <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      seat_ij <- x[i, j]
      if (how == "adjecent"){
        adj <- get_all_adjecent(x = x, row = i, col = j)
      }
      if (how == "visible"){
        adj <- get_all_visible(x = x, row = i, col = j)
      }
      if (seat_ij == "L"){
        occ[i, j] <- !any(adj == "#")
      }
      if (seat_ij == "#"){
        emp[i, j] <- sum(adj == "#") >= n_occ
      }
    }
  }
  if (any(occ)){
    x[occ] <- "#"
  }
  if (any(emp)){
    x[emp] <- "L"
  }
  return(x)
}

get_all_adjecent <- function(x, row, col){
  adj_cols <- (col - 1):(col + 1)
  adj_cols <- adj_cols[adj_cols > 0]
  adj_cols <- adj_cols[adj_cols <= ncol(x)]
  
  adj_rows <- (row - 1):(row + 1)
  adj_rows <- adj_rows[adj_rows > 0]
  adj_rows <- adj_rows[adj_rows <= nrow(x)]
  
  w <- matrix(FALSE, nrow = nrow(x), ncol = ncol(x))
  w[adj_rows, adj_cols] <- TRUE
  w[row, col] <- FALSE
  
  adj <- x[w]
  return(adj)
}

get_all_visible<- function(x, row, col){
  
  directions <- expand.grid(-1:1, -1:1)
  colnames(directions) <- c("row", "col")
  directions <- directions[!(directions$row == 0 & directions$col == 0), ]
  
  vis <- c()
  for (i in 1:nrow(directions)){
    vis <- c(vis, find_visible(x, seat = c(row, col),
                               dir = as.numeric(directions[i, ])))
  }
  return(vis)
}

find_visible <- function(x, seat, dir){
  found <- FALSE
  while (!found){
    seat <- seat + dir
    out_row <- seat[1] <= 0 | seat[1] > nrow(x)
    out_col<- seat[2] <= 0 | seat[2] > ncol(x)
    if (out_row | out_col){
      found <- TRUE
      r <- NULL
    } else{
      seat_look <- x[seat[1], seat[2]]
      emp <- seat_look == "L"
      occ <- seat_look == "#"
      if (emp | occ){
        r <- seat_look
        found <- TRUE
      }
    }
  }
  return(r)
}
