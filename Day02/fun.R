split_by_letter <- function(x){
  n <- nchar(x)
  r <- substring(x, 1:n, 1:n)
  return(r)
}

sep_data_columns <- function(x){
  x <- separate(x, col = "V1", into = c("policy", "password"), sep = ": ")
  x <- separate(x, col = "policy", into = c("policy_number", "policy_letter"),
                 sep = " ")
  x <- separate(x, col = "policy_number", into = c("policy_from", "policy_to"),
                 sep = "-")
  x[, "policy_from"] <- as.numeric(x[, "policy_from"])
  x[, "policy_to"] <- as.numeric(x[, "policy_to"])
  return(x)
}

check_policy_part1 <- function(x){
  x$valid <- FALSE
  for (i in 1:nrow(x)){
    spl <- split_by_letter(x[i, "password"])
    tbl <- table(spl)
    count <- tbl[x[i, "policy_letter"]]
    if (is.na(count)){
      count <- 0
    }
    x[i, "valid"] <-  count >= x[i, "policy_from"] & 
      count <= x[i, "policy_to"]
  }
  return(x)
}

check_policy_part2 <- function(x){
  x$valid <- FALSE
  for (i in 1:nrow(x)){
    spl <- split_by_letter(x[i, "password"])
    l1 <- spl[x[i, "policy_from"]]
    l2 <- spl[x[i, "policy_to"]]
    l <- x[i, "policy_letter"]
    x[i, "valid"] <-  sum(c(l1 == l, l2 == l)) == 1
  }
  return(x)
}
