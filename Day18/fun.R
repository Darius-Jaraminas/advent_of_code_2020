read_math <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- lapply(out, function(x){
    x <- substring(x, 1:nchar(x), 1:nchar(x))
    x <- x[x != " "]
  })
  return(out)
}

new_math <- function(expr){
  op <- which(expr == "+" | expr == "*")
  r <- as.numeric(expr[1])
  for (i in op){
    if (expr[i] == "+"){
      r <- r + as.numeric(expr[i + 1])
    }
    if (expr[i] == "*"){
      r <- r * as.numeric(expr[i + 1])
    }
  }
  return(r)
}

evaluate_expression <- function(expr){
  is_paren <- (expr == "(") | (expr == ")")
  if (any(is_paren)){
    ints <- find_parentheses(expr = expr)
    while (any(is_paren)){
      inside_paren <- expr[(ints[1, 1] + 1):(ints[1, 2] - 1)]
      is_paren_inside <- (inside_paren == "(") | (inside_paren == ")")
      if (any(is_paren_inside)){
        result <- evaluate_expression(expr = inside_paren)
      } else{
        result <- new_math(inside_paren)
      }
      expr <- expr[-((ints[1, 1] + 1):(ints[1, 2]))]
      expr[ints[1, 1]] <- result
      ints <- find_parentheses(expr = expr)
      is_paren <- (expr == "(") | (expr == ")")
    }
  }
  result <- new_math(expr)
  return(result)
}

find_parentheses <- function(expr){
  open <- which(expr == "(")
  w2 <- which(expr == ")")
  if (length(open) > 0){
    k <- length(open)
    close <- numeric(k)
    for (i in 1:k){
      found_close <- FALSE
      j <- open[i] + 1
      mid_count <- 0
      while (!found_close){
        if (expr[j] == ")" & mid_count == 0){
          close[i] <- j
          found_close <- TRUE
        }
        if (expr[j] == ")" & mid_count > 0){
          mid_count <- mid_count - 1
        }
        if (expr[j] == "("){
          mid_count <- mid_count + 1
        }
        if (j == length(expr) & !found_close){
          close[i] <- NA
          found_close <- TRUE
        }
        j <- j + 1
      }
    }
  } else{
    close <- open
  }
  intervals <- cbind(open = open, close = close)
  return(intervals)
}

addition_first <- function(x){
  for (i in 1:length(x)){
    e_i <- x[[i]]
    w <- which(e_i == "+")
    if (length(w) > 0){
      for (k in 1:length(w)){
        j <- which(e_i == "+")[k]
        left <- e_i[j - 1]
        if (left == ")"){
          ints <- find_parentheses(e_i[1:(j - 1)])
          from <- ints[which(ints[, "close"] == j - 1), "open"]
        } else{
          from <- j - 1
        }
        right <- e_i[j + 1]
        if (right == "("){
          ints <- find_parentheses(e_i[(j + 1):length(e_i)])
          to <- ints[which(ints[, "open"] == 1), "close"] + j
        } else{
          to <- j + 1
        }
        if (from > 1){
          int1 <- 1:(from - 1)
        } else{
          int1 <- 0
        }
        int2 <- from:to
        if (to < length(e_i)){
          int3 <- (to + 1):length(e_i)
        } else{
          int3 <- 0
        }
        e_i <- c(e_i[int1], "(", e_i[int2], ")", e_i[int3])
      }
      x[[i]] <- e_i
    }
  }
  return(x)
}
