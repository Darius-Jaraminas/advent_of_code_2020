read_ticket<- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  w <- which(out == "")
  rules <- out[1:(w[1] - 1)]
  rules <- strsplit(rules, ": ")
  rules_names <- lapply(rules, function(y){
    y[[1]]
  })
  rules_names <- unlist(rules_names)
  rules_ranges <- lapply(rules, function(y){
    rng <- strsplit(y[[2]], " or ")[[1]]
    nrs <- numeric()
    for (i in rng){
      interval <- strsplit(i, "-")[[1]]
      interval <- as.numeric(interval)
      nrs <- c(nrs, interval[1]:interval[2])
    }
    return(nrs)
  })
  names(rules_ranges) <- rules_names
  my_ticket <- out[(w[1] + 2):(w[2] - 1)]
  my_ticket <- my_ticket %>%
    strsplit(",") %>%
    `[[`(1) %>%
    as.numeric()
  tickets <- out[(w[2] + 2):length(out)]
  tickets <- tickets %>%
    strsplit(",") %>%
    lapply(as.numeric)
  r <- list(rules = rules_ranges, my = my_ticket, tickets = tickets)
  return(r)
}

find_invalid_values <- function(x){
  t <- x$tickets
  valid_all <- list()
  not_valid <- numeric()
  for (i in 1:length(t)){
    valid <- check_rules(ticket = t[[i]], rules = x$rules)
    not_valid <- c(not_valid, t[[i]][!valid$valid])
    valid_all[[i]] <- cbind(ticket = i, valid)
  }
  valid_all <- bind_rows(valid_all)
  r <- list(valid = valid_all, not_valid = not_valid)
  return(r)
}

check_rules <- function(ticket, rules){
  va <- list()
  for (i in 1:length(ticket)){
    v <- ticket[i]
    valid_i <- logical(length(rules))
    for (j in 1:length(rules)){
      r_j <- rules[[j]]
      valid_i[j] <- v %in% r_j
    }
    va[[i]] <- data.frame(v, t(valid_i), any(valid_i), stringsAsFactors = FALSE)
    colnames(va[[i]]) <- c("value", names(rules), "valid")
  }
  va <- bind_rows(va)
  return(va)
}

identify_fields <- function(v){
  valid_tickets <- v %>%
    group_by(ticket) %>%
    summarise(invalid = any(!valid), .groups = "drop") %>%
    filter(!invalid)
  v <- v[v$ticket %in% valid_tickets$ticket, ]
  k <- table(v$ticket)[1]
  v$attr <- 1:k
  v2 <- v[, !(colnames(v) %in% c("ticket", "valid", "value"))]
  v2 <- pivot_longer(v2, cols = -attr, names_to = "rule", values_to = "valid")
  v2 <- v2 %>%
    group_by(rule, attr) %>%
    summarise(valid = all(valid), .groups = "drop") %>%
    filter(valid)
  tb <- sort(table(v2$rule))
  r <- character(length(tb))
  while(length(tb) > 0){
    if (tb[[1]] == 1){
      nr <- v2[v2$rule == names(tb)[1], ][["attr"]]
      r[nr] <- names(tb)[1]
      v2 <- v2[v2$attr != nr, ]
      tb <- sort(table(v2$rule))
    } else{
      stop("Deduction rules not programmed for this case!")
    }
  }
  return(r)
}
