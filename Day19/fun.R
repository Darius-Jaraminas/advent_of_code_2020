read_sat <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  w <- which(out == "")
  rules <- out[1:(w - 1)]
  rules <- gsub("\"", "", rules, fixed = TRUE)
  rules <- strsplit(rules, ": ")
  rules <- do.call(rbind, rules)
  rules <- data.frame(rules, check.names = FALSE, stringsAsFactors = FALSE)
  colnames(rules) <- c("nr", "rule")
  mes <- out[(w + 1):length(out)]
  r <- list(rules = rules, mes = mes)
  return(r)
}

expand_rules <- function(x){
  x$rule <- paste0(" ", x$rule, " ")
  x_new <- x
  d <- any(grepl("[[:digit:]]", x_new$rule))
  old_rules <- NULL
  while (d){
    l <- which(!grepl("[[:digit:]]", x_new$rule))
    l <- setdiff(l, old_rules)
    old_rules <- c(old_rules, l)
    for (i in 1:length(l)){
      x_new$rule <- replace_rule(x = x_new, rule_nr = l[i])
    }
    d <- any(grepl("[[:digit:]]", x_new$rule))
  }
  x_new$rule <- gsub("[[:space:]]", "", x_new$rule)
  return(x_new)
}

replace_rule <- function(x, rule_nr){
  rule <- x[rule_nr, "rule"]
  new_rule <- ifelse(!grepl("|", rule, fixed = TRUE), rule,
                     paste0(" (", rule, ") "))
  new_nr <- paste0(" ", x[rule_nr, "nr"], " ")
  replaced <- !any(grepl(new_nr, x$rule))
  while (!replaced){
    x$rule <- gsub(new_nr, new_rule, x$rule)
    replaced <- !any(grepl(new_nr, x$rule))
  }
  return(x$rule)
}

check_messages <- function(x){
  rule <- x$rules[x$rules$nr == 0, "rule"]
  rule <- paste0("^", rule, "$")
  corr <- grepl(rule, x$mes)
  return(corr)
}

check_messages_2 <- function(x){
  r42 <- paste0(x$rules[x$rules$nr == 42, "rule"])
  g42 <- gregexpr(r42, x$mes)
  l42 <- attr(g42[[1]], "match.length")[1]
  n42 <- sapply(g42, function(y, l){
    se <- seq(1, by = l, length = 100)
    w <- which(!(y %in% se))
    if (length(w) > 0){
      y <- y[1:(min(w) - 1)]
    }
    return(length(y))
  }, l = l42)
  w42 <- which(n42 >= 2)
  corr <- n42 >= 2
  for (i in w42){
    r0 <- paste0("^(", x$rules[x$rules$nr == 42, "rule"], "){", n42[i], "}", 
                 "(", x$rules[x$rules$nr == 31, "rule"], "){1,",
                 n42[i] - 1, "}$")
    corr[i] <- grepl(r0, x$mes[i])
  }
  return(corr)
}
