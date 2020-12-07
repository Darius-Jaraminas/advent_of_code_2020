clean_rules <- function(x){
  d <- data.frame(x, stringsAsFactors = FALSE)
  d <- separate(d, "x", into = c("bag", "contain"), sep = " contain ")
  return(d)
}

find_which_can_carry <- function(rules, bag_col){
  add_col <- rules[grepl(bag_col, rules$contain), "bag"]
  add_col <- gsub(" bags", "", add_col)
  all_col <- add_col
  while (length(add_col) > 0){
    add_col <- rules[grepl(paste(add_col, collapse = "|"), rules$contain), "bag"]
    add_col <- gsub(" bags", "", add_col)
    all_col <- c(all_col, add_col)
  }
  all_col <- unique(all_col)
  return(all_col)
}

clean_rules_with_numbers <- function(x){
  d <- data.frame(x, stringsAsFactors = FALSE)
  d <- separate(d, "x", into = c("bag", "contain"), sep = " contain ")
  r <- list()
  for (i in 1:nrow(d)){
    b <- gsub(" bags", "", d[i, "bag"])
    r[[i]] <- cbind(bag = b, contain_to_df(d[i, "contain"]))
  }
  r <- bind_rows(r)
  r[r$nr == "no", "nr"] <- 0
  r$nr <- as.numeric(r$nr)
  r[r$color == "other", "color"] <- ""
  return(r)
}

contain_to_df <- function(x){
  x <- strsplit(x, ", ")[[1]]
  x <- data.frame(x, stringsAsFactors = FALSE)
  x <- extract(x, col = "x", into = c("nr", "color"), regex = "^(\\S+)\\s+(.*)")
  x$color <- gsub(" bag| bags|\\.", "", x$color)
  return(x)
}

count_bags <- function(bag_col, rules){
  w <- rules$bag == bag_col
  col_inside <- rules[w, "color"]
  nr_inside <- rules[w, "nr"]
  if (col_inside[1] != ""){
    deeper_inside <- col_inside %>%
      lapply(count_bags, rules = rules) %>%
      unlist()
  } else{
    deeper_inside <- 0
  }
  nr <- as.numeric(nr_inside %*% (1 + deeper_inside))
  return(nr)
}
