read_food <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- strsplit(out, " \\(")
  out <- lapply(out, function(x){
    ing <- strsplit(x[1], " ")[[1]]
    alle <- gsub("contains |\\)", "", x[2])
    alle <- strsplit(alle, ", ")[[1]]
    eg <- expand.grid(ing, alle)
    colnames(eg) <- c("ingredient", "allergen")
    return(eg)
  })
  out <- bind_rows(out, .id = "food")
  out$ingredient <- as.character(out$ingredient)
  out$allergen <- as.character(out$allergen)
  return(out)
}

find_possible <- function(x){
  ual <- unique(x$allergen)
  r <- list()
  for (i in 1:length(ual)){
    ali <- ual[i]
    tb <- sort(table(x[x$allergen == ali, "ingredient"]), decreasing = TRUE)
    ing <- names(tb)[tb == max(tb)]
    if (length(ing) > 0){
      r[[i]] <- data.frame(allergen = ali, ingredient = ing, n = max(tb),
                           stringsAsFactors = FALSE)
    }
  }
  r <- bind_rows(r)
  return(r)
}

count_occurences <- function(x, poss){
  r <- x %>%
    filter(!(ingredient %in% poss$ingredient)) %>%
    group_by(ingredient) %>%
    summarise(n = n_distinct(food), .groups = "drop") %>%
    summarise(n = sum(n)) %>%
    pull(n)
  return(r)
}

match_ing_with_alle <- function(x){
  idf <- list()
  while (nrow(x) > 0){
    idf_i <- identify_food(x = x)
    idf[[length(idf) + 1]] <- idf_i
    x <- x[!(x$ingredient %in% idf_i$ingredient), ]
    x <- x[!(x$allergen %in% idf_i$allergen), ]
  }
  idf <- bind_rows(idf)
  idf <- idf[order(idf$allergen), ]
  r <- paste(idf$ingredient, collapse = ",")
  return(r)
}

identify_food <- function(x){
  ual <- unique(x$allergen)
  r <- list()
  for (i in ual){
    tb <- x[x$allergen == i, "n"]
    if (sum(tb == max(tb)) == 1){
      m <- max(tb)
      ing <- x[x$allergen == i & x$n == m, "ingredient"]
      r[[i]] <- data.frame(ingredient = ing, allergen = i,
                           stringsAsFactors = FALSE)
    }
  }
  r <- bind_rows(r)
  return(r)
}
