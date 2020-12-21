read_tiles <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  o <- list()
  w <- c(which(out == ""), length(out) + 1)
  for (i in 1:length(w)){
    from <- ifelse(i > 1, w[i - 1] + 1, 1) + 1
    to <- w[i] - 1
    tl <- out[from:to]
    nm <- out[from - 1]
    nm <- gsub("Tile |:", "", nm)
    d <- data.frame(tile = nm, v = tl, check.names = FALSE,
                    stringsAsFactors = FALSE)
    o[[i]] <- separate(d, "v", sep = "", into = paste0("v", 0:nchar(tl[1])))
  }
  o <- bind_rows(o)
  o <- select(o, -all_of("v0"))
  o$tile <- as.numeric(o$tile)
  return(o)
}

read_monster <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines()
  close(con)
  out <- data.frame(v = out, stringsAsFactors = FALSE)
  out <- separate(out, "v", sep = "", into = paste0("v", 0:nchar(out[1, 1])))
  m <- select(out, -all_of("v0"))
  m <- as.matrix(m)
  return(m)
}

all_combs <- function(x){
  ac <- list()
  utl <- unique(x$tile)
  for (i in 1:length(utl)){
    ti <- x[x$tile == utl[i], 2:11]
    c1 <- paste(ti[1:10, 1], collapse = "")
    c2 <- paste(ti[10:1, 1], collapse = "")
    c3 <- paste(ti[1:10, 10], collapse = "")
    c4 <- paste(ti[10:1, 10], collapse = "")
    c5 <- paste(ti[1, 1:10], collapse = "")
    c6 <- paste(ti[1, 10:1], collapse = "")
    c7 <- paste(ti[10, 1:10], collapse = "")
    c8 <- paste(ti[10, 10:1], collapse = "")
    c_all <- c(c1, c2, c3, c4, c5, c6, c7, c8)
    ac[[i]] <- data.frame(tile = utl[i], side = c(1, 1, 3, 3, 2, 2, 4, 4),
                          flip = c(FALSE, TRUE, FALSE, TRUE,
                                   FALSE, TRUE, FALSE, TRUE),
                          border = c_all, check.names = FALSE,
                          stringsAsFactors = FALSE)
  }
  ac <- bind_rows(ac)
  return(ac)
}

find_matching <- function(x){
  x$match <- NA
  for (i in 1:nrow(x)){
    tl <- x[i, "tile"]
    bi <- x[i, "border"]
    wb <- which(x$border == bi)
    wb <- setdiff(wb, i)
    if (length(wb) > 0){
      x[i, "match"] <- wb
    }
  }
  return(x)
}

put_together <- function(x, ac){
  nsides <- ac %>%
    filter(!is.na(match)) %>%
    group_by(tile) %>%
    summarise(n_sides = n_distinct(side), .groups = "drop")
  b <- list()
  corners <- nsides[nsides$n_sides == 2, ][["tile"]]
  b[[1]] <- corner_to_corner(x = x, ac = ac, co = corners[1])
  for (i in 2:4){
    c_next <- b[[i - 1]][[length(b[[i - 1]])]]
    b_turn <- paste(c_next[nrow(c_next), ncol(c_next):1], collapse = "")
    b[[i]] <- corner_to_corner(x = x, ac = ac,
                               co = names(b[[i - 1]])[length(b[[i - 1]])],
                               co_side = b_turn)
  }
  d <- sqrt(length(unique(x$tile)))
  ro <- list()
  ro[[1]] <- b[[1]]
  for (i in 2:(d - 1)){
    ro[[i]] <- list()
    ro[[i]][[1]] <- rot(b[[4]][[d - i + 1]], k = 3)
    names(ro[[i]])[1] <- names(b[[4]])[d - i + 1]
    ro[[i]][[d]] <- rot(b[[2]][[i]], k = 1)
    names(ro[[i]])[d] <- names(b[[2]])[i]
  }
  ro[[d]] <- lapply(b[[3]][d:1], rot, k = 2)
  for (i in 2:d){
    for (j in 2:d){
      top_tile <-  ro[[i - 1]][[j]]
      top_b <- paste(top_tile[nrow(top_tile), 1:ncol(top_tile)], collapse = "")
      left_tile <-  ro[[i]][[j - 1]]
      left_b <- paste(left_tile[1:nrow(left_tile), ncol(left_tile)],
                      collapse = "")
      t_id <- ac %>%
        filter(border %in% c(top_b, left_b)) %>%
        group_by(tile) %>%
        summarise(n = n(), .groups = "drop") %>%
        filter(n == 2) %>%
        pull(tile)

      sides <- ac %>%
        filter(border %in% c(top_b, left_b), tile == t_id) %>%
        pull(side)
      tile <- as.matrix(x[x$tile == t_id, -1])
      ro[[i]][[j]] <- orient_middle(x = tile , sides = sides,
                                    top = top_b, left = left_b)
      names(ro[[i]])[j] <- t_id
    }
  }
  ro_trim <- ro
  for (i in 1:d){
    for(j in 1:d){
      tile <- ro_trim[[i]][[j]]
      tile_trim <- tile[c(-1, -nrow(tile)), c(-1, -ncol(tile))]
      ro_trim[[i]][[j]] <- tile_trim
    }
  }
  ro_rows <- list()
  for (i in 1:d){
    ro_rows[[i]] <- do.call(cbind, ro_trim[[i]])
  }
  r <- do.call(rbind, ro_rows)
  return(r)
}

corner_to_corner <- function(x, ac, co, co_side = NULL){
  sides <- unique(ac[ac$tile == co & !is.na(ac$match), "side"])
  c1 <- as.matrix(x[x$tile == co, -1])
  c1_new <- orient_corner(x = c1, sides = sides, co_side = co_side)
  
  r <- list()
  r[[paste(co)]] <- c1_new
  
  n_mid <- sqrt(length(unique(x$tile))) - 2
  prev_tile <- c1_new
  prev_id <- co
  for (i in 1:(n_mid + 1)){
    b1 <- paste(prev_tile[1:10, 10], collapse = "")
    wb <- ac$border == b1 & ac$tile != prev_id
    adj_tile <- ac[wb, "tile"]
    adj_tile_side <- ac[wb, "side"]
    ti <- as.matrix(x[x$tile == adj_tile, -1])
    ti_new <- orient_side(x = ti, side = adj_tile_side, match_side = b1)
    prev_tile <- ti_new
    prev_id <- adj_tile
    r[[paste(prev_id)]] <- prev_tile
  }
  return(r)
}

orient_corner <- function(x, sides, co_side){
  if (all(sides %in% c(1, 2))){
    x2 <- rot(x, 2)
  }
  if (all(sides %in% c(2, 3))){
    x2 <- rot(x, 1)
  }
  if (all(sides %in% c(3, 4))){
    x2 <- x
  }
  if (all(sides %in% c(4, 1))){
    x2 <- rot(x, 3)
  }
  if (!is.null(co_side)){
    bi <- paste(x2[1:nrow(x2), ncol(x2)], collapse = "")
    if (bi != co_side){
      x2 <- t(x2)
    }
  }
  return(x2)
}

orient_side <- function(x, side, match_side){
  if (side == 2){
    x <- rot(x, 3)
  }
  if (side == 3){
    x <- rot(x, 2)
  }
  if (side == 4){
    x <- rot(x, 1)
  }
  bi <- paste(x[1:nrow(x), 1], collapse = "")
  if (bi != match_side){
    x <- flip_hor(x = x)
  }
  return(x)
}

orient_middle <- function(x, sides, top, left){
  if (all(sides %in% c(1, 2))){
    x2 <- x
  }
  if (all(sides %in% c(2, 3))){
    x2 <- rot(x, 3)
  }
  if (all(sides %in% c(3, 4))){
    x2 <- rot(x, 2)
  }
  if (all(sides %in% c(4, 1))){
    x2 <- rot(x, 1)
  }
  b_top <- paste(x2[1, 1:ncol(x2)], collapse = "")
  if (b_top != top){
    x2 <- t(x2)
  }
  b_left <- paste(x2[1:nrow(x2), 1], collapse = "")
  if (b_left != left){
    x2 <- t(x2)
  }
  return(x2)
}

rot <- function(x, k){
  for (i in 1:k){
    x <- t(x)[, ncol(x):1]
  }
  return(x)
}

flip_hor <- function(x){
  x[nrow(x):1, ]
}

flip_ver <- function(x){
  x[, ncol(x):1]
}

find_monster <- function(x, m){
  dm <- dim(m)
  wm <- which(m == "#")
  x2 <- x
  for (i in 1:(nrow(x) - dm[1] + 1)){
    for (j in 1:(ncol(x) - dm[2] + 1)){
      sub_check <- x[i:(i + dm[1] - 1), j:(j + dm[2] - 1)]
      is_monster <- all(sub_check[wm] == "#")
      if (is_monster){
        sub_check[wm] <- "O"
        x2[i:(i + dm[1] - 1), j:(j + dm[2] - 1)] <- sub_check
      }
    }
  }
  return(x2)
}

find_pic_orientation <- function(x, m){
  x <- find_monster(x = x, m = m)
  if (!any(x == "O")){
    x <- flip_hor(x)
    x <- find_monster(x = x, m = m)
    if (!any(x == "O")){
      x <- flip_ver(x)
      x <- find_monster(x = x, m = m)
    }
    if (!any(x == "O")){
      x <- flip_hor(x)
      x <- find_monster(x = x, m = m)
    }
  }
  if (!any(x == "O")){
    x <- rot(x, 1)
    x <- find_monster(x = x, m = m)
  }
  if (!any(x == "O")){
    x <- rot(x, 1)
    x <- find_monster(x = x, m = m)
  }
  if (!any(x == "O")){
    x <- rot(x, 1)
    x <- find_monster(x = x, m = m)
  }
  return(x)
}