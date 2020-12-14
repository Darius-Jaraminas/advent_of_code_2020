read_program <- function(fnm){
  con <- file(fnm)
  out <- con %>%
    readLines() %>% 
    strsplit(" = ")
  close(con)
  return(out)
}

run_program <- function(p){
  mem <- numeric()
  for (i in 1:length(p)){
    command <- p[[i]][1]
    if (grepl("mask", command)){
      mask <- clean_mask(m = p[[i]][2])
    }
    if (grepl("mem", command)){
      address <- as.numeric(gsub("mem\\[|\\]", "", p[[i]][1]))
      nr <- as.numeric(p[[i]][2])
      mem <- write_to_mem(mem = mem, mask = mask, address = address, nr = nr)
    }
  }
  return(mem)
}

clean_mask <- function(m){
  m <- substring(m, 1:nchar(m), 1:nchar(m))
  m[m == "X"] <- NA
  m <- as.numeric(m)
  return(m)
}

write_to_mem <- function(mem, mask, address, nr){
  bin_nr <- to_bin(x = nr)
  bin_masked <- apply_mask(x = bin_nr, mask = mask)
  mem[address] <- bin2dec(bin_masked)
  return(mem)
}

to_bin <- function(x){
  bin_x <- dec2bin(x)
  if (nchar(bin_x) < 36){
    add_z <- paste(rep(0, 36 - nchar(bin_x)), collapse = "")
    bin_x <- paste0(add_z, bin_x)
  }
  bin_x <- as.numeric(strsplit(paste(bin_x), "")[[1]])
  return(bin_x)
}

apply_mask <- function(x, mask){
  non_na <- which(!is.na(mask))
  x[non_na] <- mask[non_na]
  return(x)
}

dec2bin <- function(fnum){
  if (fnum == 0){
    r <- "0"
  } else{
    bin_vect <- rep(0, 1 + floor(log(fnum, 2)))
    while (fnum >= 2) {
      pow <- floor(log(fnum, 2))
      bin_vect[1 + pow] <- 1
      fnum <- fnum - 2^pow
    }
    bin_vect[1] <- fnum %% 2
    r <- paste(rev(bin_vect), collapse = "")
  }
  return(r)
} 

bin2dec <- function(x){
  d <- rep(2, length(x))
  dp <- d ^ ((length(x) - 1):0)
  a <- as.numeric(x %*% dp)
  return(a)
}

run_program2 <- function(p){
  mem <- numeric()
  for (i in 1:length(p)){
    command <- p[[i]][1]
    if (grepl("mask", command)){
      mask <- clean_mask(m = p[[i]][2])
    }
    if (grepl("mem", command)){
      address <- as.numeric(gsub("mem\\[|\\]", "", p[[i]][1]))
      nr <- as.numeric(p[[i]][2])
      mem <- write_to_mem2(mem = mem, mask = mask, address = address, nr = nr)
    }
  }
  return(mem)
}

write_to_mem2 <- function(mem, mask, address, nr){
  bin_add <- to_bin(x = address)
  add_masked <- apply_mask2(x = bin_add, mask = mask)
  all_add <- generate_all_addresses(x = add_masked)
  for (i in 1:length(all_add)){
    add_i <- bin2dec(all_add[[i]])
    mem[paste(add_i)] <- nr
  }
  return(mem)
}

apply_mask2 <- function(x, mask){
  ones <- which(mask == 1)
  x[ones] <- 1
  nas <- which(is.na(mask))
  x[nas] <- NA
  return(x)
}

generate_all_addresses <- function(x){
  n <- sum(is.na(x))
  l <- lapply(1:n, function(y){
    c(0, 1)
  })
  comb <- expand.grid(l)
  all_add <- list()
  for (i in 1:nrow(comb)){
    all_add[[i]] <- x
    all_add[[i]][is.na(all_add[[i]])] <- as.numeric(comb[i, ])
  }
  return(all_add)
}