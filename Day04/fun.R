separate_passports <- function(x){
  w <- which(x == "")
  from <- c(1, w + 1)
  to <- c(w - 1, length(x))
  p <- list()
  for (i in 1:length(from)){
    p[[i]] <- x[from[i]:to[i]]
  }
  return(p)
}

tidy_passports <- function(x){
  p <- lapply(x, function(y){
    y <- paste(y, collapse = " ")
    y <- strsplit(y, " ")[[1]]
    y <- data.frame(y)
    y <- separate(y, col = "y", into = c("field", "value"), sep = ":")
  })
  p <- bind_rows(p, .id = "id")
  return(p)
}

check_passports <- function(x, mandatory_fields, validate = FALSE){
  np <- n_distinct(x$id)
  uid <- unique(x$id)
  valid <- logical(np)
  for (i in 1:np){
    pi <- x[x$id == uid[i], ]
    valid[i] <- all(mandatory_fields %in% pi$field)
    if (valid[i] & validate){
      valid[i] <- validate_passport(x = pi)
    }
  }
  return(valid)
}

validate_passport <- function(x){
  v1 <- as.numeric(x[x$field == "byr", "value"])
  t1 <- v1 >= 1920 & v1 <= 2002
  
  v2 <- as.numeric(x[x$field == "iyr", "value"])
  t2 <- v2 >= 2010 & v2 <= 2020
  
  v3 <- as.numeric(x[x$field == "eyr", "value"])
  t3 <- v3 >= 2020 & v3 <= 2030

  v4 <- x[x$field == "hgt", "value"]
  t4 <- grepl("in$|cm$", v4)
  if (t4){
    if (grepl("cm$", v4)){
      v4 <- as.numeric(gsub("cm$", "", v4))
      t4 <- v4 >= 150 & v4 <= 193
    } 
    if (grepl("in$", v4)){
      v4 <- as.numeric(gsub("in$", "", v4))
      t4 <- v4 >= 59 & v4 <= 76
    }
  }
  v5 <- x[x$field == "hcl", "value"]
  t5 <- grepl("^\\#[[:xdigit:]]{6}", v5)
  
  v6 <- x[x$field == "ecl", "value"]
  t6 <- v6 %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  
  v7 <- x[x$field == "pid", "value"]
  t7 <- grepl("^[[:digit:]]{9}$", v7)
  
  valid <- t1 & t2 & t3 & t4 & t5 & t6 & t7
  return(valid)
}
