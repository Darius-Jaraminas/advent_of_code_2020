run_code <- function(code, stop_iteration){
  code$iteration <- 0
  code$exe <- 0
  code$acc <- 0
  code[1, "exe"] <- 1
  op <- code[code$exe == max(code$exe), "operation"]
  
  stop_exe <- FALSE
  while (!stop_exe){
    op <- code[code$exe == max(code$exe), "operation"]
    if (op == "nop"){
      code <- exe_nop(code = code)
    }
    if (op == "acc"){
      code <- exe_acc(code = code)
    }
    if (op == "jmp"){
      code <- exe_jmp(code = code)
    }
    m <- max(code$exe)
    w <- which(code$exe == m)
    it <- code[w, "iteration"]
    if (it == (stop_iteration - 1) | is.na(code[nrow(code), "iteration"])){
      stop_exe <- TRUE
    }
  }
  return(code)
}

exe_nop <- function(code){
  m <- max(code$exe)
  w <- which(code$exe == m)
  code[w, "iteration"] <- code[w, "iteration"] + 1
  code[w + 1, "exe"] <- m + 1
  return(code)
}

exe_acc <- function(code){
  m <- max(code$exe)
  w <- which(code$exe == m)
  code[w, "iteration"] <- code[w, "iteration"] + 1
  code[w + 1, "exe"] <- m + 1
  code$acc <- code$acc + code[w, "argument"]
  return(code)
}

exe_jmp <- function(code){
  m <- max(code$exe)
  w <- which(code$exe == m)
  w_new <- w + code[w, "argument"]
  code[w, "iteration"] <- code[w, "iteration"] + 1
  code[w_new, "exe"] <- m + 1
  return(code)
}

change_operations <- function(code){
  non_acc <- which(code$operation != "acc")
  for (i in non_acc){
    code_i <- code
    op <- code_i[i, "operation"]
    code_i[i, "operation"] <- ifelse(op == "nop", "jmp", "nop")
    code_i <- run_code(code = code_i, stop_iteration = 2)
    if (which.max(code_i$exe) == nrow(code_i)){
      return(code_i)
    }
  }
  return(NULL)
}
