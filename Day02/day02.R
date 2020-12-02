
library(tidyr)

source("fun.R")

# part 1
fnm_input <- "input.txt"
psw <- read.csv(fnm_input, header = FALSE)
# test
fnm_t1 <- "test1.txt"
t1 <- read.csv(fnm_t1, header = FALSE)
t1 <- sep_data_columns(x = t1)
t1 <- check_policy_part1(x = t1)
r1_test <- sum(t1$valid)
r1_test == 2
# solve
psw <- sep_data_columns(x = psw)
psw <- check_policy_part1(x = psw)
r1 <- sum(psw$valid)

# part 2
# test
t1 <- check_policy_part2(x = t1)
r2_test <- sum(t1$valid)
r2_test == 1
# solve
psw <- check_policy_part2(x = psw)
r2 <- sum(psw$valid)
