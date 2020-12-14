library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_program("test1.txt")

r1_t1 <- run_program(p = t1)
sum(r1_t1, na.rm = TRUE) == 165
# solve
inp <- read_program("input.txt")

r1 <- run_program(p = inp)
format(sum(r1, na.rm = TRUE), scientific = FALSE)
# 40234 too low

# part 2
# test
t2 <- read_program("test2.txt")

r2_t2 <- run_program2(p = t2)
sum(r2_t2, na.rm = TRUE) == 208
# solve
r2 <- run_program2(p = inp)
format(sum(r2, na.rm = TRUE), scientific = FALSE)
