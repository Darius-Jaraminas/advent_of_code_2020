library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read.csv("test1.txt", header = FALSE)[[1]]
r1_test <- check_sums(x = t1, preamble = 5)
r1_test == 127

# solve
inp <- read.csv("input.txt", header = FALSE)[[1]]
r1 <- check_sums(x = inp, preamble = 25)
print(r1)
# 756008079

# part 2
# test
r2_test <- find_sum(x = t1, invalid_number = 127)
min(r2_test) + max(r2_test) == 62

# solve
r2 <- find_sum(x = inp, invalid_number = r1)
print(min(r2) + max(r2))
# 93727241
