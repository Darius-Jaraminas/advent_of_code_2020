library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read.csv("test1.txt", stringsAsFactors = FALSE, sep = " ", header = FALSE)
t1 <- rename(t1, operation = V1, argument = V2)

r1_test <- run_code(code = t1, stop_iteration = 2)
r1_test[1, "acc"] == 5

# solve
inp <- read.csv("input.txt", stringsAsFactors = FALSE,
                sep = " ", header = FALSE)
inp <- rename(inp, operation = V1, argument = V2)

r1 <- run_code(code = inp, stop_iteration = 2)
r1[1, "acc"]
# 1446

# part 2
# test
r2_test <- change_operations(code = t1)
r2_test[1, "acc"] == 8
# solve
r2 <- change_operations(code = inp)
r2[1, "acc"]
# 1403
