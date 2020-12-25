library(dplyr)

source("fun.R")

# part 1
# test
t1 <- c(3, 8, 9, 1, 2, 5, 4, 6, 7)
r1_t11 <- play_cups(x = t1, k = 10)
print(r1_t11 == "92658374")
r1_t12 <- play_cups(x = t1, k = 100)
print(r1_t12 == "67384529")

# solve
inp <- c(7, 9, 2, 8, 4, 5, 1, 3, 6)
r1 <- play_cups(x = inp, k = 100)
print(r1)
# 98742365

# part 2
# test
r2_t1 <- play_cups_for_real4(x = t1, k = 1e07, add = 1e06)
print(r2_t1 == 149245887792)

# solve
r2 <- play_cups_for_real4(x = inp, k = 1e07, add = 1e06)
print(r2)
# 294320513093
