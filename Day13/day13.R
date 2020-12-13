library(dplyr)
library(tidyr)
library(pracma)

source("fun.R")

# part 1
#test
t1 <- read_schedule("test1.txt", remx = TRUE)
r1_test <- find_closest_bus(x = t1)
r1_test == 295

# solve
inp <- read_schedule("input.txt", remx = TRUE)
r1 <- find_closest_bus(x = inp)
print(r1)
# 2095

# part 2
# test
t1 <- read_schedule("test1.txt", remx = FALSE)$schedule
r2_t1 <- find_t(x = t1)
r2_t1 == 1068781

t2 <- read_schedule("test2.txt", remx = FALSE)$schedule
r2_t2 <- find_t(x = t2)
r2_t2 == 3417

t3 <- read_schedule("test3.txt", remx = FALSE)$schedule
r2_t3 <- find_t(x = t3)
r2_t3 == 754018

t4 <- read_schedule("test4.txt", remx = FALSE)$schedule
r2_t4 <- find_t(x = t4)
r2_t4 == 779210

t5 <- read_schedule("test5.txt", remx = FALSE)$schedule
r2_t5 <- find_t(x = t5)
r2_t5 == 1261476

t6 <- read_schedule("test6.txt", remx = FALSE)$schedule
r2_t6 <- find_t(x = t6)
r2_t6 == 1202161486

# solve
inp <- read_schedule("input.txt", remx = FALSE)$schedule
r2 <- find_t(x = inp)
print(format(r2, scientific = FALSE))
# 598411311431841
