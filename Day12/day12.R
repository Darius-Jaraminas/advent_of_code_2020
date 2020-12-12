library(dplyr)
library(tidyr)

source("fun.R")

# part 1
# test
t1 <- read.csv("test1.txt", check.names = FALSE, stringsAsFactors = FALSE,
               header = FALSE)[[1]]
r1_test <- move_ship(directions = t1)
sum(abs(r1_test)) == 25
# solve
inp <- read.csv("input.txt", check.names = FALSE, stringsAsFactors = FALSE,
                header = FALSE)[[1]]
r1 <- move_ship(directions = inp)
print(sum(abs(r1)))
# 1032

# part 2
# test
r2_test <- move_ship_with_waypoint(directions = t1, wp = c(10, -1))
sum(r2_test) == 286

#solve
r2 <- move_ship_with_waypoint(directions = inp, wp = c(10, -1))
print(sum(abs(r2)))
