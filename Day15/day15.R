library(dplyr)

source("fun.R")

# part 1
# test
k <- 1:7
N <- 2020
r1_test <- c(436, 1, 10, 27, 78, 438, 1836)
for (i in k){
  ti <- read_starting(paste0("test", i, ".txt"))
  r_ti <- play_game(starting = ti, turn = N)
  correct <- r_ti == r1_test[i]
  print(correct)
}

# solve
inp <- read_starting("input.txt")
r1 <- play_game(starting = inp, turn = N)
print(r1)
# 1428

# part 2
# test
N <- 30000000
r2_test <- c(175594, 2578, 3544142, 261214, 6895259, 18, 362)
for (i in k){
  ti <- read_starting(paste0("test", i, ".txt"))
  r_ti <- play_game(starting = ti, turn = N)
  correct <- r_ti == r2_test[i]
  print(correct)
}

# solve
inp <- read_starting("input.txt")
r2 <- play_game(starting = inp, turn = N)
print(r2)
# 3718541
