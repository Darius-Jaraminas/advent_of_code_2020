library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_cards("test1.txt")
r1_t1 <- play_combat(x = t1)
print(r1_t1 == 306)

# solve
inp <- read_cards("input.txt")
r1 <- play_combat(x = inp)
print(r1)
# 31809

# part 2
# test
r2_t1 <- play_recursive_combat(x = t1)
r2_t1_score <- sum(r2_t1[[r2_t1$winner]] * (length(r2_t1[[r2_t1$winner]]):1))
print(r2_t1_score == 291)
t2 <- read_cards("test2.txt")
r2_t2 <- play_recursive_combat(x = t2)
print(r2_t2$winner == "p1")

# solve
r2 <- play_recursive_combat(x = inp)
score <- sum(r2[[r2$winner]] * (length(r2[[r2$winner]]):1))
print(score)
# 32835