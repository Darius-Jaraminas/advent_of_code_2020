library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_food("test1.txt")
poss <- find_possible(x = t1)
r1_t1 <- count_occurences(x = t1, poss = poss)
print(r1_t1 == 5)

# solve
inp <- read_food("input.txt")
poss <- find_possible(x = inp)
r1 <- count_occurences(x = inp, poss = poss)
print(r1)
# 1829

# part 2
# test
t1 <- read_food("test1.txt")
poss <- find_possible(x = t1)
r2_t1 <- match_ing_with_alle(x = poss)
print(r2_t1 == "mxmxvkd,sqjhc,fvjkl")

# solve
inp <- read_food("input.txt")
poss <- find_possible(x = inp)
r2 <- match_ing_with_alle(x = poss)
print(r2)
# mxkh,gkcqxs,bvh,sp,rgc,krjn,bpbdlmg,tdbcfb
