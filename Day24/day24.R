library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_tiles("test1.txt")
r1_t1 <- flip_tiles(x = t1)
print(sum(r1_t1) == 10)
# solve
inp <- read_tiles("input.txt")
r1 <- flip_tiles(x = inp)
print(sum(r1))
# 307

# part 2
# test
r2_t1 <- living_art_exhibit(g = r1_t1, days = 100)
print(sum(r2_t1) == 2208)
# solve
r2 <- living_art_exhibit(g = r1, days = 100)
print(sum(r2))
# 3787
