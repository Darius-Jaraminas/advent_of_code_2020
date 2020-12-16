library(dplyr)
library(tidyr)

source("fun.R")

# part 1
# test
t1 <- read_ticket("test1.txt")

r1_t1 <- find_invalid_values(x = t1)
sum(r1_t1$not_valid) == 71
# solve
inp <- read_ticket("input.txt")

r1 <- find_invalid_values(x = inp)
sum(r1$not_valid)
# 23954

# part 2
# test
t2 <- read_ticket("test2.txt")
t2$tickets[[length(t2$tickets) + 1]] <- t2$my
t2_v <- find_invalid_values(x = t2)
r2_t2 <- identify_fields(v = t2_v$valid)
all(r2_t2 == c("row", "class", "seat"))

# solve
inp <- read_ticket("input.txt")
inp$tickets[[length(inp$tickets) + 1]] <- inp$my
inp_v <- find_invalid_values(x = inp)
r2_attr <- identify_fields(v = inp_v$valid)
w_dep <- grepl("departure", r2_attr)
r2 <- prod(inp$my[w_dep])
print(r2)
# 453459307723
