library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_dimension("test1.txt")
t1 <- create_cube(x = t1, d = 6)
nst <- t1
for (i in 1:6){
  nst <- change_state(cube = nst)
}
r1_t1 <- sum(nst == "#")
r1_t1 == 112

# solve
inp <- read_dimension("input.txt")
inp <- create_cube(x = inp, d = 6)
nst <- inp
for (i in 1:6){
  nst <- change_state(cube = nst)
}
r1 <- sum(nst == "#")
print(r1)
# 232

# part 2
# test
t1 <- read_dimension("test1.txt")
t1 <- create_cube_4(x = t1, d = 6)
nst <- t1
for (i in 1:6){
  nst <- change_state_4(cube = nst)
}
r2_t1 <- sum(nst == "#")
r2_t1 == 848

# solve
inp <- read_dimension("input.txt")
inp <- create_cube_4(x = inp, d = 6)
nst <- inp
for (i in 1:6){
  nst <- change_state_4(cube = nst)
}
r2 <- sum(nst == "#")
print(r2)
# 1620
