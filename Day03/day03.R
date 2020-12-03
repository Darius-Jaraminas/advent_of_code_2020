library(dplyr)

source("fun.R")

# part 1
# test
t1 <- "test1.txt" %>%
  file() %>%
  readLines() %>%
  prep_map()
t1_path <- check_angle(map = t1, right = 3, down = 1)
r1_test <- sum(t1_path == "#")
r1_test == 7

# solve
inp <- "input.txt" %>%
  file() %>%
  readLines() %>%
  prep_map()
path <- check_angle(map = inp, right = 3, down = 1)
r1 <- sum(path == "#")
# 276

# part 2
all_right <- c(1, 3, 5, 7, 1)
all_down <- c(1, 1, 1, 1, 2)
n_steps <- length(all_down)
# test
r_test <- numeric(n_steps)
for (p in 1:n_steps){
  t_path <- check_angle(map = t1, right = all_right[p], down = all_down[p])
  r_test[p] <- sum(t_path == "#")
}
prod(r_test) == 336

# solve
r <- numeric(n_steps)
for (p in 1:n_steps){
  path <- check_angle(map = inp, right = all_right[p], down = all_down[p])
  r[p] <- sum(path == "#")
}
r <- as.numeric(r)
prod(r)
# 7812180000
