library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read.csv("test1.txt", header = FALSE)[[1]]
r1_test <- t1 %>%
  get_differences() %>%
  table()
r1_test["1"] == 7 & r1_test["3"] == 5

t2 <- read.csv("test2.txt", header = FALSE)[[1]]
r2_test <- t2 %>%
  get_differences() %>%
  table()
r2_test["1"] == 22 & r2_test["3"] == 10
# solve
inp <- read.csv("input.txt", header = FALSE)[[1]]
r1 <- inp %>%
  get_differences() %>%
  table()
r1["1"] * r1["3"]
# 2244

# part 2
# test
r2_t1 <- t1 %>%
  get_differences() %>%
  count_combinations()
r2_t1 == 8

r2_t2 <- t2 %>%
  get_differences() %>%
  count_combinations()
r2_t2 == 19208

# solve
r2 <- inp %>%
  get_differences() %>%
  count_combinations()
print(format(r2, scientific = FALSE))
# 3947645370368
