library(dplyr)
library(tidyr)

source("fun.R")

# part 1
# test
con <- file("test1.txt")
t1 <- con %>%
  readLines() %>%
  prep_seats()
close(con)

r1_test <- run_simulation(x = t1, how = "adjecent", n_occ = 4)
sum(r1_test == "#") == 37

# solve
con <- file("input.txt")
inp <- con %>%
  readLines() %>%
  prep_seats()
close(con)

# r1 <- run_simulation(x = inp, how = "adjecent")
# print(sum(r1 == "#"))
# 2263

# part 2
# test
r2_test <- run_simulation(x = t1, how = "visible", n_occ = 5)
sum(r2_test == "#") == 26

# solve
r2 <- run_simulation(x = inp, how = "visible", n_occ = 5)
sum(r2 == "#")
# 
