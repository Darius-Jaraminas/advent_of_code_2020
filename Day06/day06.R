library(dplyr)

source("fun.R")

# part 1
# test
t1 <- "test1.txt" %>%
  file() %>%
  readLines() %>%
  clean_cdf()

r1_test <- count_uniques(x = t1)
sum(r1_test) == 11
# solve
inp <- "input.txt" %>%
  file() %>%
  readLines() %>%
  clean_cdf()

r1 <- count_uniques(x = inp)
sum(r1)

# part 2
# test
r2_test <- count_repeats(x = t1)
sum(r2_test) == 6

r2<- count_repeats(x = inp)
sum(r2)
