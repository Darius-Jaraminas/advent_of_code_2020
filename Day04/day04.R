library(dplyr)
library(tidyr)

source("fun.R")

# part 1
mf <- c("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
# cid - non mandatory

# test
t1 <- file("test1.txt") %>%
  readLines() %>% 
  separate_passports() %>%
  tidy_passports()

r1_test <- check_passports(x = t1, mandatory_fields = mf, validate = FALSE)
sum(r1_test) == 2
# solve
inp <- file("input.txt") %>%
  readLines() %>% 
  separate_passports() %>%
  tidy_passports()

r1 <- check_passports(x = inp, mandatory_fields = mf, validate = FALSE)
sum(r1)
# 247

# part 2
# test
t2 <- file("test2.txt") %>%
  readLines() %>% 
  separate_passports() %>%
  tidy_passports()
r2_test <- check_passports(x = t2, mandatory_fields = mf, validate = TRUE)
all(which(r2_test) == 5:8)

# solve
r2 <- check_passports(x = inp, mandatory_fields = mf, validate = TRUE)
sum(r2)
# 145
