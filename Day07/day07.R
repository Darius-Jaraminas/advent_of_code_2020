library(dplyr)
library(tidyr)

source("fun.R")

# part 1
# test
con <- file("test1.txt")
t1 <- con %>%
  readLines() %>%
  clean_rules()
close(con)

r1_t1 <- find_which_can_carry(rules = t1, bag_col = "shiny gold")
length(r1_t1) == 4
# solve
con <- file("input.txt")
inp <- con %>%
  readLines() %>%
  clean_rules()
close(con)

r1 <- find_which_can_carry(rules = inp, bag_col = "shiny gold")
print(length(r1))
# 164

# phase 2
# test
con <- file("test1.txt")
t21 <- con %>%
  readLines() %>%
  clean_rules_with_numbers()
close(con)

r2_t1 <- count_bags(bag_col = "shiny gold", rules = t21)
r2_t1 == 32

con <- file("test2.txt")
t22 <- con %>%
  readLines() %>%
  clean_rules_with_numbers()
close(con)

r2_t2 <- count_bags(bag_col = "shiny gold", rules = t22)
r2_t2 == 126

# solve
con <- file("input.txt")
inp2 <- con %>%
  readLines() %>%
  clean_rules_with_numbers()
close(con)

r2 <- count_bags(bag_col = "shiny gold", rules = inp2)
print(r2)
# 7872
