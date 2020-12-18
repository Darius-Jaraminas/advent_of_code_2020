library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_math("test1.txt")
r1_values <- c(71, 51, 26, 437, 12240, 13632)
r1_t1 <- numeric()
for (i in t1){
  r1_t1 <- c(r1_t1, evaluate_expression(i))
}
all(r1_t1 == r1_values)

# solve
inp <- read_math("input.txt")
r1 <- numeric()
for (i in inp){
  r1 <- c(r1, evaluate_expression(i))
}
print(format(sum(r1), scientific = FALSE))
# 14006719520523

# part 2
# test
t2 <- addition_first(x = t1)
r2_values <- c(231, 51, 46, 1445, 669060, 23340)
r2_t2 <- numeric()
for (i in t2){
  r2_t2 <- c(r2_t2, evaluate_expression(i))
}
all(r2_t2 == r2_values)

# solve
inp2 <- addition_first(x = inp)
r2 <- numeric()
for (i in inp2){
  r2 <- c(r2, evaluate_expression(i))
}
print(format(sum(r2), scientific = FALSE))
# 545115449981968
