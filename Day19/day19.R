library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read_sat("test1.txt")
t1$rules <- expand_rules(x = t1$rules)

r1_t1 <- check_messages(x = t1)
print(all(r1_t1 == c(TRUE, FALSE, TRUE, FALSE, FALSE)))
# solve
inp <- read_sat("input.txt")
inp$rules <- expand_rules(x = inp$rules)

r1 <- check_messages(x = inp)
print(sum(r1))
# 222

# part 2
# test
t2 <- read_sat("test2.txt")
t2$rules <- expand_rules(x = t2$rules)

r21_t2 <- check_messages(x = t2)
sum(r21_t2) == 3

t2 <- read_sat("test2.txt")
t2$rules <- expand_rules(x = t2$rules)

r22_t2 <- check_messages_2(x = t2)
valid_t2 <- c("bbabbbbaabaabba",
              "babbbbaabbbbbabbbbbbaabaaabaaa",
              "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
              "bbbbbbbaaaabbbbaaabbabaaa",
              "bbbababbbbaaaaaaaabbababaaababaabab",
              "ababaaaaaabaaab",
              "ababaaaaabbbaba",
              "baabbaaaabbaaaababbaababb",
              "abbbbabbbbaaaababbbbbbaaaababb",
              "aaaaabbaabaaaaababaa",
              "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
              "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")
w_valid_t2 <- which(t2$mes %in% valid_t2)
print(all(which(r22_t2) == w_valid_t2))
print(sum(r22_t2) == 12)

# solve
inp <- read_sat("input.txt")
inp$rules <- expand_rules(x = inp$rules)

r2 <- check_messages_2(x = inp)
print(sum(r2))
# 339
