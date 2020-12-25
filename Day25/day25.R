library(dplyr)

source("fun.R")

# part 1
# test
t1 <- c(5764801, 17807724)
v <- transform_number(x = 7, k = 1e08)
print(which(v == t1[1]) == 8)
print(which(v == t1[2]) == 11)

# solve
inp <- c(11404017, 13768789)
print(which(v == inp[1])[1])
print(which(v == inp[2])[1])

loop_size <- c(which(v == inp[1])[1], which(v == inp[2])[1])

encryption_key1 <- transform_number(x = inp[1], k = loop_size[2])
encryption_key1 <- encryption_key1[loop_size[2]]
encryption_key2 <- transform_number(x = inp[2], k = loop_size[1])
encryption_key2 <- encryption_key2[loop_size[1]]
print(encryption_key1 == encryption_key2)
# 18862163
