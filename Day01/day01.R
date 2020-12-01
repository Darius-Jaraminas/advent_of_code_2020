
# part 1
fnm_input <- "input.txt"
report <- read.csv(fnm_input, header = FALSE)
# test
test <- c(1721, 979, 366, 299, 675, 1456)
dif_test <- 2020 - test
r1_test <- prod(test[dif_test %in% test])
r1_test == 514579
# solve
report <- report[[1]]
dif <- 2020 - report
r1 <- prod(report[dif %in% report])
# 1013211

# part 2
fnm_input <- "input.txt"
report <- read.csv(fnm_input, header = FALSE)
# test
all_combs <- expand.grid(test, test, test)
w <- rowSums(all_combs) == 2020
nrs <- as.numeric(all_combs[which(w)[1], ])
r2_test <- prod(nrs)
r2_test == 241861950
# solve
report <- report[[1]]
all_combs <- expand.grid(report, report, report)
w <- rowSums(all_combs) == 2020
nrs <- as.numeric(all_combs[which(w)[1], ])
r2 <- prod(nrs)
# 13891280
