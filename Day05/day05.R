library(dplyr)

source("fun.R")

# part 1
# test
t1 <- read.csv("test1.txt", header = FALSE, stringsAsFactors = FALSE)
row <- numeric(nrow(t1))
seat <- numeric(nrow(t1))
seat_id <- numeric(nrow(t1))
for (i in 1:nrow(t1)){
  row_coord <- clean_coord(board = t1[i, 1], subset = c(1, 7))
  row[i] <- find_binary(set = 0:127, coord = row_coord)
  seat_coord <- clean_coord(board = t1[i, 1], subset = c(8, 10))
  seat[i] <- find_binary(set = 0:7, coord = seat_coord)
  seat_id[i] <- row[i] * 8 + seat[i]
}
all(row == c(44, 70, 14, 102))
all(seat == c(5, 7, 7, 4))
all(seat_id == c(357, 567, 119, 820))
# solve
inp <- read.csv("input.txt", header = FALSE, stringsAsFactors = FALSE)
row <- numeric(nrow(inp))
seat <- numeric(nrow(inp))
seat_id <- numeric(nrow(inp))
for (i in 1:nrow(inp)){
  row_coord <- clean_coord(board = inp[i, 1], subset = c(1, 7))
  row[i] <- find_binary(set = 0:127, coord = row_coord)
  seat_coord <- clean_coord(board = inp[i, 1], subset = c(8, 10))
  seat[i] <- find_binary(set = 0:7, coord = seat_coord)
  seat_id[i] <- row[i] * 8 + seat[i]
}
max(seat_id)

# part 2
s_id <- sort(seat_id)
di <- diff(s_id)
my_seat <- s_id[which(di == 2)] + 1
