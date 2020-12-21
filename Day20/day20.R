library(dplyr)
library(tidyr)

source("fun.R")

# part 1
# test
t1 <- read_tiles("test1.txt")
ac <- all_combs(x = t1)
ac <- find_matching(x = ac)
corners <- ac %>%
  filter(!is.na(match)) %>%
  group_by(tile) %>%
  summarise(n_sides = n_distinct(side), .groups = "drop") %>%
  filter(n_sides == 2)

r1_t1 <- prod(corners$tile)
print(r1_t1 == 20899048083289)

# solve
inp <- read_tiles("input.txt")
ac <- all_combs(x = inp)
ac <- find_matching(x = ac)
corners <- ac %>%
  filter(!is.na(match)) %>%
  group_by(tile) %>%
  summarise(n_sides = n_distinct(side), .groups = "drop") %>%
  filter(n_sides == 2)

r1 <- prod(corners$tile)
print(format(r1, scientific = FALSE))
# 17712468069479

# part 2
m <- read_monster("monster.txt")

# test
t1 <- read_tiles("test1.txt")
ac <- all_combs(x = t1)
ac <- find_matching(x = ac)
pic <- put_together(x = t1, ac = ac)
pic <- find_pic_orientation(x = pic, m = m)
print(sum(pic == "#") == 273)

# solve
inp <- read_tiles("input.txt")
ac <- all_combs(x = inp)
ac <- find_matching(x = ac)
pic <- put_together(x = inp, ac = ac)
pic <- find_pic_orientation(x = pic, m = m)
print(sum(pic == "#"))
# 2173
