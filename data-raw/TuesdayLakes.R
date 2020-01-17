## This script assumes that the user downloaded the webs WEB355 and WEB356 from the Global Web DB

library(readr)
library(tidyverse)

wrangle <- . %>%
  mutate_if(is_double,as.logical) %>%
  pivot_longer(-1) %>%
  filter(value) %>%
  select(-value) %>%
  as_tbl_graph()

TuesdayLakes_1984 <- read_csv("webs_globalwebdb/WEB355.csv",
                              skip = 1) %>%
  wrangle %>%
  convert(.f = to_simple, .clean = TRUE) %E>%
  select(-.orig_data)

TuesdayLakes_1986 <- read_csv("webs_globalwebdb/WEB356.csv",
                              skip = 1) %>%
  wrangle %>%
  convert(.f = to_simple, .clean = TRUE) %E>%
  select(-.orig_data)

usethis::use_data(TuesdayLakes_1984, TuesdayLakes_1986)
