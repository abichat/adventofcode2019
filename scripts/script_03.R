# Day 3: Crossed Wires
library(tidyverse)

#### Input ####

input <- readLines("inputs/input_03.txt")
str1 <- input[1]
str2 <- input[2]


#### First puzzle #### 

tbl_join <-
  tribble(~ direction, ~ vector,
                  "R",   c(1, 0),
                  "L",   c(-1, 0),
                  "U",   c(0, 1),
                  "D",   c(0, -1))

extract_path <- function(str){
  str %>% 
    str_split(",") %>% 
    getElement(1) %>% 
    str_split("", n = 2) %>% 
    transpose() %>% 
    map(unlist) %>%
    set_names(c("direction", "length")) %>%
    as_tibble() %>% 
    left_join(tbl_join, by = "direction") %>% 
    mutate(long_vector = map2(vector, length, rep),
           long_vector = map(long_vector, matrix, ncol = 2, byrow = TRUE)) %>% 
    pull(long_vector) %>% 
    reduce(rbind) %>% 
    apply(2, cumsum) %>% 
    `colnames<-`(c("x", "y")) %>%
    as_tibble()
}

path1 <- 
  str1 %>% 
  extract_path() %>% 
  unite(col = "glued", x, y, remove = FALSE)

path2 <- 
  str2 %>% 
  extract_path() %>% 
  unite(col = "glued", x, y, remove = FALSE)

path2 %>% 
  filter(glued %in% path1$glued) %>% 
  mutate(norm = abs(x) + abs(y)) %>% 
  pull(norm) %>% 
  setdiff(0:1) %>% 
  min()


#### Second puzzle ####

step1 <- 
  rowid_to_column(path1, var = "step1") %>% 
  select(glued, step1) %>% 
  filter(glued %in% path2$glued) 

step2 <- 
  rowid_to_column(path2, var = "step2") %>% 
  select(glued, step2) %>% 
  filter(glued %in% path1$glued)

step1 %>% 
  left_join(step2, by = "glued") %>% 
  mutate(total_step = step1 + step2) %>% 
  pull(total_step) %>% 
  min()

