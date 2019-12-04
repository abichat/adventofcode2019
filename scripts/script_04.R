# Day 4: Secure Container
library(tidyverse)

#### Input ####

input <- as.numeric(strsplit(readLines("inputs/input_04.txt"), "-")[[1]])


#### First puzzle #### 

tbl_filtered <-
  tibble(x = input[1]:input[2]) %>% 
  mutate(digits = str_split(input[1]:input[2], pattern = ""),
         d1 = map_chr(digits, 1),
         d2 = map_chr(digits, 2)) %>% 
  filter(d2 >= d1) %>% 
  mutate(d3 = map_chr(digits, 3)) %>% 
  filter(d3 >= d2) %>% 
  mutate(d4 = map_chr(digits, 4)) %>% 
  filter(d4 >= d3) %>% 
  mutate(d5 = map_chr(digits, 5)) %>% 
  filter(d5 >= d4) %>% 
  mutate(d6 = map_chr(digits, 6)) %>% 
  filter(d6 >= d5) %>% 
  select(-digits) %>% 
  mutate(e12 = d1 == d2,
         e23 = d2 == d3,
         e34 = d3 == d4,
         e45 = d4 == d5,
         e56 = d5 == d6,
         ettl = e12 + e23 + e34 + e45 + e56) %>% 
  filter(ettl >= 1)
  
nrow(tbl_filtered)
  

#### Second puzzle #### 

tbl_filtered %>% 
  filter(ettl < 5) %>% 
  select(-starts_with("e")) %>% 
  pivot_longer(-x) %>% 
  group_by(x) %>% 
  count(value) %>% 
  filter(n == 2) %>% 
  pull(x) %>% 
  unique() %>% 
  length()

