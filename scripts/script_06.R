# Day 6: Universal Orbit Map

#### Input ####

input <- readLines("inputs/input_06.txt")
el <- as.data.frame(matrix(unlist(strsplit(input, split = ")")), 
                           ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
all_edges <- unique(unlist(el))


#### First puzzle #### 

n_orbits <- function(edge) {
  sub_el <- el[el$V2 == edge,]
  if (nrow(sub_el) == 0) {
    return(0)
  } else {
    return(1 + n_orbits(sub_el$V1))
  }
}

sum(sapply(all_edges, n_orbits))


#### Second puzzle ####

am <- matrix(0, nrow = length(all_edges), ncol = length(all_edges),
             dimnames = list(all_edges, all_edges))

fill_am <- function(x) {
  am[x[1], x[2]] <<- 1
  am[x[2], x[1]] <<- 1
  invisible()
}

invisible(apply(el, 1, fill_am))

amp <- am
i <- 1

while(amp["YOU", "SAN"] == 0){
  amp <- am %*% amp
  i <- i + 1
}

i - 2
