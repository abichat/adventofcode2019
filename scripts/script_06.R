# Day 6: Universal Orbit Map

#### Input ####

input <- readLines("inputs/input_06.txt")
el <- as.data.frame(matrix(unlist(strsplit(input, split = ")")), 
                           ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
all_edges <- unique(unlist(el))


#### First puzzle #### 

indirect_orbits <- function(edge) {
  sub_el <- el[el$V2 == edge,]
  if (nrow(sub_el) == 0) {
    return(c())
  } else {
    return(c(edge, indirect_orbits(sub_el$V1)))
  }
}

sum(sapply(sapply(all_edges, indirect_orbits), length))


#### Second puzzle ####

io_YOU <- indirect_orbits("YOU") # inspired from @RLesur
io_SAN <- indirect_orbits("SAN")

length(union(setdiff(io_YOU, io_SAN), setdiff(io_SAN, io_YOU))) - 2
