# Day 2: 1202 Program Alarm

#### Input ####

input <- as.numeric(unlist(strsplit(readLines("inputs/input_02.txt"), ",")))


#### First puzzle ####

N <- length(input)
vec <- input
vec[2] <- 12
vec[3] <- 2
i <- 1

while(i <= N) {
  if (vec[i] == 1) {
    vec[vec[i + 3] + 1] <- vec[vec[i + 1] + 1] + vec[vec[i + 2] + 1]
    i <- i + 4
  } else if (vec[i] == 2) {
    vec[vec[i + 3] + 1] <- vec[vec[i + 1] + 1] * vec[vec[i + 2] + 1]
    i <- i + 4
  } else if (vec[i] == 99) {
    break
  } else {
    stop("Something went wrong")
  }
}

vec[1]


#### Second puzzle ####

update_memory <- function(vector) {
  vec <- vector
  N <- length(vec)
  i <- 1
  while (i <= N) {
    if (vec[i] == 1) {
      vec[vec[i + 3] + 1] <- vec[vec[i + 1] + 1] + vec[vec[i + 2] + 1]
      i <- i + 4
    } else if (vec[i] == 2) {
      vec[vec[i + 3] + 1] <- vec[vec[i + 1] + 1] * vec[vec[i + 2] + 1]
      i <- i + 4
    } else if (vec[i] == 99) {
      break
    } else {
      stop("Something went wrong")
    }
  }
  vec
}

breaking_output <- 19690720
stop <- FALSE

for(i in 0:99) {
  for (j in 0:99) {
    vec <- input
    vec[2] <- i
    vec[3] <- j
    vec <- update_memory(vec)
    if (vec[1] == breaking_output) {
      stop <- TRUE
      break
    }
  }
  if(stop){
    break
  }
}

100 * i + j

