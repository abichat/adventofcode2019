# Day 1: The Tyranny of the Rocket Equation

#### Input ####

input <- as.numeric(readLines("inputs/input_01.txt"))


#### First puzzle #### 

sum(floor(input / 3) - 2)


#### Second puzzle ####

required_fuel <- function(mass) {
  floor(mass / 3) - 2
}

total_required_fuel <- function(mass) {
  supp <- required_fuel(mass)
  if (supp <= 0) {
    return(0)
  } else {
    return(supp + total_required_fuel(supp))
  }
}

sum(sapply(input, total_required_fuel))

