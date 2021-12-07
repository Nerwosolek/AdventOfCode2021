source("setwd.R")


# Part I ------------------------------------------------------------------

f <- scan("data/input07.txt", sep = ",")

align <- function(positions, toPos){
  sum(abs(positions-toPos))
}
for (pos in 0:1986){
  minPos <- 0
  minFuel <- Inf
  fuel <- align(f,pos)
  if (fuel < minFuel){
    minPos <- pos
    minFuel <- fuel
  }
  return (minPos)
}


