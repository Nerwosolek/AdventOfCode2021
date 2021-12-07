source("setwd.R")


# Part I ------------------------------------------------------------------

f <- scan("data/input07.txt", sep = ",")

align <- function(positions, toPos){
  sum(abs(positions-toPos))
}
minPos <- 0
minFuel <- Inf
for (pos in 0:1986){
  fuel <- align(f,pos)
  if (fuel < minFuel){
    minPos <- pos
    minFuel <- fuel
  }
}
print(minFuel)

# Part II -----------------------------------------------------------------

fuel_burnt <- function(change){
  (change+1)*change/2
}
align2 <- function(positions, toPos){
  
  sum(fuel_burnt(abs(positions-toPos)))
}
minPos <- 0
minFuel <- Inf
for (pos in 0:1986){
  fuel <- align2(f,pos)
  if (fuel < minFuel){
    minPos <- pos
    minFuel <- fuel
  }
}
print(minFuel)


