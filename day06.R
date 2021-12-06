source("setwd.R")

f <- scan("data/input06.txt", sep = ",")
f <- f + 1
fish <- rep(0,9)
for(i in 1:9){
  fish[i] = length(which(f == i))
}

next_fish <- function(fish){
  nextf <- rep(0,9)
  nextf[1:8] <- fish[2:9]
  nextf[9] <- fish[1]
  nextf[7] <- nextf[7] + fish[1]
  nextf
}

for (n in 1:256){
  fish <- next_fish(fish)
}
answer <- sum(fish)
print(answer, digits = 20)