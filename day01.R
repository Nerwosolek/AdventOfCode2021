source("setwd.R")

# Part I ------------------------------------------------------------------

depths <- read.table("data/input01.txt", header = F)
depths_next <- depths$V1[2:length(depths$V1)]

depths <- depths[-nrow(depths),,drop=F]
depths$next_val <- depths_next
result <- length(which(depths$V1 < depths$next_val))
print(result)

# Part II -----------------------------------------------------------------
library(zoo)
depths_2 <- read.table("data/input01.txt", header = F)
depths2_roll <- rollapply(depths_2, 3, sum)
depths2_inc <- rollapply(depths2_roll, 2, function(x) {
  x[1] < x[2]
})
result2 <- length(which(depths2_inc == T))
print(result2)
