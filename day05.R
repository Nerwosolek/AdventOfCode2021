source("setwd.R")
library(stringr)

# Part I ------------------------------------------------------------------

f <- scan(file = "data/input05.txt",what = character(),sep = "\n")

segments <- str_split_fixed(f, ",|\\s->\\s", n = 4)

segments <- matrix(as.numeric(segments), ncol = 4)

seg_hor_ver <- segments[which((segments[,2] == segments[,4] | segments[,1] == segments[,3])),]
seg_hor <- segments[which(segments[,2] == segments[,4]),]
seg_ver <- segments[which(segments[,1] == segments[,3]),]
seg_diag <- segments[which(segments[,1] != segments[,3] & segments[,2] != segments[,4]),]
mat <- matrix(0, nrow = 1000, ncol = 1000)
points_cnt <- 0
for (x in 1:1000){
  cat("\r",x)
  for (y in 1:1000){
    c <- 0
    rows <- which(y == seg_hor[,2]) 
    for (r in rows){
      if (x >= min(seg_hor[r,1], seg_hor[r,3]) & x <= max(seg_hor[r,1], seg_hor[r,3]))
        c <- c + 1
    }
    rows <- which(x == seg_ver[,1])
    for (r in rows){
      if (y >= min(seg_ver[r,2], seg_ver[r,4]) & y <= max(seg_ver[r,2], seg_ver[r,4]))
        c <- c + 1
    }
    c <- c + length(which((sign(x - seg_diag[,3]) == sign(seg_diag[,1] - seg_diag[,3]) | x - seg_diag[,3] == 0) &
                            (sign(y - seg_diag[,4]) == sign(seg_diag[,2] - seg_diag[,4]) | y - seg_diag[,4] == 0) &
                            (sign(x - seg_diag[,1]) == sign(seg_diag[,3] - seg_diag[,1]) | x - seg_diag[,1] == 0) &
                            (sign(y - seg_diag[,2]) == sign(seg_diag[,4] - seg_diag[,2]) | y - seg_diag[,2] == 0) &
                            abs(x - seg_diag[,3]) == abs(y - seg_diag[,4])))
    if (c > 1){
      points_cnt <- points_cnt + 1
    }
  }
}