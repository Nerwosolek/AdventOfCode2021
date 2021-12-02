source("setwd.R")
library(dplyr)
# Part I ------------------------------------------------------------------

moves <- read.table("data/input02.txt", header = F)
result <- tibble(x=0,y=0)
result$x <- moves %>% filter(V1 == "forward") %>% select(V2) %>% sum()
result$y <- result$y + moves %>% filter(V1 == "down") %>% select(V2) %>% sum()
result$y <- result$y - moves %>% filter(V1 == "up") %>% select(V2) %>% sum()
answer <- result$x * result$y
print(answer)


# Part II -----------------------------------------------------------------

result2 <- tibble(x=0,y=0,aim=0)
apply(moves,1,function(v){
#print(v)
  if (v[1] == "down")
    result2$aim <<- result2$aim + as.integer(v[2])
  else if (v[1] == "up")
    result2$aim <<- result2$aim - as.integer(v[2])
  else
  {
    result2$x <<- result2$x + as.integer(v[2])
    result2$y <<- result2$y + as.integer(v[2]) * result2$aim 
  }
})
answer2 <- result2$x * result2$y
print(answer2)
