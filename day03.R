source("setwd.R")
library(readr)
library(stringr)
library(dplyr)

# Part I ------------------------------------------------------------------

bits <- read_csv("data/input03.txt", col_names = c("byte"), 
                 col_types = cols(byte = col_character()))
bits_mat <- str_split_fixed(bits$byte,"",n=12)

most_bit <- function(col) {
  ifelse(length(which(col == "1")) >= length(col)/2, "1", "0")
}

least_bit <- function(col) {
  ifelse(length(which(col == "0")) <= length(col)/2, "0", "1")
}

gamma_bits <- apply(bits_mat,2, most_bit)
gamma <- str_c(gamma_bits,sep="",collapse = "")
gamma_dec <- strtoi(gamma, base = 2)

epsilon_bits <- apply(bits_mat,2, least_bit)
epsilon <- str_c(epsilon_bits,sep="",collapse = "")
epsilon_dec <- strtoi(epsilon, base = 2)

answer1 <- gamma_dec * epsilon_dec
print(answer1)


# Part II -----------------------------------------------------------------


filter_bits <- function(bitmat, bitfunc) {
  cind <- 1
  while(nrow(bitmat) > 1)
  {
    bitmat <- bitmat[which(bitmat[,cind] == bitfunc(bitmat[,cind])),,drop=F]
    cind <- cind + 1
  }
  bitmat
} 

oxy <- strtoi(str_c(filter_bits(bits_mat, most_bit), sep="",collapse = ""), base = 2)
co2 <- strtoi(str_c(filter_bits(bits_mat, least_bit), sep="",collapse = ""), base = 2) 
answer2 <- co2 * oxy
print(answer2)
