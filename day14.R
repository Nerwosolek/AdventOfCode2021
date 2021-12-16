source("setwd.R")
library(stringr)
library(dplyr)
# Part I ------------------------------------------------------------------

f <- scan("data/input14.txt", what = character())

template <- str_split_fixed(f[1], "", n=nchar(f[1]))[1,]
mapping <- f[seq(4,301,3)]
names(mapping) <- f[seq(2,299,3)]

expand <- function(template) {
  new_template <- vector(mode = "character", length = 2 * length(template) - 1)
  new_template[seq(1,length(new_template),2)] <- template
  new_template
}

map <- function(template) {
  str_c(template[1:length(template)-1], template[2:length(template)])
  unname(mapping[str_c(template[1:length(template)-1], template[2:length(template)])])
}

insert <- function(template, letters) {
  new_template <- expand(template)
  new_template[seq(2,length(new_template)-1,2)] <- letters
  new_template
}
run_template <- template
for (i in 1:1) {
  run_template <- insert(run_template, map(run_template))  
}
ranking <- sort(table(run_template))
print(unname(ranking[length(ranking)] - ranking[1]))

# Part II -----------------------------------------------------------------

initial_pairs <- str_c(template[1:length(template)-1], template[2:length(template)])
counters <- rep(0,100)
names(counters) <- names(mapping)
counters[initial_pairs] = 1
letters_cnt <- rep(0,10)
names(letters_cnt) <- unique(c(mapping, template))

init_counters <- function(template, mapping){
  counters <- rep(0,100)
  names(counters) <- names(mapping)
  initial_pairs <- table(str_c(template[1:length(template)-1], template[2:length(template)]))
  counters[names(initial_pairs)] = unname(initial_pairs)
  counters
}

init_letters_cnt <- function(init_template){
  letters_cnt <- rep(0,10)
  names(letters_cnt) <- unique(c(mapping, init_template))
  letters_cnt[names(table(init_template))] <- table(init_template)
  letters_cnt
}

transfer <- function(counters){
  new_counters <- counters * 0
  new_counters <- c(rbind(counters[which(counters > 0)], counters[which(counters > 0)]))
  names(new_counters) <- c(rbind(names(counters[which(counters > 0)]), names(counters[which(counters > 0)])))
  new_counters
}

join_pairs <- function(template){
  str_c(template[1:length(template)-1], template[2:length(template)])
}



gen_new_letters_cnt <- function(counters, mapping, letters_cnt){
  temp_lett <- unname(counters[counters > 0])
  names(temp_lett) <- unname(mapping[names(counters[which(counters > 0)])])

  df_letters <- data.frame(letters = names(temp_lett), cnt = unname(temp_lett))
  df_letters <- df_letters %>% group_by(letters) %>% summarise(sum(cnt))
  letters_freq <- df_letters$`sum(cnt)`
  names(letters_freq) <- df_letters$letters
  letters_cnt[names(letters_freq)] <- letters_cnt[names(letters_freq)] + letters_freq
  letters_cnt
}

gen_new_pairs_cnt <- function(counters, mapping) {
  ll <- unname(mapping[names(counters[which(counters > 0)])])
  pp <- names(counters[which(counters > 0)])
  
  new_countersA <- unname(counters[which(counters > 0)])
  names(new_countersA) <- str_c(str_sub(pp, 1,1), ll)

  new_countersB <- unname(counters[which(counters > 0)])
  names(new_countersB) <- str_c(ll, str_sub(pp, 2,2))

  new_countersAB <- c(rbind(new_countersA, new_countersB))
  names(new_countersAB) <- c(rbind(names(new_countersA), names(new_countersB)))

  df <- data.frame(pair = names(new_countersAB), cnt = new_countersAB)
  df <- df %>% group_by(pair) %>% summarise(sum(cnt))
  counters[] <- 0
  counters[as.character(df$pair)] <- df$`sum(cnt)`
  counters
}

letters_cnt <- init_letters_cnt(template)
counters <- init_counters(template, mapping)

for (i in 1:40){
  letters_cnt <- gen_new_letters_cnt(counters, mapping, letters_cnt)
  counters <- gen_new_pairs_cnt(counters, mapping)
}
sorted_letters <- sort(letters_cnt)
print(sorted_letters[10] - sorted_letters[1],20)

