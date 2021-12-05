source("setwd.R")

# Part  I -----------------------------------------------------------------

f <- scan(file = "data/input04.txt",what = character())
numbers <- strsplit(f[1],",")[[1]]

raw_boards <- f[2:length(f)]
board_nbr <- length(raw_boards)/25
columns <- matrix(0, nrow = board_nbr, ncol = 5)
lines <- matrix(0, nrow = board_nbr, ncol = 5)
max_checked <- c(0,0,0,0,-1)
names(max_checked) <- c("board","lc_ind","l_or_c","nbr_checked","stop_nbr")

get_board_ind <- function(el_ind)
{
  floor((el_ind-1)/25)+1
}

get_col_nbr <- function(el_ind)
{
  (el_ind-1)%%5+1
}

get_line_nbr <- function(el_ind)
{
  floor((el_ind-1)/5)%%5+1
}

get_board <- function(b_ind){
  first <- (b_ind - 1) * 25 + 1
  raw_boards[first:(first+24)]
}

is_board_won <- function(b_ind){
  any(lines[b_ind,] == 5 | columns[b_ind,] == 5)
}

new_max <- function(board, ind, lines.or.cols, nbr_checked){
  max_checked["board"] <<- board
  max_checked["lc_ind"] <<- ind
  max_checked["l_or_c"] <<- lines.or.cols
  max_checked["nbr_checked"] <<- nbr_checked
}

for (n in numbers) {
  bs <- get_board_ind(which(raw_boards == n))
  cols <- get_col_nbr(which(raw_boards == n))
  ls <- get_line_nbr(which(raw_boards == n))
  for(b in 1:length(bs)) {
    
      columns[bs[b],cols[b]] <- columns[bs[b],cols[b]] + 1
      if (columns[bs[b],cols[b]] > max_checked["nbr_checked"])
        new_max(bs[b],cols[b],1,columns[bs[b],cols[b]])
      if (max_checked["nbr_checked"] == 5)
      {
        max_checked["stop_nbr"] = as.integer(n)
        print(n)
        return(n)
      }
            
      lines[bs[b],ls[b]] <- lines[bs[b],ls[b]] + 1
      if (lines[bs[b],ls[b]] > max_checked["nbr_checked"])
        new_max(bs[b],ls[b],0,lines[bs[b],ls[b]])
      if (max_checked["nbr_checked"] == 5){
        max_checked["stop_nbr"] = as.integer(n)
        print(n)
        return(n)
      }
        
  }
}
sum(as.integer(setdiff(get_board(as.integer(max_checked[1])), numbers[1:which(numbers == n)]))) * as.integer(n)


# Part II -----------------------------------------------------------------

boards_won <- 0
columns <- matrix(0, nrow = board_nbr, ncol = 5)
lines <- matrix(0, nrow = board_nbr, ncol = 5)
max_checked <- c(0,0,0,0,-1)
names(max_checked) <- c("board","lc_ind","l_or_c","nbr_checked","stop_nbr")

for (n in numbers) {
  bs <- get_board_ind(which(raw_boards == n))
  cols <- get_col_nbr(which(raw_boards == n))
  ls <- get_line_nbr(which(raw_boards == n))
  for(b in 1:length(bs)) {
    if (!is_board_won(bs[b])) {
      columns[bs[b],cols[b]] <- columns[bs[b],cols[b]] + 1
      lines[bs[b],ls[b]] <- lines[bs[b],ls[b]] + 1
      if (is_board_won(bs[b]))
      {
        if (lines[bs[b],ls[b]] == 5)
          new_max(bs[b],ls[b],1,lines[bs[b],ls[b]])
        if (columns[bs[b],cols[b]] == 5)
          new_max(bs[b],cols[b],1,columns[bs[b],cols[b]])
        print(n)
        max_checked["stop_nbr"] = as.integer(n)
        boards_won <- boards_won + 1
      }
      if (boards_won == 100) return(n)
    }
  }
}

sum(as.integer(setdiff(get_board(as.integer(max_checked[1])), numbers[1:which(numbers == n)]))) * as.integer(n)
