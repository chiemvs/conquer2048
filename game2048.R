# Test script for the game 2048

# To-do: add 2:4 ratio for the new tiles

n_col <- 4
n_row <- 4

board_t0 <- matrix(data = NA, nrow = n_row, ncol = n_col)

# This function adds a tile to the board, on a random free spot. It puts either the value 2 or the value 4
add_tile <- function(board) {
 
  # is.na returns True when free spot, False when occupied with a tile, returns a matrix of similar dimensions.
  # If no spot is free the game will end
  stopifnot(any(is.na(board)))
  
  # The matrix with TRUE's is converted to indices by which. Each row of free_spots contains a row and column index combination
  free_spots <- which(x = is.na(board), arr.ind = TRUE, useNames = TRUE) 
  
  # Randomly choose a row-column combination, and the value
  random_spot <- free_spots[sample(x = 1:nrow(free_spots), size = 1),]
  new_row <- random_spot['row']
  new_col <- random_spot['col']
  new_value <- sample(x = c(2,4), size = 1) # Currently a 50/50 ratio
  
  # Place onto the board
  board[new_row, new_col] <- new_value

  return(board)
}

# This is a function to rotate the board (a 2d matrix) a designated number of times, clockwise.
clockwise_rotate <- function(board, n_times) {
  while(n_times > 0) {
    board <- t(apply(X = board, MARGIN = 2, FUN = rev))
    n_times <- n_times - 1
  }
  return(board)
}

# This function moves all the tiles as far as possible in the direction of movement. 
# To unify the procedure it rotates the board a designated number of times so the direction is always upward.
move_tiles <- function(board, direction) {
 
  # list with possible clockwise rotations of the matrix, check whether the supplied direction is in it.
  rotations <- list(up = 0, left = 1, down = 2, right = 3) 
  stopifnot(direction %in% names(rotations))
  
  # Extract the right number of rotations and rotate the board
  n_rot <- rotations[[direction]]
  rot_board <- clockwise_rotate(board = board, n_times = n_rot)
  
  # column wise storing of the values, removing NA's and pasting them as far to the upward end as possible.
  columns <- split(rot_board, rep(1:ncol(rot_board), each = nrow(rot_board)))
  columns_new <- unname(lapply(X = columns, FUN = function(col) {
    
    col_clean <- col[!is.na(col)] # removes all NA
    
    # Procedure for the pairwise doubling.
    run_lengths <- rle(col_clean) # Stores in order: each value in the column ($values) and in how many consecutive! tiles it occurs ($lengths)
    col_merged <- lapply(X = seq_along(run_lengths$values), FUN = function(index) {
      doubles <- rep(x = 2 * run_lengths$values[[index]], times = floor(run_lengths$lengths[[index]] / 2)) # paste the double of the value for each pair in the consecutive sequence, starting at the beginning.
      single <- rep(x = run_lengths$values[[index]], times = run_lengths$lengths[[index]] %% 2) # take account of the possible single value that remains. This one is not doubled. Modulo two (%% 2) becomes zero if the run length is a multiple of two and there are only pairs.
      return(c(doubles,single))
    })
    col_merged <- do.call(what = c, col_merged) # Concatenate the list output (vectors of various lengths) into a vector
    
    # Fill the column up with NA's till it is the right length again
    col_full <- c(col_merged, rep(NA, length.out = length(col) - length(col_merged))) 
  }))
  
  # Bind the columns and do the remainder of rotations for a total of 4, so the board is in original orientation again.
  board <- clockwise_rotate(board = do.call(cbind, columns_new), n_times = 4 - n_rot)
  return(board)
}

# Function to play the game interactively
start_2048 <- function() {
  
  print("Welcome to 2048, type 'end' to stop the game")
  board <- board_t0
  input <- "up" # just an initial value so it does not crash.
  
  while(input != "end") {
    
    board <- move_tiles(board = board, direction = input)
    print(board)
    board <- add_tile(board = board)
    print(board)
    input <- readline(prompt = "enter direction: ")
  }
  stop()
}

start_2048()
