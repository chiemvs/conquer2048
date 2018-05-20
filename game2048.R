# Test script for the game 2048

# added a 90:10 ratio for 2:4 in the new tile creation
# refactored to a socalled 'game' structure

# This function adds a tile to the board, on a random free spot. It puts the value 2 in 90 percent of the times and 4 in the remaining 10 percent.
add_tile <- function(game) {
 
  # is.na returns True when free spot, False when occupied with a tile, returns a matrix of similar dimensions.
  # The matrix with TRUE's is converted to indices by which. Each row of free_spots contains a row and column index combination
  free_spots <- which(x = is.na(game$board), arr.ind = TRUE, useNames = TRUE) 
  
  # Randomly choose a row-column combination, and the value
  random_spot <- free_spots[sample(x = 1:nrow(free_spots), size = 1),]
  new_row <- random_spot['row']
  new_col <- random_spot['col']
  new_value <- sample(x = c(rep(x = 2, 9), rep(x = 4, 1)), size = 1) # Gives a 90/10 ratio.
  
  # Place onto the board
  game$board[new_row, new_col] <- new_value

  return(game)
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
# Then it merges the tiles and adds the number to the score
# To unify the procedure it rotates the board a designated number of times so the direction is always upward.
move_tiles <- function(game, direction) {
 
  # list with possible clockwise rotations of the matrix, check whether the supplied direction is in it.
  rotations <- list(up = 0, left = 1, down = 2, right = 3) 
  stopifnot(direction %in% names(rotations))
  
  # Extract the score, such that it can be easily accessed by the 'assign' function in the loop
  score <- game$score
  
  # Read the right number of rotations from the list and rotate the board
  n_rot <- rotations[[direction]]
  rot_board <- clockwise_rotate(board = game$board, n_times = n_rot)
  
  # column wise storing of the values, removing NA's and pasting them as far to the upward end as possible.
  columns <- split(rot_board, rep(1:ncol(rot_board), each = nrow(rot_board)))
  columns_new <- unname(lapply(X = columns, FUN = function(col) {
    
    col_clean <- col[!is.na(col)] # removes all NA
    
    # Procedure for the pairwise doubling and scoring of the doubled values.
    run_lengths <- rle(col_clean) # Stores in order: each value in the column ($values) and in how many consecutive! tiles it occurs ($lengths)
    col_merged <- lapply(X = seq_along(run_lengths$values), FUN = function(index) {
      doubles <- rep(x = 2 * run_lengths$values[[index]], times = floor(run_lengths$lengths[[index]] / 2)) # paste the double of the value for each pair in the consecutive sequence, starting at the beginning.
      single <- rep(x = run_lengths$values[[index]], times = run_lengths$lengths[[index]] %% 2) # take account of the possible single value that remains. This one is not doubled. Modulo two (%% 2) becomes zero if the run length is a multiple of two and there are only pairs.
      assign(x = "score", value = score + sum(doubles), envir = parent.frame(4)) # Adds the doubled values to the score, which is defined four environments above this one.
      return(c(doubles,single))
    })
    col_merged <- do.call(what = c, args = col_merged) # Concatenate the values in the list of list output (vectors of various lengths) into a vector
      
    # Fill the column up with NA's till it is the right length again
    col_full <- c(col_merged, rep(NA, length.out = length(col) - length(col_merged))) 
  }))
  
  # Bind the columns and do the remainder of rotations for a total of 4, so the board is in original orientation again.
  game$board <- clockwise_rotate(board = do.call(cbind, columns_new), n_times = 4 - n_rot)
  game$score <- score
  return(game)
}

# Function for the initialization of the game. The variable play is used to start or terminate the game
initialize_2048 <- function() {
  n_col <- 4
  n_row <- 4
  board_t0 <- matrix(data = NA, nrow = n_row, ncol = n_col)
  score_t0 <- 0
  turns_t0 <- 0
  return(list(board = board_t0, score = score_t0, turns = turns_t0, play = TRUE))
}

# Function capture the progress after each turn (by adding 1 to the counter), and to terminate the game for the next turn when no spot is free to add a tile
update_game <- function(game, manual_terminate = FALSE) {
  game$turns <- game$turns + 1
  if (manual_terminate | (!any(is.na(game$board)))) {
    game$play <- FALSE # Stop when the board is full or when it is manually stopped.
  }
  return(game)
}
