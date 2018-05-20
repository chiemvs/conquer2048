# This script is an interactive way to play the game, which is programmed in game2048.R
source("./game2048.R")

# Function to play the game interactively
# The structure to go through the steps of the game is a list with: 
# - the board (4x4 matrix)
# - the global score at that moment.
# Then at each turn a direction of movement needs to be given, and a random tile is added to the field.
start_2048 <- function() {
  
  print("Welcome to 2048, type 'end' to stop the game")
  output <- list(board = board_t0, score = 0) # Initialization
  input <- "up" # just an initial direction of movement so it does not crash.
  
  while(input != "end") {
    
    output <- move_tiles(board = output$board, direction = input, score = output$score )
    print(paste("your score is", output$score, sep = " "))
    print(output$board)
    output$board <- add_tile(board = output$board)
    print(output$board)
    input <- readline(prompt = "enter direction: ")
  }
  stop()
}

start_2048()