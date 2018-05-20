# This script is an interactive way to play the game, which is programmed in game2048.R
source("./game2048.R")

# Function to play the game interactively
# The structure to go through the steps of the game is a list with: 
# - the board (4x4 matrix)
# - the global score at that moment.
# Then at each turn a direction of movement needs to be given, and a random tile is added to the field.
play_2048 <- function() {
  
  print("Welcome to 2048, type 'end' to stop the game")
  game <- initialize_2048() # Initialization
  
  while (game$play) {
    
    game <- add_tile(game = game)
    print(game$board)
    input <- readline(prompt = "enter direction: ")
    if (input != 'end') {
      game <- move_tiles(game = game, direction = input)
      print(game$board)
      game <- update_game(game)
      print(paste("turns:", game$turns ,"score:", game$score, sep = " "))
    } else {
      game <- update_game(game, manual_terminate = TRUE)
    }
  }
  print("The game has finished")
  print(paste("turns:", game$turns ,"score:", game$score, sep = " "))
}

play_2048()