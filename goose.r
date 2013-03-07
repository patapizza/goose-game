#!/usr/bin/Rscript

SAFE <- 1
RISKY <- 2

NORMAL <- 0
RESTART <- 1
JAIL <- 2
BACK_2 <- 3

# The Markov decision process
# @in:
#   _board: a list of square types:
#            type 0: normal (no trap)
#            type 1: back to square #1
#            type 2: await one turn (jail)
#            type 3: go two squares back
#   _cycle: whether or not the board is a full circle
# @out:
#   a 2-col matrix containing the esperance vector and the dice choices' vector
markovDec <- function(board, cycle = FALSE) {
    dice <- vector("numeric", length = 15)
    esp <- vector("numeric", length = 15)
    # building board shape
    build_board(cycle)
    # initializing values for recursive computations
    for (i in seq_along(esp))
        esp[i] <- 1
    esp[11] <- 0 # goal reward
    # computing actual values using value-iteration algorithm
    repeat { # converging
        prev_esp <- esp[1]
        for (i in c(10, 15, 9, 8, 14, 7, 6, 13, 5, 4, 12, 3, 2, 1)) {
            esp_safe <- 1 + 0.5 * esp[i] + 0.5 * if (length(neighbors[[i]]) == 1)
                                                     esp[neighbors[[i]][[1]]]
                                                 else
                                                     (0.5 * esp[neighbors[[i]][[1]]] + 0.5 * esp[neighbors[[i]][[2]]])
            esp_risky <- 1 + exp_cost(esp, board, i) / 3 + if (length(neighbors[[i]]) == 1)
                                                                       exp_cost(esp, board, neighbors[[i]][[1]]) / 3 + exp_cost(esp, board, neighbors[[neighbors[[i]][[1]]]][[1]]) / 3
                                                                   else
                                                                       (0.5 * exp_cost(esp, board, neighbors[[i]][[1]]) + 0.5 * exp_cost(esp, board, neighbors[[i]][[2]])) / 3 + (0.5 * exp_cost(esp, board, neighbors[[neighbors[[i]][[1]]]][[1]]) + 0.5 * exp_cost(esp, board, neighbors[[neighbors[[i]][[2]]]][[1]])) / 3
            # handling jails
            jails <- 0
            if (length(neighbors[[i]]) == 1) {
                if (board[neighbors[[i]][[1]]] == JAIL)
                    jails <- jails + 1
                if (board[neighbors[[neighbors[[i]][[1]]]][[1]]] == JAIL)
                    jails <- jails + 1
                esp_risky <- esp_risky + jails / 3
            }
            else {
                if (board[neighbors[[i]][[1]]] == JAIL)
                    jails <- jails + 1
                if (board[neighbors[[i]][[2]]] == JAIL)
                    jails <- jails + 1
                if (board[neighbors[[neighbors[[i]][[1]]]][[1]]] == JAIL)
                    jails <- jails + 1
                if (board[neighbors[[neighbors[[i]][[2]]]][[1]]] == JAIL)
                    jails <- jails + 1
                esp_risky <- esp_risky + jails / 6
            }
            if (esp_safe <= esp_risky) {
                dice[i] <- SAFE
                esp[i] <- esp_safe
            }
            else {
                dice[i] <- RISKY
                esp[i] <- esp_risky
            }
        }
        if (prev_esp == esp[1])
            break
    }
    return(cbind(esp, dice))
}

# Builds the board shape
# @in:
#   _cycle: whether or not it's a full circle
# @post:
#   _back_2: contains the previous states for each square
#   _neighbors: contains the following states for each square
build_board <- function(cycle) {
    # TODO: do that automatically; build shape for any board
    # previous states (needed for traps BACK_2)
    back_2 <<- array(list(), 15)
    back_2[1] <<- if (cycle) 10 else 1
    back_2[2] <<- 1
    back_2[3] <<- 1
    back_2[4] <<- 2
    back_2[5] <<- 3
    back_2[6] <<- 4
    back_2[7] <<- 5
    back_2[8] <<- 6
    back_2[9] <<- 7
    back_2[10] <<- 8
    back_2[11] <<- 9
    back_2[12] <<- 2
    back_2[13] <<- 4
    back_2[14] <<- 6
    back_2[15] <<- 8
    # following states
    neighbors <<- array(list(), 15)
    neighbors[1] <<- 2
    neighbors[2] <<- 3
    neighbors[[3]] <<- c(4, 12)
    neighbors[4] <<- 5
    neighbors[[5]] <<- c(6, 13)
    neighbors[6] <<- 7
    neighbors[[7]] <<- c(8, 14)
    neighbors[8] <<- 9
    neighbors[[9]] <<- c(10, 15)
    neighbors[10] <<- 11
    neighbors[11] <<- if (cycle) 1 else 11
    neighbors[12] <<- 11
    neighbors[13] <<- 11
    neighbors[14] <<- 11
    neighbors[15] <<- 11
}

# Returns the expected cost given a square type.
# @in:
#   _esp: the expected cost of the square s
#   _board: the board containing the squares' type
#   _s: a square
# @out:
#   the expected cost according to the type of _s
exp_cost <- function(esp, board, s) {
    return (if (board[s] == NORMAL || board[s] == JAIL)
                esp[s]
            else if (board[s] == BACK_2)
                exp_cost(esp, board, back_2[[s]]) # trap leading to another trap
            else
                esp[1]) # starting square
}

# Simulates a game.
# @in:
#   _board: the board
#   _dice: the dices to play for each square
# @out:
#   _throws: the number of dice throws
play_game <- function(board, dice) {
    square <- 1
    throws <- 0
    repeat {
        throws <- throws + 1
        choice <- dice[square]
        square <- if (choice == SAFE)
                      if (sample(0:1, 1))
                          square
                      else
                          if (length(neighbors[[square]]) == 1 || sample(0:1, 1))
                              neighbors[[square]][[1]]
                          else
                              neighbors[[square]][[2]]
                  else {
                      x <- sample(0:2, 1)
                      if (x == 0)
                          square
                      else if (x == 1)
                          if (length(neighbors[[square]]) == 1 || sample(0:1, 1))
                              neighbors[[square]][[1]]
                          else
                              neighbors[[square]][[2]]
                      else
                          if (length(neighbors[[square]]) == 1 || sample(0:1, 1))
                              neighbors[[neighbors[[square]][[1]]]][[1]]
                          else
                              neighbors[[neighbors[[square]][[2]]]][[1]]
                  }
        if (choice == RISKY)
            if (board[square] == RESTART)
                square <- 1
            else if (board[square] == BACK_2)
                square <- back_2[[square]]
            else if (board[square] == JAIL)
                throws <- throws + 1
        if (square == 11)
            break
    }
    return (throws)
}


liste <- c(0, 2, 0, 1, 0, 3, 0, 3, 0, 3, 0, 2, 2, 0, 0)
v <- markovDec(liste, TRUE)
Espe <- v[, 1]
De <- v[, 2]
print(Espe)
print(De)
print(play_game(liste, De))
safe <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
risky <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
print(play_game(liste, safe))
print(play_game(liste, risky))
