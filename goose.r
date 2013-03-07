#!/usr/bin/Rscript

SAFE <- 1
RISKY <- 2

NORMAL <- 0
RESTART <- 1
JAIL <- 2
BACK_2 <- 3

# The Markov decision process
# @in:
#   _board: a vector of square types:
#            type 0: normal (no trap)
#            type 1: back to square #1
#            type 2: await one turn (jail)
#            type 3: go two squares back
#           OR a graph
#   _cycle: whether or not the board is a full circle
# @out:
#   a 2-col matrix containing the esperance vector and the dice choices' vector
markovDec <- function(board, cycle = FALSE) {
    # building board shape
    if (typeof(board) == "list") {
        build_board_from_graph(board, cycle)
        start <- board[[3]]
        goal <- board[[4]]
        board <- board[[1]]
    }
    else {
        build_default_board(board, cycle)
        start <- 1
        goal <- 11
    }
    dice <- vector("numeric", length = length(board))
    esp <- vector("numeric", length = length(board))
    # initializing values for recursive computations
    for (i in seq_along(esp))
        esp[i] <- 1
    esp[goal] <- 0 # goal reward
    # computing actual values using value-iteration algorithm
    repeat { # converging
        prev_esp <- esp[1]
        for (i in c(start:(goal - 1), (goal + 1):length(board))) {
            esp_safe <- 1 + 0.5 * esp[i] + 0.5 * if (length(neighbors[[i]]) == 1)
                                                     esp[neighbors[[i]][[1]]]
                                                 else
                                                     (0.5 * esp[neighbors[[i]][[1]]] + 0.5 * esp[neighbors[[i]][[2]]])
            esp_risky <- 1 + exp_cost(esp, board, i, start) / 3 + if (length(neighbors[[i]]) == 1)
                                                                       exp_cost(esp, board, neighbors[[i]][[1]], start) / 3 + exp_cost(esp, board, neighbors[[neighbors[[i]][[1]]]][[1]], start) / 3
                                                                   else
                                                                       (0.5 * exp_cost(esp, board, neighbors[[i]][[1]], start) + 0.5 * exp_cost(esp, board, neighbors[[i]][[2]], start)) / 3 + (0.5 * exp_cost(esp, board, neighbors[[neighbors[[i]][[1]]]][[1]], start) + 0.5 * exp_cost(esp, board, neighbors[[neighbors[[i]][[2]]]][[1]], start)) / 3
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

# Builds the default board shape
# @in:
#   _v: the square types' vector
#   _cycle: whether or not it's a full circle
build_default_board <- function(v, cycle) {
    e <- list(c(1, 2), c(2, 3), c(3, 4), c(3, 12), c(4, 5), c(5, 6), c(5, 13), c(6, 7), c(7, 8), c(7, 14),
              c(8, 9), c(9, 10), c(9, 15), c(10, 11), c(12, 11), c(13, 11), c(14, 11), c(15, 11))
    build_board_from_graph(list(v, e, 1, 11), cycle)
}

# Builds a board from a graph
# @in:
#   _graph: a (square types' vector, edges, starting node, goal node) list
#   _cycle: whether or not it's a full circle
# @post:
#   _back_2: contains the previous states for each square
#   _neighbors: contains the following states for each square
build_board_from_graph <- function(graph, cycle) {
    # TODO: sorting routine so that it accepts edges in any order
    v <- graph[[1]]
    e <- graph[[2]]
    start <- graph[[3]]
    goal <- graph[[4]]
    neighbors <<- array(list(), length(v))
    back_2 <<- vector("numeric", length = length(v))
    for (i in seq_along(e))
        neighbors[[e[[i]][[1]]]] <<- c(neighbors[[e[[i]][[1]]]], e[[i]][[2]])
    neighbors[[goal]] <<- if (cycle) start else goal
    back_2_start <- length(v)
    for (i in seq_along(e)) {
        if (neighbors[[e[[i]][[2]]]][[1]] != e[[i]][[2]])
            back_2[[neighbors[[e[[i]][[2]]]][[1]]]] <<- e[[i]][[1]]
        if (length(neighbors[[e[[i]][[2]]]]) == 2)
            back_2[[neighbors[[e[[i]][[2]]]][[2]]]] <<- e[[i]][[1]]
        if (e[[i]][[2]] == goal && e[[i]][[1]] < back_2_start)
            back_2_start <- e[[i]][[1]]
    }
    back_2[[start]] <<- if (cycle) back_2_start else start
    back_2[[neighbors[[start]][[1]]]] <<- if (cycle) goal else start
    if (length(neighbors[[start]]) == 2)
        back_2[[neighbors[[start]][[2]]]] <<- if (cycle) goal else start
}

# Returns the expected cost given a square type.
# @in:
#   _esp: the expected cost of the square s
#   _board: the board containing the squares' type
#   _s: a square
#   _start: the starting square
# @out:
#   the expected cost according to the type of _s
exp_cost <- function(esp, board, s, start) {
    return (if (board[s] == NORMAL || board[s] == JAIL)
                esp[s]
            else if (board[s] == BACK_2)
                exp_cost(esp, board, back_2[[s]]) # trap leading to another trap
            else
                esp[start]) # starting square
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

# Tests routine.
# @in:
#   _boards: boards to simulate
#   _cycle: whether or not the boards are full circled
#   _games: number of games to simulate
# @post:
#   Prints out the mean number of throws for different strategies.
run_tests <- function(boards, cycle, games = 10000) {
    for (i in 1:length(boards[1, ])) {
        cat("Board #", i, "\n")
        v <- markovDec(boards[, i], cycle)
        exp <- v[, 1]
        mdp_dice <- v[, 2]
        cat("Expected costs: ", exp, "\nDecisions: ", mdp_dice, "\n")
        safe_dice <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
        risky_dice <- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
        mdp_mean <- 0
        safe_mean <- 0
        risky_mean <- 0
        random_mean <- 0
        for (j in 1:games) {
            safe_mean <- safe_mean + play_game(boards[, i], safe_dice)
            risky_mean <- risky_mean + play_game(boards[, i], risky_dice)
            random_mean <- random_mean + play_game(boards[, i], sample(1:2, 15, TRUE))
            mdp_mean <- mdp_mean + play_game(boards[, i], mdp_dice)
        }
        cat("Mean number of throws for safe dice: ", safe_mean / games, "\n",
            "Mean number of throws for risky dice: ", risky_mean / games, "\n",
            "Mean number of throws for random strategy: ", random_mean / games, "\n",
            "Mean number of throws for Markov decision process: ", mdp_mean / games, "\n")
    }
}

# Usage sample:
# liste <- c(0, 2, 0, 1, 0, 3, 0, 3, 0, 3, 0, 2, 2, 0, 0)
# v <- markovDec(liste)
# Espe <- v[, 1]
# De <- v[, 2]
# print(Espe)
# print(De)

boards <- cbind(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                c(0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0),
                c(0, 0, 2, 0, 0, 2, 2, 0, 2, 0, 0, 0, 0, 0, 2),
                c(0, 3, 0, 3, 3, 0, 0, 3, 0, 3, 0, 0, 3, 0, 0))

run_tests(boards, TRUE)
run_tests(boards, FALSE)
