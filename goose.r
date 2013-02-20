#!/usr/bin/Rscript

SAFE <- 1
RISKY <- 2

NORMAL <- 0
RESTART <- 1
JAIL <- 2
BACK_2 <- 3

# The Markov decision process
# @in:
#   board: a list of square types:
#            type 0: normal (no trap)
#            type 1: back to square #1
#            type 2: await one turn (jail)
#            type 3: go two squares back
#   cycle: whether or not the board is a cycle
# @out:
#   a 2-col matrix containing the esperance vector and the dice choices' vector
markovDec <- function(board, cycle = FALSE) {
    dice <- vector("numeric", length = 15)
    esp <- vector("numeric", length = 15)
    v <- vector("numeric", length = 15)
    # building board shape
    neighbors <- array(list(), 15)
    neighbors[1] <- 2
    neighbors[2] <- 3
    neighbors[[3]] <- c(4, 12)
    neighbors[4] <- 5
    neighbors[[5]] <- c(6, 13)
    neighbors[6] <- 7
    neighbors[[7]] <- c(8, 14)
    neighbors[8] <- 9
    neighbors[[9]] <- c(10, 15)
    neighbors[10] <- 11
    neighbors[11] <- 11 # no cycle for now
    neighbors[12] <- 11
    neighbors[13] <- 11
    neighbors[14] <- 11
    neighbors[15] <- 11
    # initializing values for recursive computations
    for (i in seq_along(v))
        v[i] <- 1
    v[11] <- -1 # goal reward
    # computing actual values using value-iteration algorithm
    for (k in 1:15) # getting to convergence
        for (i in c(10, 15, 9, 8, 14, 7, 6, 13, 5, 4, 12, 3, 2, 1))
            v[i] <- if (length(neighbors[[i]]) == 1)
                        min(cost(SAFE, board[i], i) + 0.5 * v[i] + 0.5 * v[neighbors[[i]][[1]]],
                            if (length(neighbors[[neighbors[[i]][[1]]]]) == 1)
                                cost(RISKY, board[i], i) + 1/3 * v[i] + 1/3 * v[neighbors[[i]][[1]]] + 1/3 * v[neighbors[[neighbors[[i]][[1]]]][[1]]]
                            else
                                cost(RISKY, board[i], i) + 1/3 * v[i] + 1/3 * v[neighbors[[i]][[1]]] + 1/3 * (0.5 * v[neighbors[[neighbors[[i]][[1]]]][[1]]] + 0.5 * v[neighbors[[neighbors[[i]][[1]]]][[2]]])
                           )
                    else
                        min(cost(SAFE, board[i], i) + 0.5 * v[i] + 0.5 * (0.5 * v[neighbors[[i]][[1]]] + 0.5 * v[neighbors[[i]][[2]]]),
                            cost(RISKY, board[i], i) + 1/3 * v[i] + 1/3 * (0.5 * v[neighbors[[i]][[1]]] + 0.5 * v[neighbors[[i]][[2]]]) + 1/3 * (0.5 * v[neighbors[[neighbors[[i]][[1]]]][[1]]] + 0.5 * v[neighbors[[neighbors[[i]][[2]]]][[1]]]))
    print(v)
    # @TODO: argmin to pick the best action for each square
    for (i in seq_along(board)) {
        dice[i] <- 0
        esp[i] <- 42
    }
    return(cbind(esp, dice))
}

cost <- function(action, s_type, s_pos) {
    return (if (action == SAFE || s_type == NORMAL)
                1
            else # RESTART for now
                s_pos)
}

liste <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
v <- markovDec(liste)
Espe <- v[, 1]
De <- v[, 2]
print(Espe)
print(De)
