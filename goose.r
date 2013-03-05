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
    # building board shape
    # TODO: do that automatically; build shape for any board
    # previous states (needed for traps BACK_2)
    back_2 <- array(list(), 15)
    back_2[1] <- 1 # no cycle for now
    back_2[2] <- 1
    back_2[3] <- 1
    back_2[4] <- 2
    back_2[5] <- 3
    back_2[6] <- 4
    back_2[7] <- 5
    back_2[8] <- 6
    back_2[9] <- 7
    back_2[10] <- 8
    back_2[11] <- 9
    back_2[12] <- 2
    back_2[13] <- 4
    back_2[14] <- 6
    back_2[15] <- 8
    # following states
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
    for (i in seq_along(esp))
        esp[i] <- 1
    esp[11] <- 0 # goal reward
    # computing actual values using value-iteration algorithm
    repeat { # converging
        prev_esp = esp[1]
        for (i in c(10, 15, 9, 8, 14, 7, 6, 13, 5, 4, 12, 3, 2, 1)) {
            esp_safe <- 1 + 0.5 * esp[i] + 0.5 * if (length(neighbors[[i]]) == 1)
                                                     esp[neighbors[[i]][[1]]]
                                                 else
                                                     (0.5 * esp[neighbors[[i]][[1]]] + 0.5 * esp[neighbors[[i]][[2]]])
            esp_risky <- 1 + exp_cost(esp, back_2, board, i) / 3 + if (length(neighbors[[i]]) == 1)
                                                                       exp_cost(esp, back_2, board, neighbors[[i]][[1]]) / 3 + exp_cost(esp, back_2, board, neighbors[[neighbors[[i]][[1]]]][[1]]) / 3
                                                                   else
                                                                       (0.5 * exp_cost(esp, back_2, board, neighbors[[i]][[1]]) + 0.5 * exp_cost(esp, back_2, board, neighbors[[i]][[2]])) / 3 + (0.5 * exp_cost(esp, back_2, board, neighbors[[neighbors[[i]][[1]]]][[1]]) + 0.5 * exp_cost(esp, back_2, board, neighbors[[neighbors[[i]][[2]]]][[1]])) / 3
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

exp_cost <- function(esp, back_2, board, s) {
    return (if (board[s] == NORMAL || board[s] == JAIL)
                esp[s]
            else if (board[s] == BACK_2)
                esp[back_2[[s]]]
            else
                esp[1]) # starting square
}


liste <- c(0, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0)
v <- markovDec(liste)
Espe <- v[, 1]
De <- v[, 2]
print(Espe)
print(De)
