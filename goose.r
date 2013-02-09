#!/usr/bin/Rscript

# The Markov decision process
# @in:
#   board:
#   cycle: whether or not the board is a cycle
# @out:
#   a 2-col matrix containing the esperance vector and the dice choices' vector
markovDec <- function(board, cycle = FALSE) {
    dice <- vector("numeric", length = 15)
    esp <- vector("numeric", length = 15)
    for (i in seq_along(board)) {
        dice[i] <- 0
        esp[i] <- 42
    }
    return(cbind(esp, dice))
}

liste <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
v <- markovDec(liste)
Espe <- v[, 1]
De <- v[, 2]
print(Espe)
print(De)
