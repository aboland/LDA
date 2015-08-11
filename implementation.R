# Latent Dirichlet Allocation implementation
#
# Date: 6/7/15
# Author: Aidan Boland
#
# Simple examples of LDA as described in original paper (Blei et al., 2002)
# https://www.cs.princeton.edu/~blei/papers/BleiNgJordan2003.pdf
#
# Uses the functions from lda_functions.R
#

source('lda_functions.R')

# ---------------- Generating a corpus

# my_vocab <- c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog")
my_vocab <- c("car", "engine", "exhaust", "wheel",
              "milk", "cream", "dairy", "yogurt",
              "coke", "water", "juice", "coffee", "drink", "bottle", "can")

n_topics <- 3

my_beta <- matrix(c(10, 10, 10, 10, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                    0.1, 0.1, 0.1, 0.1, 10, 10, 10, 10, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                    0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 10, 10, 10, 10, 10, 10, 10), 
                  byrow = T, nrow = n_topics, ncol = length(my_vocab))

my_corpus <- generate_lda(alpha = 0.1, beta = my_beta, xi = 4, Vocab = my_vocab, Ndoc = 6)
my_corpus