# Latent Dirichlet Allocation
#
# Date: 6/7/15
# Author: Aidan Boland
#
# Simple examples of LDA as described in original paper (Blei et al., 2002)
# https://www.cs.princeton.edu/~blei/papers/BleiNgJordan2003.pdf
#

library(MCMCpack)  # for dirichlet dist, alternative library(gtools)

generate_lda <- function(alpha, beta, xi, Vocab, Ndoc){
  # function to generate a single document
  # arguments:
  #   alpha, k x 1 concentration paramter for topics, (take input as single value for now)
  #   beta, k x V each row represents the concentration parameter for words in topic k
  #   xi, parameter to simulate length of documents
  #   Vocab, vector of size V containing all possible words
  #   ndoc, number of documents to simulate
  #
  
  k <- nrow(beta)
  alpha <- rep(alpha, k)
  
  if(dim(beta)[2] != length(Vocab))
    stop("Incorrect dimension for Beta!!")
  
  w <- list()  # blank list to store words, documents diff size so using an array is not suitable

  for(i in 1:Ndoc){
    N <- rpois(1, lambda = xi)  # Choose the length of the ith doc
    theta <- rdirichlet(n=1, alpha = alpha)
    z <- rmultinom(1, 1, prob = theta)
    phi <- rdirichlet(n=1, alpha = beta[z,])
    w[[i]] <- Vocab[which(rmultinom(1, 1, prob = phi) == 1)]  # Initial the ith item in the list w
    
    for(j in 1:N){
      z <- rmultinom(1, 1, prob = theta)
      phi <- rdirichlet(n=1, alpha = beta[z,])
      w[[i]] <- c(w[[i]], Vocab[which(rmultinom(1, 1, prob = phi) == 1)])
    }
  }
  w
}


 # ---------------- Generating a corpus

# my_vocab <- c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog")
my_vocab <- c("car", "engine", "exhaust", "wheel",
              "milk", "cream", "dairy", "yogurt",
              "coke", "water", "juice", "coffee", "drink", "bottle", "can")

n_topics <- 3

my_beta <- matrix(c(1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                    0.1, 0.1, 0.1, 0.1, 1, 1, 1, 1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                    0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 1, 1, 1, 1, 1, 1, 1), byrow = T,
                  nrow = n_topics, ncol = length(my_vocab))

my_corpus <- generate_lda(alpha = 1, beta = my_beta, xi = 3, Vocab = my_vocab, Ndoc = 6)
