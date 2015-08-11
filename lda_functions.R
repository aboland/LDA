# Latent Dirichlet Allocation
#
# Date: 6/7/15
# Author: Aidan Boland
#
# Simple examples of LDA as described in original paper (Blei et al., 2002)
# https://www.cs.princeton.edu/~blei/papers/BleiNgJordan2003.pdf
#

library(MCMCpack)  # alternative library(gtools)

my_vocab <- c("the", "quick", "brown", "fox", "jumps", "over", "lazy", "dog")

generate_lda <- function(alpha, beta, xi, Vocab, ndoc){
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
  
  if(dim(beta)[2]!=length(Vocab))
    stop("Incorrect dimension for Beta!!")
  
  w <- array(0,c(Ndoc,N))  # blank array to store words, w_i,j will be a numeric value corresponding to a word in Vocab
  
  for(i in 1:Ndoc){
    N <- rpois(xi)
    theta <- rdirichlet(n=1, alpha=alpha)
    for(j in 1:N){
      z <- rmultinom(1, 1, prob=theta)
      phi <- rdirichlet(n=1, alpha=beta[z,])
      w[i,j] <- rmultinom(1, 1, prob=phi)
    }
  }
  
  
}