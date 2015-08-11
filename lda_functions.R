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
    theta <- rdirichlet(n = 1, alpha = alpha)
    
    z <- rmultinom(1, 1, prob = theta)
    phi <- rdirichlet(n = 1, alpha = beta[which(z == 1),])
    w[[i]] <- Vocab[which(rmultinom(1, 1, prob = phi) == 1)]  # Initialise the ith item in the list w
    
    for(j in 1:N){
      z <- rmultinom(1, 1, prob = theta)
      phi <- rdirichlet(n=1, alpha = beta[which(z ==1),])
      w[[i]] <- c(w[[i]], Vocab[which(rmultinom(1, 1, prob = phi) == 1)])
    }
  }
  w
}


# This function is wrong!!!!
# Should be seperate gamma and psi for each document!!!
variational_Expectation <- function(alpha, beta, epsilon = 0.1){
  # function to cary out variational inference as described in Blei (pg's 1001-1005)
  # Expectation step of EM alg
  # arguments:
  #   alpha
  #   beta
  n_topics <- nrow(beta)
  n_words <- ncol(beta)
  alpha <- rep(alpha, n_topics)  # Using fixed alpha...can be easily changed
  psi <- psi_new <- array(1/n_topics, c(n_topics, n_words))
  gamma <- gamma_new <- alpha + (n_words/n_topics)
  conv.test <- epsilon + 1

  while(conv.test > epsilon){
    for(n in 1:n_words)
      for(i in 1:n_topics)
        psi_new[i,n] <- beta[i,n] * exp(digamma(gamma[i]))  # update psi
      psi_new <- psi_new / colSums(psi_new)  # normalise psi
      gamma_new <- alpha + rowSums(psi_new)  # update gamma

      conv.test <- max(c(abs(gamma - gamma_new),abs(psi - psi_new)))  # check for convergence
      gamma <- gamma_new
      psi <- psi_new
  }
  list(gamma, psi)
}


variational_Expectation_full <- function(alpha, beta, dtm, epsilon = 0.1){
  # function to cary out variational inference as described in Blei (pg's 1001-1005)
  # Expectation step of EM alg
  #
  # arguments:
  #   alpha
  #   beta
  #   dtm, document term matrix
  
  n_topics <- nrow(beta)
  n_docs <- nrow(dtm)
  output <- list()
  
  pb <- txtProgressBar(min = 0, max = n_docs, style = 3)
  for(d in 1:n_docs){
    setTxtProgressBar(pb, d)
    n_words <- sum(dtm[d,]!=0)  # number of unique words
    alpha_temp <- rep(alpha, n_topics)  # Using fixed alpha...can be easily changed
    beta_temp <- array(beta[,which(dtm[d,]!=0)] ,c(n_topics,n_words))
    psi <- psi_new <- array(1/n_topics, c(n_topics, n_words))
    gamma <- gamma_new <- alpha_temp + (n_words/n_topics)
    conv.test <- epsilon + 1

    while(conv.test > epsilon){
      for(n in 1:n_words)
        for(i in 1:n_topics)
          psi_new[i,n] <- beta_temp[i,n] * exp(digamma(gamma[i]))  # update psi
        psi_new <- psi_new / colSums(psi_new)  # normalise psi
        gamma_new <- alpha_temp + rowSums(psi_new)  # update gamma
      
        conv.test <- max(c(abs(gamma - gamma_new), abs(psi - psi_new)))  # check for convergence
        gamma <- gamma_new
        psi <- psi_new
    }
    output[[d]]<-list(gamma = gamma, psi = psi)
  }
  close(pb)
  output
}


maximization <- function(variational_para, dtm){
  # Function to carry out maximization step of Blei
  # arguments:
  #   gamma and psi, the two variational parameters
  #   dtm, document term matrix, where d,nth entry is 1 if word n is in document d (not sure about multiple words in the same doc)
  
  #if(length(gamma) != nrow(psi) || ncol(dtm) != ncol(psi))  # check that the dimensions match
  #  stop("Inncorrect dimensions of parameters!")
  n_topics <- nrow(variational_para[[1]]$psi)
  
  beta <- array(0, c(n_topics, ncol(dtm)))
  for(d in 1:length(variational_para)){
    #n_words <- ncol(variational_para[[d]]$psi)
    for(i in 1:n_topics)
      beta[i,which(dtm[d,]!=0)] <- beta[i,which(dtm[d,]!=0)] + variational_para[[d]]$psi[i,]  # * dtm[d,which(dtm[d,]!=0)])
  }
  beta
}
