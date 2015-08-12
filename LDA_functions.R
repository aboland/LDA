# Latent Dirichlet Allocation
#
# Date: 6/7/15
# Author: Aidan Boland
#
# Simple examples of LDA as described in original paper (Blei et al., 2002)
# https://www.cs.princeton.edu/~blei/papers/BleiNgJordan2003.pdf
#

library(MCMCpack)  # for dirichlet dist, alternative is library(gtools)

GenerateLDA <- function(alpha, beta, xi, Vocab, Ndoc){
  # function to generate a single document
  # arguments:
  #   alpha, k x 1 concentration paramter for topics, (take input as single value for now)
  #   beta, k x V each row represents the concentration parameter for words in topic k
  #   xi, parameter to simulate length of documents
  #   Vocab, vector of size V containing all possible words
  #   ndoc, number of documents to simulate
  #
  # todo:
  #   change output to document term matrix rather than list....
  
  k <- nrow(beta)
  alpha <- rep(alpha, k)
  
  if(dim(beta)[2] != length(Vocab))
    stop("Incorrect dimension for Beta!!")
  
  docs <- list()  # blank list to store words, documents diff size so using an array is not suitable. Could use DTM alternatively...

  for(i in 1:Ndoc){
    N <- rpois(1, lambda = xi)  # Choose the length of the ith doc
    theta <- rdirichlet(n = 1, alpha = alpha)
    
    z <- rmultinom(1, 1, prob = theta)  # Choose a topic for 1st word in doc i
    phi <- rdirichlet(n = 1, alpha = beta[which(z == 1),])  # generate probs for the word
    docs[[i]] <- Vocab[which(rmultinom(1, 1, prob = phi) == 1)]  # Initialise the ith item in the list docs
    
    for(j in 1:N){
      z <- rmultinom(1, 1, prob = theta)  # Choose a topic for jth word in doc i  
      phi <- rdirichlet(n = 1, alpha = beta[which(z == 1),])
      docs[[i]] <- c(docs[[i]], Vocab[which(rmultinom(1, 1, prob = phi) == 1)])
    }
  }
  docs
}


# # This function is wrong!!!!
# # Should be seperate gamma and phi for each document!!!
# VariationalExpectation <- function(alpha, beta, epsilon = 0.1){
#   # function to cary out variational inference as described in Blei (pg's 1001-1005)
#   # Expectation step of EM alg
#   # arguments:
#   #   alpha
#   #   beta
#   n_topics <- nrow(beta)
#   n_words <- ncol(beta)
#   alpha <- rep(alpha, n_topics)  # Using fixed alpha...can be easily changed
#   phi <- phi_new <- array(1 / n_topics, c(n_topics, n_words))
#   gamma <- gamma_new <- alpha + (n_words / n_topics)
#   conv_test <- epsilon + 1
# 
#   while(conv_test > epsilon){
#     for(n in 1:n_words)
#       for(i in 1:n_topics)
#         phi_new[i,n] <- beta[i,n] * exp(digamma(gamma[i]))  # update phi
#       phi_new <- phi_new / colSums(phi_new)  # normalise phi
#       gamma_new <- alpha + rowSums(phi_new)  # update gamma
# 
#       conv_test <- max(c(abs(gamma - gamma_new),abs(phi - phi_new)))  # check for convergence
#       gamma <- gamma_new
#       phi <- phi_new
#   }
#   list(gamma, phi)
# }


VariationalExpectationFull <- function(alpha, beta, dtm, epsilon = 0.1){
  # function to cary out variational inference as described in Blei (pg's 1001-1005)
  # Expectation step of EM alg
  #
  # arguments:
  #   alpha, vector of size topics
  #   beta, array size topics X words
  #   dtm, document term matrix
  #   epsilon, value for convergence test, smaller = stricter
  #
  # output:
  #   list where the ith element contains values of gamma and phi for the ith document
  
  n_topics <- nrow(beta)
  n_docs <- nrow(dtm)
  output <- list()
  
  #pb <- txtProgressBar(min = 0, max = n_docs, style = 3)  # progress bar
  for(d in 1:n_docs){
    #setTxtProgressBar(pb, d)
    n_words <- sum(dtm[d,] != 0)  # number of unique words
    alpha_temp <- alpha
    beta_temp <- array(beta[,which(dtm[d,] != 0)] ,c(n_topics,n_words))  # cut beta to only contain the columns for the words in doc d
    phi <- phi_new <- array(1 / n_topics, c(n_topics, n_words))  # initalise phi
    gamma <- gamma_new <- alpha_temp + (n_words / n_topics)  # initialise gamma
    conv_test <- epsilon + 1

    while(conv_test > epsilon){
      for(n in 1:n_words)
        for(i in 1:n_topics)
          phi_new[i,n] <- beta_temp[i,n] * exp(digamma(gamma))  # update phi
        phi_new <- phi_new / colSums(phi_new)  # normalise phi
        gamma_new <- alpha_temp + rowSums(phi_new)[1]  # update gamma, not sure how to handle fixed alpha here.....
        #cat("\n",rowSums(phi_new),"\n")
      
        conv_test <- max(c(abs(gamma - gamma_new), abs(phi - phi_new)))  # check for convergence
        gamma <- gamma_new
        phi <- phi_new
    }
    output[[d]] <- list(gamma = gamma, phi = phi)
  }
  #close(pb)
  output
}


VariationalMaximization <- function(variational_para, dtm, epsilon = 0.1){
  # Function to carry out maximization step of Blei
  # arguments:
  #   gamma and phi, the two variational parameters
  #   dtm, document term matrix, where d,nth entry is 1 if word n is in document d (not sure about multiple words in the same doc)

  n_topics <- nrow(variational_para[[1]]$phi)
  beta <- array(0, c(n_topics, ncol(dtm)))
  for(d in 1:nrow(dtm)){
    # debugging: I'm happy with this update of beta being correct!
    for(i in 1:n_topics)
<<<<<<< HEAD
      beta[i,which(dtm[d,]!=0)] <- beta[i,which(dtm[d,]!=0)] + exp(variational_para[[d]]$phi[i,])  # * dtm[d,which(dtm[d,]!=0)])
  }
  
  alpha <- rep(0.1, n_topics)  # initialise alpha
  log_alpha <- log(alpha)
  conv_test <- 1
  gradient_constant <- colSums(matrix(unlist(lapply(variational_para, function(x) digamma(x$gamma) - digamma(sum(x$gamma)))), ncol = n_topics, byrow = T))
  while(conv_test > epsilon){
    # For the following Newton-Rhapson algorithm see pages 1018-1022 in Blei
    # g is the gradient, z and h relate to the Hessian, all on page 1022
    # H_inv is from a formula on pages 1018/1019
    
    g <- length(variational_para) * (digamma(sum(alpha)) - digamma(alpha)) + gradient_constant
      
    
    z <- trigamma(sum(alpha))
    h <- length(variational_para) * trigamma(alpha)
    c <- sum(g / h) / (1 / z + sum(1 / h))
    H_inv <- (g - c) / h
    conv_test <- max(abs(H_inv))
=======
      beta[i,which(dtm[d,] != 0)] <- beta[i, which(dtm[d,] != 0)] + exp(variational_para[[d]]$phi[i,])  # * dtm[d,which(dtm[d,]!=0)])
  }
  
  alpha <- 0.1  # initialise alpha
  log_alpha <- log(alpha)
  conv_test <- 1
  gradient_constant <- sum(unlist(lapply(variational_para, function(x) digamma(x$gamma) - digamma(n_topics * x$gamma))))
  
  while(conv_test > epsilon){
    # the following Newton-Rhapson was taken from source code relating to Blei paper
    # g is the gradient
    # H is hte Hessian
>>>>>>> alpha
    
    g <- (nrow(dtm) * (digamma(n_topics * alpha) - digamma(alpha))) + gradient_constant
    H <- nrow(dtm) * (n_topics^2 * digamma(n_topics * alpha) - n_topics * digamma(alpha))
    
<<<<<<< HEAD
    log_alpha <- log_alpha - H_inv
=======
    log_alpha <- log_alpha - g/(H * alpha + g);
    conv_test <- abs(alpha - exp(log_alpha))
>>>>>>> alpha
    alpha <- exp(log_alpha)
  }
  list(alpha = alpha, beta = beta)
}

EM_LDA <- function(dtm, n_topics = 2, epsilon = 0.1){
  # Function to combine the expectationa and maximisation steps for EM algorithm
  # arguments:
  #   dtm, document term matrix
  #   n_topics, number of topics
  #   epsilon, parameter for convergence, smaller = stricter
  
  n_terms <- ncol(dtm)
  maximum <- list(alpha = 0.1,
                  beta = array(0.1,c(n_topics, n_terms)))
  conv_test <- epsilon + 1
  
  while(conv_test > epsilon){
    # E-M steps
    expect <- VariationalExpectationFull(alpha = maximum$alpha, beta = maximum$beta, dtm = dtm, epsilon = epsilon)
    maximum_new <- VariationalMaximization(variational_para = expect, dtm = dtm, epsilon = epsilon)
    
    conv_test <- max(c(abs(maximum$alpha - maximum_new$alpha), abs(maximum$beta - maximum_new$beta)))
    maximum <- maximum_new
    cat("alpha:",maximum$alpha,"\tmax beta:",max(maximum$beta),"\n")  # Debugging
  }
  list(alpha = maximum$alpha, beta = maximum$beta)
}


