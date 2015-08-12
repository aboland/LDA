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

my_vocab <- c("car", "engine", "exhaust", "wheel",
              "milk", "cream", "dairy", "yogurt",
              "coke", "water", "juice", "coffee", "drink", "bottle", "can")

n_topics <- 3

 # beta matrix needs a row for each topic, and a column for each word in vocab
my_beta <- matrix(c(10, 10, 10, 10, 01, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                    0.1, 0.1, 0.1, 0.1, 10, 10, 10, 10, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1,
                    0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 10, 10, 10, 10, 10, 10, 10), 
                  byrow = T, nrow = n_topics, ncol = length(my_vocab))

my_corpus <- GenerateLDA(alpha = 0.1, beta = my_beta, xi = 5, Vocab = my_vocab, Ndoc = 20)
my_corpus


# ------------------ Estimating alpha and beta

library(tm)
my_corpus
my_dtm <- as.array(DocumentTermMatrix(Corpus(VectorSource(my_corpus))))
# need to check my_dtm, as some words may not be in any of the generated documents....
dim(my_dtm) # should have 15 columns!!!

expect <- VariationalExpectationFull(alpha=rep(0.1, 3), beta = my_beta, dtm = my_dtm, epsilon = 0.1)

maximum <- VariationalMaximization(variational_para = expect, dtm = my_dtm)

combined <- EM_LDA(my_dtm, n_topics = 3, epsilon = 0.1)
maximum
