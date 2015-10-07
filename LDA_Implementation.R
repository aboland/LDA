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

# ----------------- Exisiting algorithm
library(topicmodels)
base_results <- LDA(my_dtm, k=3, method="Gibbs",control = list(iter=2000, burnin=100))
base_results@beta
base_results@gamma

base_results2 <- LDA(my_dtm, k=3, method="VEM")
exp(base_results2@beta)
base_results2@gamma

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

# ---------------- Gibbs
my_dtm <- DocumentTermMatrix(Corpus(VectorSource(my_corpus)))
my_z <- sample(1:3,size=length(my_dtm$i),replace=T)
my_lambda <- my_beta
for(i in 1:n_topics)
  my_lambda[i,] <- rdirichlet(1,my_beta[i,])

test_counts <- my_topic_counts(my_dtm, my_z)
topic_and_doc_nloglik(alpha = c(1,1,1), lambda = my_lambda, word_counts = test_counts[[1]], doc_counts=test_counts[[2]])

my_new_beta <- matrix(optim(fn = topic_and_doc_nloglik_optim, par = c(1, 1, 1, my_beta), n_topic = 3, word_counts = test_counts[[1]], doc_counts=test_counts[[2]])$par,nrow=3)

gibbs_test <- my_lda_gibbs(my_dtm, n_topic = 3, iterations = 40)
gibbs_test[[1]][40,,]
gibbs_test[[2]]
plot(gibbs_test[[1]][,1,1], type = "l")



topic_and_doc_nloglik_optim2(c(1,1), n_topic=3, word_counts = test_counts[[1]], doc_counts=test_counts[[2]], log = T)
my_lda_gibbs2(my_dtm, n_topic = 3, iterations = 10)
