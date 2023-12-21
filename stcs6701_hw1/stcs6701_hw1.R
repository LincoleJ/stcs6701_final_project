library(readr)
library(tm)
library(dplyr)
library(tidyverse)
library(parallel)
library(foreach)
library(ggplot2)

################################################################################
#                             *Data Processing*                                #
################################################################################
df_original = read_csv("./HW1_data/IMDB Dataset.csv") 
set.seed(2023)
df = df_original[sample(nrow(df), size = 10000, replace = FALSE), ]
df_excluded = setdiff(df_original, df)
df_test = df_excluded[sample(nrow(df_excluded), size = 10000, replace = FALSE), ]

# tf-idf
df$review = tolower(df$review) # make lower-case
df$review = removePunctuation(df$review) # remove punctuations
df$review = removeNumbers(df$review) # remove numbers
corpus = VCorpus(VectorSource(df$review)) # create corpus
corpus = tm_map(corpus, removeWords, stopwords("english")) # remove stopwords
# corpus = tm_map(corpus, stemDocument) # perform stemming
dtm = DocumentTermMatrix(corpus)
dtm_filtered = removeSparseTerms(dtm, sparse = 1 - 10 / 10000)
# check removed terms
removed_terms = setdiff(colnames(dtm), colnames(dtm_filtered))
sample(removed_terms, size = 20) # looks fine; most seem to be typos
# create tfidf
tfidf = weightTfIdf(dtm_filtered)
tfidf_values = as.data.frame(as.matrix(tfidf)) 
# change resp. var. to 0/1
df$sentiment[df$sentiment == "positive"] = 1
df$sentiment[df$sentiment == "negative"] = 0 
# rename response variable; merge as desired data frame
df = rename(df, y_obs = sentiment)
df = df[, -c(1)]
df <- cbind(df, tfidf_values)
# save dataset to local directory as RDS file
saveRDS(df, "./HW1_data/imdb_dat.rds") # save to local directory

# for test dataset
# tf-idf
df_test$review = tolower(df_test$review) # make lower-case
df_test$review = removePunctuation(df_test$review) # remove punctuations
df_test$review = removeNumbers(df_test$review) # remove numbers
corpus_test = VCorpus(VectorSource(df_test$review)) # create corpus
corpus_test = tm_map(corpus_test, removeWords, stopwords("english")) # remove stopwords
# corpus = tm_map(corpus, stemDocument) # perform stemming
dtm_test = DocumentTermMatrix(corpus_test)
dtm_filtered_test = removeSparseTerms(dtm_test, sparse = 1 - 10 / 10000)
# check removed terms
removed_terms_test = setdiff(colnames(dtm_test), colnames(dtm_filtered_test))
sample(removed_terms, size = 20) # looks fine; most seem to be typos
# create tfidf
tfidf_test = weightTfIdf(dtm_filtered_test)
tfidf_values_test = as.data.frame(as.matrix(tfidf_test)) 
# change resp. var. to 0/1
df_test$sentiment[df_test$sentiment == "positive"] = 1
df_test$sentiment[df_test$sentiment == "negative"] = 0 
# rename response variable; merge as desired data frame
df_test = rename(df_test, y_obs = sentiment)
df_test = df_test[, -c(1)]
df_test <- cbind(df_test, tfidf_values_test)
# save dataset to local directory as RDS file
saveRDS(df_test, "./HW1_data/imdb_dat_test.rds") # save to local directory
gc()

################################################################################
#                   *Implement stochastic optimization*                        #
################################################################################
# load data
df <- readRDS("./HW1_data/imdb_dat.rds")
X = df[, -1] %>% as.matrix()
Y = df$y_obs %>% as.numeric()

# define sigmoid function
sigmoid = function(x) {
  return(1 / (1 + exp(-x)))
}

p = ncol(X) # number of features
n = nrow(df) # number of data points
beta = c(rep(0, p)) # initialize beta
lambda = 1000
tol = 1e-5
B = 1042

# set up parallel processing
num_cores = detectCores()
registerDoParallel(cores = num_cores)

# initilize iteration
iter = 0
converged = FALSE
parameters = list()

while (!converged & iter <= 2000) {
  # sample a minibatch of data at each iteration; then scale approximation appropriately
  idx = sample(1:n, size = B, replace = TRUE)
  
  resid_sum = foreach(b = 1:B, .combine = "+", .packages = "dplyr") %dopar% {
    x_b = X[idx[b], ]
    y_b = Y[idx[b]]
    y_pred = sigmoid(beta * x_b)
    return((y_b - y_pred) * x_b)
  }
  
  # gradient
  grad = (n * resid_sum) / B - beta / lambda
  
  # update iteration
  iter = iter + 1
  rho = 1 / iter # Robbins-Monro step-size schedule of 1 / t
  beta = beta + rho * grad
  
  # moniter convergence
  norm_grad = sqrt(sum(grad^2))
  if (norm_grad < tol) {
    converged = TRUE
  }
  
  # define the log joint
  # X, Y: n-dimensional vectors
  # beta: k-dimensional vector
  log_joint = function(X, Y, beta, lambda) {
    n = nrow(X)
    p = length(beta)
    log_resp = foreach(i = 1:n, .combine = "+", .packages = "dplyr") %dopar% {
      return(Y[i] * log(sigmoid(beta %*% X[i, ])) + (1 - Y[i]) * log(sigmoid(-beta %*% X[i, ])))
    }
    return(log_resp - sum(beta^2) / (2 * lambda^2))
  }
  log_joint = log_joint(X, Y, beta, lambda)
  
  # save iterations and log joint
  parameters = rbind(parameters, c(iter, log_joint, norm_grad))
  # print(c(iter, norm_grad, log_joint)) # moniter convergence
  
}
stopImplicitCluster()

params = parameters %>% data.frame() 
colnames(params) = c("iterations", "log_joint", "grad_norm")
params = rbind(c(0, -Inf, 0), params)
params$iterations = as.numeric(params$iterations)
params$log_joint = as.numeric(params$log_joint)
p = params %>% ggplot(aes(x = iterations, y = log_joint)) + 
  geom_point(size = 0.5, colour = "blue") + geom_line(colour = "blue") +
  ggtitle("Stochastic MAP estimation of Bayesian Logistic Regression") +
  xlab("Iteration")+ ylab("Estimated Log Joint") 
ggsave("./log_joint_over_iteration.jpg", p)

# training error
(sum(sigmoid(X %*% beta) < 0.5 & Y == 0) + sum(sigmoid(X %*% beta) >= 0.5 & Y == 1)) / 10000
# test error
df_test = read_csv("./HW1_data/imdb_dat_test.rds")

# graphing coeffcients
sort(beta)[1:10] 
sort(beta, decreasing = TRUE)[1:10]

################################################################################
#    *The influence of choice of prior, step-size schedule, and batch size*    #
################################################################################
# the function performs stochastic gradient ascent to estimate B_map
stoch_grad <- function(X, Y, lambda = 1000^2, tol = 1e-5, B = 1042) {
  
  p = ncol(X) # number of features
  n = nrow(df) # number of data points
  beta = c(rep(0, p)) # initialize beta
  
  # set up parallel processing
  num_cores = detectCores()
  registerDoParallel(cores = num_cores)
  
  # initilize iteration
  iter = 0
  converged = FALSE
  parameters = list()
  
  while (!converged & iter <= 3) {
    # sample a minibatch of data at each iteration; then scale approximation appropriately
    idx = sample(1:n, size = B, replace = TRUE)
    
    resid_sum = foreach(b = 1:B, .combine = "+", .packages = "dplyr") %dopar% {
      x_b = X[idx[b], ]
      y_b = Y[idx[b]]
      y_pred = sigmoid(beta * x_b)
      return((y_b - y_pred) * x_b)
    }
    
    # gradient
    grad = (n * resid_sum) / B - beta / sqrt(lambda)
    
    # update iteration
    iter = iter + 1
    rho = 1 / iter # Robbins-Monro step-size schedule of 1 / t
    beta = beta + rho * grad
    
    # moniter convergence
    norm_grad = sqrt(sum(grad^2))
    if (norm_grad < tol) {
      converged = TRUE
    }
    
    # log_joint
    log_joint = log_joint(X, Y, beta)
    
    # save iterations and log joint
    parameters = rbind(parameters, c(iter, log_joint, norm_grad))
    
    print(parameters[iter,])
  }
  return(parameters, beta)
  stopImplicitCluster()
}

# perform 
stoch_grad(X, Y)

# test errors
predicted_probs = sigmoid(train_x %*% beta)
predicted_labels = ifelse(predicted_probs >= 0.5, 1, 0)
accuracy = accuracy(train_y, predicted_labels)
