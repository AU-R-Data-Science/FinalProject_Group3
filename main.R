# function to calculate inital values for optimization from least squares
calculate_beta_initial <- function(y, X){
  return((t(X)%*%X)**-1 %*% t(X) %*% y)
}

calculate_loss_fn <- function(beta, y, X){
  # i <- 1:length(y_hat)
  # Xi <- matrix(X[i ,], nrow = 1)
  # pi <- 1/(1+exp(-t(Xi) %*% beta))
  beta <- beta
  y <- y
  X <- X
  sum <- 0
  for(i in 1:length(y_hat)){
    yi <- y[i]
    Xi <- matrix(X[i ,])
    pi <- 1/(1+exp(-t(Xi) %*% beta))
    sum <- sum + (-yi * log(pi) - (1 - yi) * log(1 - pi))
  }
  return(sum)
}

beta_hat <- function(y, X) {
  beta_est <- optim(calculate_beta_initial(y, X), calculate_loss_fn, y = y, X = X)$par
  
  output <- list("beta_hat" = beta_est, "response" = y, "predictors" = X)
  class(output) = "lsoptim"

  return(output)

}

# function to create bootstrap intervals for each coefficient in vector beta
# n is number of bootstraps and b is beta matrix
calculate_bootstrap_cis <- function(alpha, N = 20, y, X){
  B <- length(calculate_beta_initial(y, X))
  # (dimension of x) + 1 is last number below
  # number of predictors based on problem
  beta_mat <- matrix(NA, nrow = N, ncol = B)

  for(n in 1:N){
    boot_data <- data[sample(1:nrow(data), nrow(data), replace = T) ,]
    y_hat <- model_matrix[,1]
    X_hat <- model_matrix[, !1]
    model <- beta_hat(y_hat, X_hat)
    beta_mat[n, ] <- model[["beta_hat"]]
  }
  
  return(beta_mat)
}

# function to plot the logistic curve
plot_logistic <- function(y, X){
  library(ggplot2)
  ggplot(data, aes(x=X[, 1], y=y)) + 
    geom_point(alpha=.5) +
    stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))
}

# function to create confusion matrix
confusion_matrix <- function(){

}


###TESTING#######################
data <- ISLR::Default
summary(data)
num_rows <- nrow(data)

vars <- default~student+balance+income
utils::str(m <- model.frame(vars, data))
model_matrix <- model.matrix(vars, m)

#training n testing data
set.seed(13)
sample <- sample(c(TRUE, FALSE), num_rows, replace = TRUE, prob = c(0.8, 0.2))
train <- model_matrix[sample, ]
test <- model_matrix[!sample, ]

#fitting the model
y_hat <- model_matrix[, 1]
X_hat <- model_matrix[, c(2, 3, 4)]
model <- beta_hat(y_hat, X_hat)
summary(model)

boots <- calculate_bootstrap_cis(0.05, 20, y_hat, X_hat)


