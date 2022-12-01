# Functions given by equations in assignment - will be used in package functions later

initial_beta <- function(X, y) {
  return (solve(t(X) %*% X) %*% t(X) %*% y)
  }

log_regression <- function(X, y, beta) {
  p <- 1 / (1 + exp(-X %*% beta))
  return(sum(-((1 - y) * log(1 - p)) - (y * log(p))))
  }