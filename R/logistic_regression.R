
xx.data <- c(1,2,3,4,5,6,7,8,9)
xx <- matrix(xx.data,nrow=9,ncol=1,byrow=TRUE)
yye = xx
yy  = xx
rr = matrix(runif(9),nrow=9,ncol=1,byrow=TRUE)
XX = matrix(0,nrow=9,ncol=2,byrow=TRUE)

for (i in  1:9 ) {
  yy[i] = xx[i]*xx[i] 
  yye[i] = yy[i] + rr[i]*10
  XX[i,1] = xx[i]
  XX[i,2] = xx[i]*xx[i]
}
xx
yye
rr
XX
xhat = solve(t(XX) %*% XX ) %*% t(XX) %*%yye

yhat = XX %*% xhat
err = yhat - yye


log_regression <- function(X, y, beta) {
  p <- 1 / (1 + exp(-X %*% beta))
  return(sum(-((1 - y) * log(1 - p)) - (y * log(p))))
}

#'Initial Coefficients for Logistic Regression
#'
#'@description
#'This function generates the initial coefficients for logistic regression using the least squares formula.
#'
#'@param X An matrix containing the values of the predictors.
#'@param y A vector containing the values of responses.
#'
#'@return A vector containing the initial values of the coefficients for logisitic regression
#'
#'@author Cole Kenney
#'@export
initial_beta_least_squares <- function(X, y) {
  return (solve(t(X) %*% X) %*% t(X) %*% y)
}

#'Logistic Regression Coefficients Generation
#'
#'@description
#'This function generates optimal coefficients for logistic regression.
#'
#'@param predictor A matrix containing values of the predictors.
#'@param response A vector containing values of the responses.
#'
#'@return A vector containing the optimal set of coefficients for logistic regression.
#'
#'@author Cole Kenney
#'@export
beta_hat <- function(predictor, response) {
  return (optim(get_initial_beta(predictor, response), log_regression, X = predictor, y = response)$par)
}


#fit logistic regression model
model <- glm(vs ~ hp, data=mtcars, family=binomial)

#define new data frame that contains predictor variable
newdata <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=500))

#use fitted model to predict values of vs
newdata$vs = predict(model, newdata, type="response")

#plot logistic regression curve
plot(vs ~ hp, data=mtcars, col="steelblue")
lines(vs ~ hp, newdata, lwd=2)


plot <- function(){
  
}



#' Confusion Matrix and Metrics
#' 
#' @description 
#' This function performs logistic regression and then computes a confusion matrix and some common metrics from the confusion matrix.
#' 
#' @param predictor A matrix containing values of the predictors.
#' @param response A vector containing values of the responses.
#' @param cutoff A double value - predictions greater than the cutoff are set to one, predictions less than the cutoff are set to zero.
#' 
#' @return A list containing the following values:
#' \describe{
#' \item{tp}{The double value of true postives. Actual = predicted = 1.}
#' \item{tn}{The double value of true negatives. Actual = predicted = 0.}
#' \item{fp}{The double value of false postives. Actual = 0, predicted = 1.}
#' \item{fn}{The double value of false negatives. Actual = 1, predicted = 0.}
#' \item{prev}{The double value of prevalence.}
#' \item{acc}{The double value of accuracy.}
#' \item{sens}{The double value of sensitivity.}
#' \item{spec}{The double value of specificity.}
#' \item{fdr}{The double value of the false discovery rate.}
#' \item{dor}{The double value of the diagnostic odds ratio.}
#' }
#' @author Cole Kenney
#' @export
confusion_matrix_and_metrics <- function(predictor, response, cutoff) {
  optim_beta <- beta_hat(predictor, response)
  p <- 1 / (1 + exp(-predictor %*% optim_beta))
  predictions <- ifelse(p > cutoff, 1, 0)
  
  tp  <- sum((response == 1) & (predictions == 1))
  tn  <- sum((response == 0) & (predictions == 0))
  fp  <- sum((response == 0) & (predictions == 1))
  fn  <- sum((response == 1) & (predictions == 0))
  
  p_predictions = tp + fp
  n_predictions = tn + fn
  total = p_predictions + n_predictions
  
  prevalence = p_predictions / total
  accuracy = (tn + tp) / total
  sensitiviy = tp/(tp + fn)
  specificity = tn/(tn + fp)
  fdr = fp/p_predictions
  dor = (sensitivity/(1-specificity))/((1-sensitivity)/specificity)
  
  return(list("tp" = tp,
              "tn" = tn,
              "fp" = fp,
              "fn" = fn,
              "prev" = prevalence, 
              "acc" = accuracy, 
              "sens" = sensitiviy, 
              "spec" = specificity, 
              "fdr" = fdr, 
              "dor" = dor))
}



#' Plot (metric)
#' 
#' @description 
#' This function plots (metric) over cutoff values from 0.1 to 0.9.
#' 
#' @param predictor A matrix containing values of the predictors.
#' @param response A vector containing values of the responses.
#' 
#' @author (name)
#' @export
plot_metrics_starter_function <- function(predictor, response) {
  plot_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(plot_df) <- c("Cut_Off", "Prevalence")
  
  for(i in seq(from = 0.1, to = 0.9, by = 0.1)) {
    plot_df[nrows(plot_df)+1, ] <- c(i, confusion_matrix_and_metrics(predictor, response, i)$prev)
  }
  
  #plot data frame here
}

# Starter function for plotting confusion matrix metrics. 
# Please create plotting code / edit descriptions /
# create function for each metric. Example is using prevalence.
