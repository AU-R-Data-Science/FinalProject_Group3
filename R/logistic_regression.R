log_regression <- function(X, y, beta) {
  p <- 1 / (1 + exp(-X %*% beta))
  return(sum(-((1 - y) * log(1 - p)) - (y * log(p))))
}

initial_beta_least_squares <- function(X, y) {
  return (solve(t(X) %*% X) %*% t(X) %*% y)
}

#function for generating β^ - vector of logistic regression coefficients
beta_hat <- function(predictor, response) {
  return (optim(get_initial_beta(predictor, response), log_regression, X = predictor, y = response)$par)
}

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
  
  return(list("Prevalence" = prevalence, "Accuracy" = accuracy, "Sensitivity" = sensitiviy, "Specificity" = specificity, "False Discovery Rate" = fdr, "Diagnostic Odds Ratio" = dor))
}

#plotting metrics starter function - with prevalence as example
plot_metrics_starter_function <- function(predictor, response) {
  plot_df <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(plot_df) <- c("Cut_Off", "Prevalence")
  
  for(i in seq(from = 0.1, to = 0.9, by = 0.1)) {
    plot_df[nrows(plot_df)+1, ] <- c(i, confusion_matrix_and_metrics(predictor, response, i)$Prevalence)
    }
  #this will give you a data frame with the specified metric for each cutoff value. plot data frame from here / repeat for all metrics listed
}

