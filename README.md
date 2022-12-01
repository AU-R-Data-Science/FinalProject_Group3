# Final Project for STAT 5210, Group 3
R package implementing logistic regression using numerical optimization.

The estimator to be computed using numerical optimization:

<img src="https://latex.codecogs.com/svg.image?%5Chat%7B%5Cbeta%7D%20:=%20%5Carg%5Cmin_%7B%5Cbeta%7D%20%5Csum_%7Bi%20=%201%7D%5E%7Bn%7D(-y_%7Bi%7D%20%5Ccdot%20ln(p_%7Bi%7D)%20-%20(1%20-%20y_%7Bi%7D)%20%5Ccdot%20ln(1%20-%20p_%7Bi%7D))">

The outputs include:
- initial values for optimization
- bootstrap confidence intervals (default number of bootstraps = 20, alpha provided by user)
- a plot of the fitted logistic curve
- confusion matrix
- prevalence, accuracy, sensitivity, specificity, false discovery rate, diagnostic odds ratio
