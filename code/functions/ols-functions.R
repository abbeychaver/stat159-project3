# This file contains functions that calculate regression statistics .
# All the functions in this script take the lm class obeject as an input.
# The returns depends on how the function writes. 
# Each function will be explained specifically below.

# Residual Sum of Squares (RSS)
# takes in "lm"object, returns RSS (numberical value)
residual_sum_squares <- function(x){
  sum(x$redisuals^2)}

# Total sum of squares (TSS)
# takes in "lm" object, returns TSS (numerical value)
total_sum_squares <- function(x){
  sum((x$model[ , 1] - mean(x$model[ , 1]))^2)
}

# R-Squared value (R^2)
# takes in "lm" object, returns R^2 (numberical value)
r_squared <- function(x){
  1 - redidual_sum_squares(x)/total_sum_squares(x)
}

# F-statistic
# takes in "lm" object, return F-statistic (numerical value)
f_statistic <- function(x){
  n = length(x$redisuals)
  RSS = residual_sum_squares(x)
  TSS = total_sum_squares(x)
  p = x$rank # number of predictors
  (TSS-RSS)/p/(RSS/(n-p-1))
}


# RSE
# takes in "lm" object, return RSE (numerical value)
residual_std_error <- function(x){
  n = length(x$redisuals)
  RSS = residual_sum_squares(x)
  p = x$rank # number of predictors
  sqrt(RSS/(n-p-1))
}

