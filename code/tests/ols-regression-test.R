# Setting up and context
require(testthat)
library(testthat)
source("../functions/ols-regression-function.R")
context("Test for statistics calculated from functions for ols regression")

#load the multiple regression for testing
reg <- lm(mpg ~disp + hp, data = mtcars)
regsum <- summary(reg)

#test the residual_sum_squares function
test_that("RSS function is correct", {
  expect_equal(residual_sum_squares(reg), sum(reg$residuals^2))
  expect_type(residual_sum_squares(reg), 'double')
  expect_length(residual_sum_squares(reg), 1)
})

#test the total_sum_squares function
test_that("TSS function is correct", {
  expect_equal(total_sum_squares(reg), sum((mtcars$mpg - mean(mtcars$mpg))^2))
  expect_length(total_sum_squares(reg),1)
  expect_type(total_sum_squares(reg),'double')
  
})

#test the r_square function
test_that ("R square function is correct",{
  expect_equal(r_squared(reg),regsum$r.squared)
  expect_length(r_squared(reg), 1)
  expect_type(r_squared(reg),'double')
})

#test the F-statistic function
test_that ("F-statistic function is correct",{
  expect_equivalent(f_statistic(reg),regsum$fstatistic[1])
  expect_length(f_statistic(reg),1)
  expect_type(f_statistic(reg), 'double')
})

#test the residual_std_error function
test_that ("RSD function is correct",{
  expect_equal(residual_std_error(reg), regsum$sigma)
  expect_length(residual_std_error(reg), 1)
  expect_type(residual_std_error(reg),'double')
})



