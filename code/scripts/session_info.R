# include all R packages in your project
library(devtools)
library(knitr)
library(rmarkdown)
library(plyr)
library(pander)
library(shiny)
library(testthat)

sink("../../session-info.txt", append = TRUE)
cat("Session Information\n\n")
print(sessionInfo())
devtools::session_info()
sink()
