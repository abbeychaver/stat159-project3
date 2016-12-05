#---------------------------------------------------------------------
# Exploratory Data Analysis Functions

# Description: Functions for plotting relationships between 
# outcome gaps and our explanatory variables
#---------------------------------------------------------------------


library(ggplot2)
cohort_plot <- function(var_string, cohort_comparison) {
  png(paste("images/eda_scatterplots/", var_string, "_cohort.png", sep=""))
  plot <- ggplot(cohort_comparison, aes_string(x="INEXPFTE", y = var_string, 
                                col="cohort")) +
    geom_point() + geom_line() + 
    labs(xlab("Expenditure per Student")) +
    ggtitle(label = (var_string))
  print(plot)
  dev.off()
}

all_plot <- function(var_string, df) {
  png(paste("images/eda_scatterplots/", var_string, ".png", sep=""))
  plot <- ggplot(df, aes_string(x="INEXPFTE", y = var_string, 
                                       col="TYPE")) +
    geom_point() + 
    labs(xlab("Expenditure per Student")) +
    ggtitle(label = (var_string))
  print(plot)
  dev.off()
}

zoom_plot <- function(var_string, df) {
  png(paste("images/eda_scatterplots/", var_string, "_zoom.png", sep=""))
  plot <- ggplot(df, aes_string(x="INEXPFTE", y = var_string, 
                        col="TYPE")) +
    geom_point() + scale_y_continuous(limits=c(-1, 1)) + 
    scale_x_continuous(limits=c(0, 15000)) +
    labs(xlab("Expenditure per Student")) +
    ggtitle(label = (paste("Zoom:",var_string)))
  print(plot)
  dev.off()
}

cohort_plot("gap_completion_white_hispanic", cohort_comparison)
