##################################################
# Correlation Analysis
##################################################

# This script is for examing the relationship between the gap metrics 
# and school expenditure per student. We also do some analysis on
# school revenue per student. 

# Three regression between completion gap and school revenue (TUITFTE) per student: linear
# Three regression between income gap and school revenue (TUITFTE) per student: linear-log

# Three linear regressions between completion gap and the 
# instructional expenditures per full-time student (INEXPFTE) 
# in public schools, private school, private non-profit, private for-profit


# Three linear regressions each between earnings gap and the 
# instructional expenditures per full-time student (INEXPFTE) 
# in public schools, private school, private non-profit, private for-profit

# Load Libraries
library(plyr)
library(xtable)
library(ggplot2)

# Loading data sets
completion_W_A <- read.csv("data/Completion_W_A.csv")
completion_W_B <- read.csv("data/Completion_W_B.csv")
completion_W_H <- read.csv("data/Completion_W_H.csv")
income_gap <- read.csv("data/Income.csv")



# Exploring relationship between gap metrics and school revenue by graphing
# Completion gap
png('images/gap_completion_white_black_TUITFTE.png')
with(completion_W_B, scatter.smooth(gap_completion_white_black~TUITFTE))
title(main = "linear")
dev.off()

png('images/gap_completion_white_asian_TUITFTE.png')
with(completion_W_A, scatter.smooth(gap_completion_white_asian~TUITFTE))
title(main = 'linear')
dev.off()

png('images/gap_completion_white_hispanic_TUITFTE.png')
with(completion_W_H, scatter.smooth(gap_completion_white_hispanic~TUITFTE))
title(main = 'linear')
dev.off()

with(completion_W_B, scatter.smooth(gap_completion_white_black~log(TUITFTE)))
title(main = 'linear-log')

with(completion_W_A, scatter.smooth(gap_completion_white_asian~log(TUITFTE)))
title(main = 'linear-log')

with(completion_W_H, scatter.smooth(gap_completion_white_hispanic~log(TUITFTE)))
title(main = 'linear-log')

# NaNs and errors generated from log(gap_completion_white_xx) due to negative values
#with(completion_W_B, scatter.smooth(log(gap_completion_white_black)~log(TUITFTE)))
#title(main = 'log-log')
#with(completion_W_A, scatter.smooth(log(gap_completion_white_asian)~log(TUITFTE)))
#title(main = 'log-log')
#with(completion_W_H, scatter.smooth(log(gap_completion_white_hispanic)~log(TUITFTE)))
#title(main = 'log-log')
#with(completion_W_B, scatter.smooth(log(gap_completion_white_black)~TUITFTE)))
#title(main = 'log-linear')
#with(completion_W_A, scatter.smooth(log(gap_completion_white_asian)~(TUITFTE)))
#title(main = 'log-linear')
#with(completion_W_H, scatter.smooth(log(gap_completion_white_hispanic)~(TUITFTE)))
#title(main = 'log-linear')

# The linear model gives a almost straight fitted line, while the linear-log model
# shows a curve (The log(TUIFTE) with power of 4 gives a almost straight fitted line,
# but I feel like it is hard to make a discusion). Thus, I choose the linear model 
# in this case. All three give negative relationship. 

# Income gap
with(income_gap, scatter.smooth(gap_earnings_high_low~TUITFTE))
title(main = "linear")
with(income_gap, scatter.smooth(gap_earnings_high_mid~TUITFTE))
title(main = 'linear')
with(income_gap, scatter.smooth(gap_earnings_mid_low~TUITFTE))
title(main = 'linear')

png('images/income_gap_high_low_logTUITFTE.png')
with(income_gap, scatter.smooth(gap_earnings_high_low~log(TUITFTE)))
title(main = 'linear-log')
dev.off()

png('images/income_gap_high_low_logTUITFTE.png')
with(income_gap, scatter.smooth(gap_earnings_high_mid~log(TUITFTE)))
title(main = 'linear-log')
dev.off()

png('images/income_gap_mid_low_logTUITFTE.png')
with(income_gap, scatter.smooth(gap_earnings_mid_low~log(TUITFTE)))
title(main = 'linear-log')
dev.off()

# Using linear-log model for in this case, due to better fitted line. 


# Fitting ols regression and regression summary statistics
# Completion gap
ols_regression_wa<-lm(completion_W_A$gap_completion_white_asian ~ completion_W_A$TUITFTE)
ols_summary_wa<- summary(ols_regression_wa)

ols_regression_wb<-lm(completion_W_B$gap_completion_white_black ~ completion_W_B$TUITFTE)
ols_summary_wb <- summary(ols_regression_wb)

ols_regression_wh<-lm(completion_W_H$gap_completion_white_hispanic ~ completion_W_H$TUITFTE)
ols_summary_wh<- summary(ols_regression_wh)

# Income gap
ols_regression_hl<-lm(income_gap$gap_earnings_high_low ~ log(income_gap$TUITFTE))
ols_summary_hl<- summary(ols_regression_hl)

ols_regression_hm<-lm(income_gap$gap_earnings_high_mid ~ log(income_gap$TUITFTE))
ols_summary_hm <- summary(ols_regression_hm)

ols_regression_ml<-lm(income_gap$gap_earnings_mid_low ~ log(income_gap$TUITFTE))
ols_summary_ml<- summary(ols_regression_ml)




# Regression coefficients
# Completion gap
ols_coefficients_wa <- coef(ols_regression_wa)
ols_coefficients_wb <- coef(ols_regression_wb)
ols_coefficients_wh <- coef(ols_regression_wh)
# Income gap
ols_coefficients_hl <- coef(ols_regression_hl)
ols_coefficients_hm <- coef(ols_regression_hm)
ols_coefficients_ml <- coef(ols_regression_ml)

# Computing MSE from ols regression
# Completion gap
ols_mse_wa <- mean(ols_regression_wa$residuals^2)
ols_mse_wb <- mean(ols_regression_wb$residuals^2)
ols_mse_wh <- mean(ols_regression_wh$residuals^2)
# Income gap
ols_mse_hl <- mean(ols_regression_hl$residuals^2)
ols_mse_hm <- mean(ols_regression_hm$residuals^2)
ols_mse_ml <- mean(ols_regression_ml$residuals^2)



# Spliting data into public and private school
# Completion gap
public_school_wb<-completion_W_B[completion_W_B$CONTROL == 1,]
private_school_wb <- completion_W_B[completion_W_B$CONTROL != 1,]
public_school_wa<-completion_W_A[completion_W_A$CONTROL == 1,]
private_school_wa <- completion_W_A[completion_W_A$CONTROL != 1,]
public_school_wh<-completion_W_H[completion_W_H$CONTROL == 1,]
private_school_wh <- completion_W_H[completion_W_H$CONTROL != 1,]

# Income gap
public_school_income<-income_gap[income_gap$CONTROL == 1,]
private_school_income <- income_gap[income_gap$CONTROL != 1,]

# Spliting private school into nonprofit and for-profit
private_nonpft_wa <- private_school_wa[private_school_wa$CONTROL == 2,]
private_pft_wa <- private_school_wa[private_school_wa$CONTROL == 3,]
private_nonpft_wh <- private_school_wh[private_school_wh$CONTROL == 2,]
private_pft_wh <- private_school_wh[private_school_wh$CONTROL == 3,]
private_nonpft_wb <- private_school_wb[private_school_wb$CONTROL == 2,]
private_pft_wb <- private_school_wb[private_school_wb$CONTROL == 3,]

private_nonpft_income <- income_gap[income_gap$CONTROL == 2,]
private_pft_income <- income_gap[income_gap$CONTROL == 3,]

# Examing relationship...
# Completion gap
#with(public_school_wb, scatter.smooth(gap_completion_white_black~TUITFTE))
#title(main = "linear")
#with(public_school_wa, scatter.smooth(gap_completion_white_asian~TUITFTE))
#title(main = 'linear')
#with(public_school_wh, scatter.smooth(gap_completion_white_hispanic~TUITFTE))
#title(main = 'linear')

#with(public_school_wb, scatter.smooth(gap_completion_white_black~log(TUITFTE)))
#title(main = 'linear-log')
#with(public_school_wa, scatter.smooth(gap_completion_white_asian~log(TUITFTE)))
#title(main = 'linear-log')
#with(public_school_wh, scatter.smooth(gap_completion_white_hispanic~log(TUITFTE)))
#title(main = 'linear-log')

# Income gap
#with(public_school_income, scatter.smooth(gap_earnings_high_low~TUITFTE))
#title(main = "linear")
#with(public_school_income, scatter.smooth(gap_earnings_high_mid~TUITFTE))
#title(main = 'linear')
#with(public_school_income, scatter.smooth(gap_earnings_mid_low~TUITFTE))
#title(main = 'linear')

#with(public_school_income, scatter.smooth(gap_earnings_high_low~log(TUITFTE)))
#title(main = 'linear-log')
#with(public_school_income, scatter.smooth(gap_earnings_high_mid~log(TUITFTE)))
#title(main = 'linear-log')
#with(public_school_income, scatter.smooth(gap_earnings_mid_low~log(TUITFTE)))
#title(main = 'linear-log')




# Fitting ols regression and regression summary statistics
# Public schools
# Completion gap
ols_regression_pbc_wa<-lm(public_school_wa$gap_completion_white_asian ~ 
                            public_school_wa$INEXPFTE)
ols_summary_pbc_wa<- summary(ols_regression_pbc_wa)

ols_regression_pbc_wb<-lm(public_school_wb$gap_completion_white_black ~ 
                            public_school_wb$INEXPFTE)
ols_summary_pbc_wb<- summary(ols_regression_pbc_wb)

ols_regression_pbc_wh<-lm(public_school_wh$gap_completion_white_hispanic ~ 
                            public_school_wh$INEXPFTE)
ols_summary_pbc_wh<- summary(ols_regression_pbc_wh)

# Income gap
ols_regression_pbc_hl<-lm(public_school_income$gap_earnings_high_low ~ 
                            public_school_income$INEXPFTE)
ols_summary_pbc_hl<- summary(ols_regression_pbc_hl)

ols_regression_pbc_hm<-lm(public_school_income$gap_earnings_high_mid ~ 
                            public_school_income$INEXPFTE)
ols_summary_pbc_hm<- summary(ols_regression_pbc_hm)

ols_regression_pbc_ml<-lm(public_school_income$gap_earnings_mid_low ~ 
                            public_school_income$INEXPFTE)
ols_summary_pbc_ml<- summary(ols_regression_pbc_ml)


# Private schools
# Completion gap
ols_regression_pvt_wa<-lm(private_school_wa$gap_completion_white_asian ~ 
                            private_school_wa$INEXPFTE)
ols_summary_pvt_wa<- summary(ols_regression_pvt_wa)

ols_regression_pvt_wb<-lm(private_school_wb$gap_completion_white_black ~ 
                            private_school_wb$INEXPFTE)
ols_summary_pvt_wb<- summary(ols_regression_pvt_wb)

ols_regression_pvt_wh<-lm(private_school_wh$gap_completion_white_hispanic ~ 
                            private_school_wh$INEXPFTE)
ols_summary_pvt_wh<- summary(ols_regression_pvt_wh)

# Income gap
ols_regression_pvt_hl<-lm(private_school_income$gap_earnings_high_low ~ 
                            private_school_income$INEXPFTE)
ols_summary_pvt_hl<- summary(ols_regression_pvt_hl)

ols_regression_pvt_hm<-lm(private_school_income$gap_earnings_high_mid ~ 
                            private_school_income$INEXPFTE)
ols_summary_pvt_hm<- summary(ols_regression_pvt_hm)

ols_regression_pvt_ml<-lm(private_school_income$gap_earnings_mid_low ~ 
                            private_school_income$INEXPFTE)
ols_summary_pvt_ml<- summary(ols_regression_pvt_ml)

# Private for profit
# Completion gap
ols_regression_pft_wa<-lm(private_pft_wa$gap_completion_white_asian ~ 
                            private_pft_wa$INEXPFTE)
ols_summary_pft_wa<- summary(ols_regression_pft_wa)

ols_regression_pft_wb<-lm(private_pft_wb$gap_completion_white_black ~ 
                            private_pft_wb$INEXPFTE)
ols_summary_pft_wb<- summary(ols_regression_pft_wb)

ols_regression_pft_wh<-lm(private_pft_wh$gap_completion_white_hispanic ~ 
                            private_pft_wh$INEXPFTE)
ols_summary_pft_wh<- summary(ols_regression_pft_wh)

# Income gap
ols_regression_pft_hl<-lm(private_pft_income$gap_earnings_high_low ~ 
                            private_pft_income$INEXPFTE)
ols_summary_pft_hl<- summary(ols_regression_pft_hl)

ols_regression_pft_hm<-lm(private_pft_income$gap_earnings_high_mid ~ 
                            private_pft_income$INEXPFTE)
ols_summary_pft_hm<- summary(ols_regression_pft_hm)

ols_regression_pft_ml<-lm(private_pft_income$gap_earnings_mid_low ~ 
                            private_pft_income$INEXPFTE)
ols_summary_pft_ml<- summary(ols_regression_pft_ml)

# Private for nonprofit
# Completion gap
ols_regression_nonpft_wa<-lm(private_nonpft_wa$gap_completion_white_asian ~ 
                               private_nonpft_wa$INEXPFTE)
ols_summary_nonpft_wa<- summary(ols_regression_nonpft_wa)

ols_regression_nonpft_wb<-lm(private_nonpft_wb$gap_completion_white_black ~ 
                               private_nonpft_wb$INEXPFTE)
ols_summary_nonpft_wb<- summary(ols_regression_nonpft_wb)

ols_regression_nonpft_wh<-lm(private_nonpft_wh$gap_completion_white_hispanic ~ 
                               private_nonpft_wh$INEXPFTE)
ols_summary_nonpft_wh<- summary(ols_regression_nonpft_wh)

# Income gap
ols_regression_nonpft_hl<-lm(private_nonpft_income$gap_earnings_high_low ~ 
                               private_nonpft_income$INEXPFTE)
ols_summary_nonpft_hl<- summary(ols_regression_nonpft_hl)

ols_regression_nonpft_hm<-lm(private_nonpft_income$gap_earnings_high_mid ~ 
                               private_nonpft_income$INEXPFTE)
ols_summary_nonpft_hm<- summary(ols_regression_nonpft_hm)

ols_regression_nonpft_ml<-lm(private_nonpft_income$gap_earnings_mid_low ~ 
                               private_nonpft_income$INEXPFTE)
ols_summary_nonpft_ml<- summary(ols_regression_nonpft_ml)


# Regression coefficients
# Public
# Completion gap
ols_coefficients_pbc_wa <- coef(ols_regression_pbc_wa)
ols_coefficients_pbc_wb <- coef(ols_regression_pbc_wb)
ols_coefficients_pbc_wh <- coef(ols_regression_pbc_wh)
# Income gap
ols_coefficients_pbc_hl <- coef(ols_regression_pbc_hl)
ols_coefficients_pbc_hm <- coef(ols_regression_pbc_hm)
ols_coefficients_pbc_ml <- coef(ols_regression_pbc_ml)
# Private
# Completion gap
ols_coefficients_pvt_wa <- coef(ols_regression_pvt_wa)
ols_coefficients_pvt_wb <- coef(ols_regression_pvt_wb)
ols_coefficients_pvt_wh <- coef(ols_regression_pvt_wh)
# Income gap
ols_coefficients_pvt_hl <- coef(ols_regression_pvt_hl)
ols_coefficients_pvt_hm <- coef(ols_regression_pvt_hm)
ols_coefficients_pvt_ml <- coef(ols_regression_pvt_ml)
# Private for nonprofit
# Completion gap
ols_coefficients_nonpft_wa <- coef(ols_regression_nonpft_wa)
ols_coefficients_nonpft_wb <- coef(ols_regression_nonpft_wb)
ols_coefficients_nonpft_wh <- coef(ols_regression_nonpft_wh)
# Income gap
ols_coefficients_nonpft_hl <- coef(ols_regression_nonpft_hl)
ols_coefficients_nonpft_hm <- coef(ols_regression_nonpft_hm)
ols_coefficients_nonpft_ml <- coef(ols_regression_nonpft_ml)
# Private for profit
# Completion gap
ols_coefficients_pft_wa <- coef(ols_regression_pft_wa)
ols_coefficients_pft_wb <- coef(ols_regression_pft_wb)
ols_coefficients_pft_wh <- coef(ols_regression_pft_wh)
# Income gap
ols_coefficients_pft_hl <- coef(ols_regression_pft_hl)
ols_coefficients_pft_hm <- coef(ols_regression_pft_hm)
ols_coefficients_pft_ml <- coef(ols_regression_pft_ml)



# Computing MSE from ols regression
# public
# Completion gap
ols_mse_pbc_wa <- mean(ols_regression_pbc_wa$residuals^2)
ols_mse_pbc_wb <- mean(ols_regression_pbc_wb$residuals^2)
ols_mse_pbc_wh <- mean(ols_regression_pbc_wh$residuals^2)
# Income gap
ols_mse_pbc_hl <- mean(ols_regression_pbc_hl$residuals^2)
ols_mse_pbc_hm <- mean(ols_regression_pbc_hm$residuals^2)
ols_mse_pbc_ml <- mean(ols_regression_pbc_ml$residuals^2)

# private
# Completion gap
ols_mse_pvt_wa <- mean(ols_regression_pvt_wa$residuals^2)
ols_mse_pvt_wb <- mean(ols_regression_pvt_wb$residuals^2)
ols_mse_pvt_wh <- mean(ols_regression_pvt_wh$residuals^2)
# Income gap
ols_mse_pvt_hl <- mean(ols_regression_pvt_hl$residuals^2)
ols_mse_pvt_hm <- mean(ols_regression_pvt_hm$residuals^2)
ols_mse_pvt_ml <- mean(ols_regression_pvt_ml$residuals^2)

# private for nonprofit
# Completion gap
ols_mse_nonpft_wa <- mean(ols_regression_nonpft_wa$residuals^2)
ols_mse_nonpft_wb <- mean(ols_regression_nonpft_wb$residuals^2)
ols_mse_nonpft_wh <- mean(ols_regression_nonpft_wh$residuals^2)
# Income gap
ols_mse_nonpft_hl <- mean(ols_regression_nonpft_hl$residuals^2)
ols_mse_nonpft_hm <- mean(ols_regression_nonpft_hm$residuals^2)
ols_mse_nonpft_ml <- mean(ols_regression_nonpft_ml$residuals^2)

# private for profit
# Completion gap
ols_mse_pft_wa <- mean(ols_regression_pft_wa$residuals^2)
ols_mse_pft_wb <- mean(ols_regression_pft_wb$residuals^2)
ols_mse_pft_wh <- mean(ols_regression_pft_wh$residuals^2)

# Income gap
ols_mse_pft_hl <- mean(ols_regression_pft_hl$residuals^2)
ols_mse_pft_hm <- mean(ols_regression_pft_hm$residuals^2)
ols_mse_pft_ml <- mean(ols_regression_pft_ml$residuals^2)


# Saving output to RData file
save(ols_regression_wa, ols_regression_wb, ols_regression_wh, 
     ols_summary_wa, ols_summary_wb, ols_summary_wh,
     ols_coefficients_wa,ols_coefficients_wh, ols_coefficients_wb,
     ols_mse_wa, ols_mse_wh, ols_mse_wb, 
     ols_regression_pbc_wa, ols_regression_pbc_wb, ols_regression_pbc_wh,
     ols_regression_pvt_wa, ols_regression_pvt_wb, ols_regression_pvt_wh,
     ols_regression_pft_wa, ols_regression_pft_wb, ols_regression_pft_wh,
     ols_regression_nonpft_wa, ols_regression_nonpft_wb, ols_regression_nonpft_wh,
     ols_summary_pbc_wa, ols_summary_pbc_wb, ols_summary_pbc_wh,
     ols_summary_pvt_wa, ols_summary_pvt_wb, ols_summary_pvt_wh,
     ols_summary_pft_wa, ols_summary_pft_wb, ols_summary_pft_wh,
     ols_summary_nonpft_wa, ols_summary_nonpft_wb, ols_summary_nonpft_wh,
     ols_coefficients_pbc_wa, ols_coefficients_pbc_wb, ols_coefficients_pbc_wh,
     ols_coefficients_pvt_wa, ols_coefficients_pvt_wb, ols_coefficients_pvt_wh,
     ols_coefficients_pft_wa, ols_coefficients_pft_wb, ols_coefficients_pft_wh,
     ols_coefficients_nonpft_wa, ols_coefficients_nonpft_wb, ols_coefficients_nonpft_wh,
     ols_mse_pbc_wa, ols_mse_pbc_wb, ols_mse_pbc_wh,
     ols_mse_pvt_wa, ols_mse_pvt_wb, ols_mse_pvt_wh,
     ols_mse_pft_wa, ols_mse_pft_wb, ols_mse_pft_wh,
     ols_mse_nonpft_wa, ols_mse_nonpft_wb, ols_mse_nonpft_wh,
     ols_regression_hl, ols_regression_hm, ols_regression_ml, 
     ols_summary_hl, ols_summary_hm, ols_summary_ml,
     ols_coefficients_hl,ols_coefficients_hm, ols_coefficients_ml,
     ols_mse_hl, ols_mse_hm, ols_mse_ml, 
     ols_regression_pbc_hl, ols_regression_pbc_hm, ols_regression_pbc_ml,
     ols_regression_pvt_hl, ols_regression_pvt_hm, ols_regression_pvt_ml,
     ols_regression_pft_hl, ols_regression_pft_hm, ols_regression_pft_ml,
     ols_regression_nonpft_hl, ols_regression_nonpft_hm, ols_regression_nonpft_ml,
     ols_summary_pbc_hl, ols_summary_pbc_hm, ols_summary_pbc_ml,
     ols_summary_pvt_hl, ols_summary_pvt_hm, ols_summary_pvt_ml,
     ols_summary_pft_hl, ols_summary_pft_hm, ols_summary_pft_ml,
     ols_summary_nonpft_hl, ols_summary_nonpft_hm, ols_summary_nonpft_ml,
     ols_coefficients_pbc_hl, ols_coefficients_pbc_hm, ols_coefficients_pbc_ml,
     ols_coefficients_pvt_hl, ols_coefficients_pvt_hm, ols_coefficients_pvt_ml,
     ols_coefficients_pft_hl, ols_coefficients_pft_hm, ols_coefficients_pft_ml,
     ols_coefficients_nonpft_hl, ols_coefficients_nonpft_hm, ols_coefficients_nonpft_ml,
     ols_mse_pbc_hl, ols_mse_pbc_hm, ols_mse_pbc_ml,
     ols_mse_pvt_hl, ols_mse_pvt_hm, ols_mse_pvt_ml,
     ols_mse_pft_hl, ols_mse_pft_hm, ols_mse_pft_ml,
     ols_mse_nonpft_hl, ols_mse_nonpft_hm, ols_mse_nonpft_ml,
     file="data/second-model.RData")

# Sinking output to txt file
sink('data/second-model-output.txt')
cat('\nOLS regression summary statistics between white and asian\n')
print(ols_summary_wa)
cat('\nOLS MSE between white and asian\n')
print(ols_mse_wa)
cat('\nOLS regression coefficients between white and asian\n')
print(ols_coefficients_wa)
cat('\nOLS regression summary statistics between white and black\n')
print(ols_summary_wb)
cat('\nOLS MSE between white and black\n')
print(ols_mse_wb)
cat('\nOLS regression coefficients between white and asian\n')
print(ols_coefficients_wb)
cat('\nOLS regression summary statistics between white and hispanic\n')
print(ols_summary_wh)
cat('\nOLS MSE for white and hispanic\n')
print(ols_mse_wh)
cat('\nOLS regression coefficients between white and hispanic\n')
print(ols_coefficients_wh)
cat('\nOLS regression summary statistics between white and asian in public school\n')
print(ols_summary_pbc_wa)
cat('\nOLS MSE between white and asian in public school\n')
print(ols_mse_pbc_wa)
cat('\nOLS regression coefficients between white and asian in public school\n')
print(ols_coefficients_pbc_wa)
cat('\nOLS regression summary statistics between white and black in public school\n')
print(ols_summary_pbc_wb)
cat('\nOLS MSE between white and black in public school\n')
print(ols_mse_pbc_wb)
cat('\nOLS regression coefficients between white and asian in public school\n')
print(ols_coefficients_pbc_wb)
cat('\nOLS regression summary statistics between white and hispanic in private school\n')
print(ols_summary_pvt_wh)
cat('\nOLS MSE for white and hispanic in private school\n')
print(ols_mse_pvt_wh)
cat('\nOLS regression coefficients between white and hispanic in private school\n')
print(ols_coefficients_pvt_wh)
cat('\nOLS regression summary statistics between white and asian in private school\n')
print(ols_summary_pvt_wa)
cat('\nOLS MSE between white and asian in private school\n')
print(ols_mse_pvt_wa)
cat('\nOLS regression coefficients between white and asian in private school\n')
print(ols_coefficients_pvt_wa)
cat('\nOLS regression summary statistics between white and black in private school\n')
print(ols_summary_pvt_wb)
cat('\nOLS MSE between white and black in private school\n')
print(ols_mse_pvt_wb)
cat('\nOLS regression coefficients between white and asian in private school\n')
print(ols_coefficients_pvt_wb)
cat('\nOLS regression summary statistics between white and hispanic in private school\n')
print(ols_summary_pvt_wh)
cat('\nOLS MSE for white and hispanic in private school\n')
print(ols_mse_pvt_wh)
cat('\nOLS regression coefficients between white and hispanic in private school\n')
print(ols_coefficients_pvt_wh)
cat('\nOLS regression summary statistics between white and hispanic in non-profit private school\n')
print(ols_summary_nonpft_wh)
cat('\nOLS MSE for white and hispanic in non-profit private school\n')
print(ols_mse_nonpft_wh)
cat('\nOLS regression coefficients between white and hispanic in non-profit private school\n')
print(ols_coefficients_nonpft_wh)
cat('\nOLS regression summary statistics between white and asian in non-profit private school\n')
print(ols_summary_nonpft_wa)
cat('\nOLS MSE between white and asian in non-profit private school\n')
print(ols_mse_nonpft_wa)
cat('\nOLS regression coefficients between white and asian in non-profit private school\n')
print(ols_coefficients_nonpft_wa)
cat('\nOLS regression summary statistics between white and black in non-profit private school\n')
print(ols_summary_nonpft_wb)
cat('\nOLS MSE between white and black in non-profit private school\n')
print(ols_mse_nonpft_wb)
cat('\nOLS regression coefficients between white and black in non-profit private school\n')
print(ols_coefficients_nonpft_wb)
cat('\nOLS regression summary statistics between white and hispanic in private for profit school\n')
print(ols_summary_pft_wh)
cat('\nOLS MSE for white and hispanic in private for profit school\n')
print(ols_mse_pft_wh)
cat('\nOLS regression coefficients between white and hispanic in private for profit school\n')
print(ols_coefficients_pft_wh)
cat('\nOLS regression summary statistics between white and asian in private for profit school\n')
print(ols_summary_pft_wa)
cat('\nOLS MSE between white and asian in private for profit school\n')
print(ols_mse_pft_wa)
cat('\nOLS regression coefficients between white and asian in private for profit school\n')
print(ols_coefficients_pft_wa)
cat('\nOLS regression summary statistics between white and black in private for profit school\n')
print(ols_summary_pft_wb)
cat('\nOLS MSE between white and black in private for profit school\n')
print(ols_mse_pft_wb)
cat('\nOLS regression coefficients between white and asian in private for profit school\n')
print(ols_coefficients_pft_wb)

cat('\nOLS regression summary statistics between high and low\n')
print(ols_summary_hl)
cat('\nOLS MSE between high and low\n')
print(ols_mse_hl)
cat('\nOLS regression coefficients betweenhigh and low\n')
print(ols_coefficients_hl)
cat('\nOLS regression summary statistics between high and middle\n')
print(ols_summary_hm)
cat('\nOLS MSE between high and middle\n')
print(ols_mse_hm)
cat('\nOLS regression coefficients between high and middle\n')
print(ols_coefficients_hm)
cat('\nOLS regression summary statistics between middle and low\n')
print(ols_summary_ml)
cat('\nOLS MSE for middle and low\n')
print(ols_mse_ml)
cat('\nOLS regression coefficients between middle and low\n')
print(ols_coefficients_ml)
cat('\nOLS regression summary statistics between high and low in public school\n')
print(ols_summary_pbc_hl)
cat('\nOLS MSE between high and low in public school\n')
print(ols_mse_pbc_hl)
cat('\nOLS regression coefficients between high and low in public school\n')
print(ols_coefficients_pbc_hl)
cat('\nOLS regression summary statistics between high and middle in public school\n')
print(ols_summary_pbc_hm)
cat('\nOLS MSE between high and middle in public school\n')
print(ols_mse_pbc_hm)
cat('\nOLS regression coefficients between high and middle in public school\n')
print(ols_coefficients_pbc_hm)
cat('\nOLS regression summary statistics between middle and low in private school\n')
print(ols_summary_pvt_ml)
cat('\nOLS MSE for middle and low in private school\n')
print(ols_mse_pvt_ml)
cat('\nOLS regression coefficients between middle and low in private school\n')
print(ols_coefficients_pvt_ml)
cat('\nOLS regression summary statistics between high and low in private school\n')
print(ols_summary_pvt_hl)
cat('\nOLS MSE between high and low in private school\n')
print(ols_mse_pvt_hl)
cat('\nOLS regression coefficients between high and low in private school\n')
print(ols_coefficients_pvt_hl)
cat('\nOLS regression summary statistics between high and middle in private school\n')
print(ols_summary_pvt_hm)
cat('\nOLS MSE between high and middle in private school\n')
print(ols_mse_pvt_hm)
cat('\nOLS regression coefficients between high and middle in private school\n')
print(ols_coefficients_pvt_hm)
cat('\nOLS regression summary statistics between middle and low in private school\n')
print(ols_summary_pvt_ml)
cat('\nOLS MSE for middle and low in private school\n')
print(ols_mse_pvt_ml)
cat('\nOLS regression coefficients between middle and low in private school\n')
print(ols_coefficients_pvt_ml)
cat('\nOLS regression summary statistics between high and low in non-profit private school\n')
print(ols_summary_nonpft_hl)
cat('\nOLS MSE for high and low in non-profit private school\n')
print(ols_mse_nonpft_hl)
cat('\nOLS regression coefficients between high and low in non-profit private school\n')
print(ols_coefficients_nonpft_hl)
cat('\nOLS regression summary statistics between high and middle in non-profit private school\n')
print(ols_summary_nonpft_hm)
cat('\nOLS MSE between high and middle in non-profit private school\n')
print(ols_mse_nonpft_hm)
cat('\nOLS regression coefficients between high and middle in non-profit private school\n')
print(ols_coefficients_nonpft_hm)
cat('\nOLS regression summary statistics between middle and low in non-profit private school\n')
print(ols_summary_nonpft_ml)
cat('\nOLS MSE between middle and low in non-profit private school\n')
print(ols_mse_nonpft_ml)
cat('\nOLS regression coefficients between middle and low in non-profit private school\n')
print(ols_coefficients_nonpft_ml)
cat('\nOLS regression summary statistics between high and low in profit private school\n')
print(ols_summary_pft_hl)
cat('\nOLS MSE for high and low in profit private school\n')
print(ols_mse_pft_hl)
cat('\nOLS regression coefficients between high and low in profit private school\n')
print(ols_coefficients_pft_hl)
cat('\nOLS regression summary statistics between high and middle in private for profit school\n')
print(ols_summary_pft_hm)
cat('\nOLS MSE for high and middle in private for profit school\n')
print(ols_mse_pft_hm)
cat('\nOLS regression coefficients between high and middle in private for profit school\n')
print(ols_coefficients_pft_hm)
cat('\nOLS regression summary statistics between middle and low in private for profit school\n')
print(ols_summary_pft_ml)
cat('\nOLS MSE between middle and low in private for profit school\n')
print(ols_mse_pft_ml)
cat('\nOLS regression coefficients between middle and low in private for profit school\n')
print(ols_coefficients_pft_ml)
sink()     


# Plots and tables

# Earnings Gaps Coefficients
Model <- c('Public: High-Low', 'Non-Profit Private: High-Low', 'For-Profit Private: High-Low', 
           'Public: High-Mid', 'Non-Profit Private: High-Mid', 'For-Profit Private: High-Mid', 
           'Public: Mid-Low', 'Non-Profit Private: Mid-Low', 'For-Profit Private: Mid-Low')

Estimate <- c(ols_summary_pbc_hl$coefficients[2], ols_summary_pvt_hl$coefficients[2], 
              ols_summary_pft_hl$coefficients[2], ols_summary_pbc_hm$coefficients[2], 
              ols_summary_pvt_hm$coefficients[2], ols_summary_pft_hm$coefficients[2],
              ols_summary_pbc_ml$coefficients[2], ols_summary_pvt_ml$coefficients[2], 
              ols_summary_pft_ml$coefficients[2])
SE <- c(ols_summary_pbc_hl$coefficients[4], ols_summary_pvt_hl$coefficients[4], 
        ols_summary_pft_hl$coefficients[4], ols_summary_pbc_hm$coefficients[4], 
        ols_summary_pvt_hm$coefficients[4], ols_summary_pft_hm$coefficients[4],
        ols_summary_pbc_ml$coefficients[4], ols_summary_pvt_ml$coefficients[4], 
        ols_summary_pft_ml$coefficients[4])
p <- c(ols_summary_pbc_hl$coefficients[8], ols_summary_pvt_hl$coefficients[8], 
       ols_summary_pft_hl$coefficients[8], ols_summary_pbc_hm$coefficients[8], 
       ols_summary_pvt_hm$coefficients[8], ols_summary_pft_hm$coefficients[8],
       ols_summary_pbc_ml$coefficients[8], ols_summary_pvt_ml$coefficients[8], 
       ols_summary_pft_ml$coefficients[8])

earnings_gaps_results <- data.frame(Model, Estimate, SE, p)
earnings_gaps_results$Model <- factor(earnings_gaps_results$Model, 
                                      levels = earnings_gaps_results$Model)
earnings_gaps_table <- xtable(earnings_gaps_results, 
                              caption = 'Income Gap Slope comparison', digits = -7)

png("images/earnings_gaps_results.png", width=7,height=3, units="in", res=1200)
ggplot(earnings_gaps_results, aes(x = Model, y = Estimate)) +
  geom_point() + coord_flip() +
  geom_errorbar(aes(x = Model, ymin = Estimate - 2*SE, ymax = Estimate + 2*SE)) +
  labs(title= "Slope Estimates: Earnings Gap ~ INEXPFTE
       2 SE Intervals")
dev.off()

Model <- c('Public: White-Black', 'Non-Profit Private: White-Black', 
           'For-Profit Private: White-Black', 'Public: White-Hispanic', 
           'Non-Profit Private: White-Hispanic', 'For-Profit Private: White-Hispanic', 
           'Public: White-Asian', 'Non-Profit Private: White-Asian', 
           'For-Profit Private: White-Asian')

Estimate <- c(ols_summary_pbc_wb$coefficients[2], ols_summary_pvt_wb$coefficients[2], 
              ols_summary_pft_wb$coefficients[2], ols_summary_pbc_wh$coefficients[2], 
              ols_summary_pvt_wh$coefficients[2], ols_summary_pft_wh$coefficients[2],
              ols_summary_pbc_wa$coefficients[2], ols_summary_pvt_wa$coefficients[2], 
              ols_summary_pft_wa$coefficients[2])
SE <- c(ols_summary_pbc_wb$coefficients[4], ols_summary_pvt_wb$coefficients[4], 
              ols_summary_pft_wb$coefficients[4], ols_summary_pbc_wh$coefficients[4], 
              ols_summary_pvt_wh$coefficients[4], ols_summary_pft_wh$coefficients[4],
              ols_summary_pbc_wa$coefficients[4], ols_summary_pvt_wa$coefficients[4], 
              ols_summary_pft_wa$coefficients[4])
p <- c(ols_summary_pbc_wb$coefficients[8], ols_summary_pvt_wb$coefficients[8], 
        ols_summary_pft_wb$coefficients[8], ols_summary_pbc_wh$coefficients[8], 
        ols_summary_pvt_wh$coefficients[8], ols_summary_pft_wh$coefficients[8],
        ols_summary_pbc_wa$coefficients[8], ols_summary_pvt_wa$coefficients[8], 
        ols_summary_pft_wa$coefficients[8])

completion_gaps_results <- data.frame(Model, Estimate, SE, p)
completion_gaps_results$Model <- factor(completion_gaps_results$Model, 
                                      levels = completion_gaps_results$Model)

png("images/completion_gaps_results.png",  width=7, height=3, units="in", res=1200)
ggplot(completion_gaps_results, aes(x = Model, y = Estimate)) +
  geom_point() + coord_flip() +
  geom_errorbar(aes(x = Model, ymin = Estimate - 2*SE, ymax = Estimate + 2*SE)) +
  labs(title= "Slope Estimates: Completion Gap ~ INEXPFTE
       2 SE Intervals")
dev.off()

save.image("data/second-model.RData")
