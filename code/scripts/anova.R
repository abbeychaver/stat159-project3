###################################################
# ANOVA Analysis
###################################################
# Testing the hypothesis that there is no difference in outcome gaps
# between different types of schools

# loading datasets
cr_w_b <- read.csv("data/Completion_W_B.csv")
cr_w_h <- read.csv("data/Completion_W_H.csv")
cr_w_a <- read.csv("data/Completion_W_A.csv")
income_rates <- read.csv("data/Income.csv")
cr_w_b$CONTROL <- as.factor(cr_w_b$CONTROL)
cr_w_a$CONTROL <- as.factor(cr_w_a$CONTROL)
cr_w_h$CONTROL <- as.factor(cr_w_h$CONTROL)
income_rates$CONTROL <- as.factor(income_rates$CONTROL)

# Creating boxplots of data
png('images/completion_rate_boxplot_white_black.png')
boxplot(cr_w_b$gap_completion_white_black ~ cr_w_b$CONTROL, outline=FALSE, xlab= "Classification of Institution", ylab= "Gap Metric", xaxt = "n", main= "Completion Rate Gap Between White and Black Students")
axis(1, at=1:3, labels=c("Public", "Private Non-Profit", "Private For-Profit"))
dev.off()

# Anova Analysis For Racial Groups
aov_cr_wb <- aov(cr_w_b$gap_completion_white_black ~ cr_w_b$CONTROL)
sum_aov_cr_wb <- summary(aov_cr_wb)
tukey_cr_wb <- TukeyHSD(aov_cr_wb, main="White-Black Completion Rate Differences")

aov_cr_wa <- aov(cr_w_a$gap_completion_white_asian ~ cr_w_a$CONTROL)
sum_aov_cr_wa <- summary(aov_cr_wa)
tukey_cr_wa <- TukeyHSD(aov_cr_wa, main="White-Asian Completion Rate Differences")

aov_cr_wh <- aov(cr_w_h$gap_completion_white_hispanic ~ cr_w_h$CONTROL)
sum_aov_cr_wh <- summary(aov_cr_wh)
tukey_cr_wh <- TukeyHSD(aov_cr_wh, main="White-Hispanic Completion Rate Differences")

# Anova Analysis For Income Groups
aov_e_hl <- aov(income_rates$gap_earnings_high_low ~ income_rates$CONTROL)
sum_aov_e_hl <- summary(aov_e_hl)
tukey_e_hl <- TukeyHSD(aov_e_hl, main="High-Low Income Earnings Differences")

aov_e_hm <- aov(income_rates$gap_earnings_high_mid ~ income_rates$CONTROL)
sum_aov_e_hm <- summary(aov_e_hm)
tukey_e_hm <- TukeyHSD(aov_e_hm, main="High-Mid Income Earnings Differences")

aov_e_ml <- aov(income_rates$gap_earnings_mid_low ~ income_rates$CONTROL)
sum_aov_e_ml <- summary(aov_e_ml)
tukey_e_ml <- TukeyHSD(aov_e_ml, main="Mid-Low Income Earnings Differences")

# Creating Tukey Plots
png('images/completion_rate_tukeyplot_white_black.png', 
    width=6,height=4, units="in", res=1200)
par(pin = c(3, 3), mai=c(1, 2.5, 1, 0.5))
plot(tukey_cr_wb, yaxt="n")
axis(2, at=1:3, las=2,cex.axis=0.8, 
     labels=c("For-Profit : Private Non-Profit", 
              "Private For-Profit : Public", 
              "Private Non-Profit : Public"))
dev.off()

png('images/completion_rate_tukeyplot_white_asian.png', 
    width=6,height=4, units="in", res=1200)
par(pin = c(3, 3), mai=c(1, 2.5, 1, 0.5))
plot(tukey_cr_wa, yaxt="n")
axis(2, at=1:3, las=2,cex.axis=0.6, 
     labels=c("Private For-Profit : Private Non-Profit", 
              "Private For-Profit : Public", 
              "Private Non-Profit : Public"))
dev.off()

png('images/completion_rate_tukeyplot_white_hispanics.png', 
    width=6,height=4, units="in", res=1200)
par(pin = c(3, 3), mai=c(1, 2.5, 1, 0.5))
plot(tukey_cr_wh, yaxt="n")
axis(2, at=1:3, las=2,cex.axis=0.8, 
     labels=c("Private For-Profit : Private Non-Profit", 
              "Private For-Profit : Public", 
              "Private Non-Profit : Public"))
dev.off()

png('images/earnings_tukeyplot_high_low.png', 
    width=6,height=4, units="in", res=1200)
par(pin = c(3, 3), mai=c(1, 2.5, 1, 0.5))
plot(tukey_e_hl,yaxt="n")
axis(2, at=1:3, las=2,cex.axis=0.8, 
     labels=c("Private For-Profit : Private Non-Profit", 
              "Private For-Profit : Public", 
              "Private Non-Profit : Public"))
dev.off()

png('images/earnings_tukeyplot_high_mid.png', width=6,height=4, units="in", res=1200)
par(pin = c(3, 3), mai=c(1, 2.5, 1, 0.5))
plot(tukey_e_hm,yaxt="n")
axis(2, at=1:3, las=2,cex.axis=0.8, 
     labels=c("Private For-Profit : Private Non-Profit", 
              "Private For-Profit : Public", 
              "Private Non-Profit : Public"))
dev.off()

png('images/earnings_tukeyplot_mid_low.png',
    width=6,height=4, units="in", res=1200)
par(pin = c(3, 3), mai=c(1, 2.5, 1, 0.5))
plot(tukey_e_ml,yaxt="n")
axis(2, at=1:3, las=2,cex.axis=0.8, 
     labels=c("Private For-Profit : Private Non-Profit", 
              "Private For-Profit : Public", 
              "Private Non-Profit : Public"))
dev.off()

# Saving output to RData file
save(aov_cr_wb, sum_aov_cr_wb, tukey_cr_wb, aov_cr_wa, 
     sum_aov_cr_wa, tukey_cr_wa, aov_cr_wh, sum_aov_cr_wh, 
     tukey_cr_wh, aov_e_hl, sum_aov_e_hl, tukey_e_hl, aov_e_hm, 
     sum_aov_e_hm, tukey_e_hm, aov_e_ml, sum_aov_e_ml, 
     tukey_e_ml, file = "data/anova.RData")

# Sinking output to txt file
sink('data/anova.txt')
cat('\nAnova Summary of Completion Rate of White and Black Students\n')
print(sum_aov_cr_wb)
cat('\nTukeyHSD of Completion Rate of White and Black Students\n')
print(tukey_cr_wb)
cat('\nAnova Summary of Completion Rate of White and Asian Students\n')
print(sum_aov_cr_wa)
cat('\nTukeyHSD of Completion Rate of White and Asian Students\n')
print(tukey_cr_wa)
cat('\nAnova Summary of Completion Rate of White and Hispanic Students\n')
print(sum_aov_cr_wh)
cat('\nTukeyHSD of Completion Rate of White and Hispanic Students\n')
print(tukey_cr_wh)
cat('\nAnova Summary of Earnings of High and Low Income Groups\n')
print(sum_aov_e_hl)
cat('\nTukeyHSD of Earnings of High and Low Income Groups\n')
print(tukey_e_hl)
cat('\nAnova Summary of Earnings of High and Mid Income Groups\n')
print(sum_aov_e_hm)
cat('\nTukeyHSD of Earnings of High and Mid Income Groups\n')
print(tukey_e_hm)
cat('\nAnova Summary of Earnings of Mid and Low Income Groups\n')
print(sum_aov_e_ml)
cat('\nTukeyHSD of Earnings of Mid and Low Income Groups\n')
print(tukey_e_ml)
sink()