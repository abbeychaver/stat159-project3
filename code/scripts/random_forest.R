######################################################
# Random Forest
######################################################
# We use random forest as a method to evaluate relative importance
# of other possible predictors for outcome gaps.

# Load Libraries
library(dplyr)
library(ggplot2)
library(randomForest)
library(miscTools)

# Load Data
school_data <- read.csv("data/school_data.csv")
combined_data <- read.csv("data/combined_data.csv")
completion_W_A <- read.csv("data/Completion_W_A.csv")
completion_W_B <- read.csv("data/Completion_W_B.csv")
completion_W_H <- na.omit(read.csv("data/Completion_W_H.csv"))
income_gap <- read.csv("data/Income.csv")

# Predictors we are interested in
predictors <- school_data[, c("INSTNM", "SCH_DEG", "PREDDEG", "REGION", "LOCALE",
                      "ADM_RATE", "UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN",
                      "COSTT4_A", "MEDIAN_HH_INC", "POVERTY_RATE", "INEXPFTE", 
                      "CONTROL")]

# Create one merged dataframe for ease in graphing
df <- merge(completion_W_B[ , c("X", "INSTNM", "gap_completion_white_black")], 
            completion_W_H[ , c("X", "gap_completion_white_hispanic")], 
            by="X",  all= TRUE)
df <- merge(df, completion_W_A[, c("X", "gap_completion_white_asian")], 
            by="X",  all.x = TRUE)
df <- merge(df, income_gap[ , c("X", "gap_earnings_high_low",
                                "gap_earnings_high_mid", "gap_earnings_mid_low")], 
            by="X",  all = TRUE)
df <- merge(df, combined_data[, c("INSTNM", "INEXPFTE", "CONTROL")], 
            by = "INSTNM")
df$X <- as.numeric(df$X)

# Set labels for CONTROL for ease in graphing
control.code <- c("Public" = 1, "Non-Profit Private"=2, "For-Profit Private"=3)
df$TYPE <- names(control.code)[match(df$CONTROL, control.code)]


full <- merge(df, predictors, by = "INSTNM", all = TRUE)

# RF

source("code/functions/rf_functions.R")
gap_metrics = c("gap_completion_white_black", "gap_completion_white_hispanic",
                "gap_completion_white_asian",  "gap_earnings_high_low",
                "gap_earnings_high_mid", "gap_earnings_mid_low")
set.seed(123)

# Save RFs for all 6 metrics
rfs <- list()
for (g in gap_metrics) {
  rf <- run_rf(g, full)
  rfs <- c(rfs, list(rf))
}

# Compare Importance across metrics
temp_matrix <- as.matrix(rfs[[1]]$importance)
importance_matrix <- data.frame("Variable" = rownames(temp_matrix), 
                                "Node_Purity_Increase" = unname(temp_matrix),
                         "Gap" = rep(gap_metrics[1], 14))
for (i in 2:6) {
  temp_matrix <- as.matrix(rfs[[i]]$importance)
  new_matrix <- data.frame("Variable" = rownames(temp_matrix), 
                           "Node_Purity_Increase"= unname(temp_matrix),
                           "Gap" = rep(gap_metrics[i], 14))
  importance_matrix <- rbind(importance_matrix, new_matrix)
}

png("images/rf_importance_completion.png",width=7,height=3, units="in", res=1200)
ggplot(importance_matrix[1:42, ], aes(x = Variable, y = Node_Purity_Increase)) +
  geom_bar(aes(fill = Gap), position = "dodge", stat="identity") + coord_flip() +
  labs(ylab("Node Purity Increase")) + labs(xlab("Gap Metrics")) +
  ggtitle("Variable Importance: Completion Rate Random Forests")
dev.off()

png("images/rf_importance_earnings.png", width=7,height=3, units="in", res=1200)
ggplot(importance_matrix[43:84,], aes(x = Variable, y = Node_Purity_Increase)) +
  geom_bar(aes(fill = Gap), position = "dodge", stat="identity") + coord_flip() +
  labs(ylab("Node Purity Increase")) + labs(xlab("Gap Metrics")) +
  ggtitle("Variable Importance: Earnings Random Forests")
dev.off()


# Compute and plot performance statistics
var_explained <- c()
for (i in 1:6) {
  var_explained[i] <- rfs[[i]]$rsq[100]*100
}
var_explained <- data.frame(var_explained, gap_metrics)

png("images/rf_performance.png", width=7,height=3, units="in", res=1200)
ggplot(var_explained, aes(x = gap_metrics, y= var_explained)) +
  geom_bar(stat="identity") + coord_flip() +
  labs(ylab("Percent Variance Explained")) + labs(xlab("Gap Metrics")) +
  ggtitle("Random Forest Performance by Gap Metric")
dev.off()

