####################################################
# Random Forest Functions
###################################################
# Description: Functions for running a random forest on given data.



# Takes the name of the variable as a string and complete dataframe as arguments
# Plots the error as a function of the number of trees
# Returns the random forest output
run_rf <- function(var_string, full) {
  explanatory <- c(var_string, "SCH_DEG", "PREDDEG", "REGION", "LOCALE",
                   "ADM_RATE", "UGDS_WHITE", "UGDS_BLACK", "UGDS_HISP", "UGDS_ASIAN",
                   "COSTT4_A", "MEDIAN_HH_INC", "POVERTY_RATE", "INEXPFTE.x", 
                   "CONTROL.x")
  rf_df <- na.omit(full[,explanatory])
  for (i in 6:14) {
    rf_df[ , i] <- as.numeric(as.character(rf_df[, i]))
  }
  for (i in c(2:5, 15)){
    rf_df[ , i] <- as.factor((rf_df[, i]))
  }
  rf_df <- na.omit(rf_df)
  rf <- randomForest(rf_df[ , var_string] ~ ., data= rf_df[, c(2:15)], ntree=100, mtry=4)
  png(paste("images/rf_trees", var_string, ".png", sep="" ))
  plot(rf)
  dev.off()
  print(rf)
  return(rf)
}