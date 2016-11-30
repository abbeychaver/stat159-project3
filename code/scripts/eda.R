source("code/functions/summary_functions.R")
library(plyr)

school_data <- read.csv("data/school_data.csv")

# Calculate summary statistics for the only qualitative variable, school classification

classification = school_data$CONTROL
classification = revalue(as.factor(classification), c("1" = "Public", "2" = "Private nonprofit", "3" = "Private for-profit"))
qual_stats(classification, "School_Classification")

# Calculate summary statistics for the quantitative variables
# Includes school revenue per student, school expenditure per student, completion rate
# partitioned by ethnicity, median debts partitioned by income, and mean earnings partitioned by income

cols = c(382, 383, 397, 398, 399, 400, 401, 402, 403, 404, 405, 1507, 1508, 1509, 1655, 1656, 1657)
data = school_data[, c(4, 17, cols)]
names = c("Revenue_Per_Student", "Expenditure_Per_Student", "Completion_Rate_White", "Completion_Rate_Black", "Completion_Rate_Hispanic", "Completion_Rate_Asian", "Completion_Rate_American_Indian", "Completion_Rate_Hawaiian_Pacific_Islander", "Completion_Rate_Multiple_Races", "Completion_Rate_Aliens", "Completion_Rate_Unknown_Race", "Median_Debt_Lowest_Income", "Median_Debt_Middle_Income", "Median_Debt_High_Income", "Mean_Earnings_Low_Income", "Mean_Earnings_Middle_Income", "Mean_Earnings_High_Income")
for (i in 1:length(cols)) {
  vec = as.numeric(as.character(school_data[, cols[i]]))
  vec = vec[!is.na(vec)]
  quant_stats(vec, names[i])
  if (i != 1) {
    data[, i] = as.numeric(as.character(data[, i]))
  }
}

# Create new dataframe with relevant columns

#data = school_data[, c(4, 17, cols)]
write.csv(data, file = "data/combined_data.csv")
