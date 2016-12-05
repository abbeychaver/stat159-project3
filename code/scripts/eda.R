#################################################
# Exploratory Data Analysis
#################################################
# Calculating summary statistics and plots of variables of interest
# Plotting the relationships between expenditure, outcome gaps, and school type

source("code/functions/summary_functions.R")
source("code/functions/eda_functions.R")
library(plyr)

school_data <- read.csv("data/school_data.csv")

# Calculate summary statistics for the only qualitative variable, school classification

classification = school_data$CONTROL
classification = revalue(as.factor(classification), c("1" = "Public", "2" = "Private nonprofit", "3" = "Private for-profit"))
qual_stats(classification, "School_Classification")

# Calculate summary statistics for the quantitative variables
# Includes school revenue per student, school expenditure per student, completion rate
# partitioned by ethnicity, median debts partitioned by income, and mean earnings partitioned by income

cols = c(382, 383, 397, 398, 399, 400, 401, 402, 403, 404, 405, 
         1507, 1508, 1509, 1655, 1656, 1657)
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

combined_data = school_data[, c(4, 17, cols)]
write.csv(combined_data, file = "data/combined_data.csv")


####################################
# Exploring Relationships
####################################

completion_W_A <- read.csv("data/Completion_W_A.csv")
completion_W_B <- read.csv("data/Completion_W_B.csv")
completion_W_H <- na.omit(read.csv("data/Completion_W_H.csv"))
income_gap <- read.csv("data/Income.csv")

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

control.code <- c("Public" = 1, "Non-Profit Private"=2, "For-Profit Private"=3)
df$TYPE <- names(control.code)[match(df$CONTROL, control.code)]
df$INEXPFTE <- as.numeric(as.character(df$INEXPFTE))

CSU_names <- c("California State University-Bakersfield",
               "California State University-Stanislaus",
               "California State University-San Bernardino",
               "California State Polytechnic University-Pomona",
               "California State University-Chico",
               "California State University-Dominguez Hills",
               "California State University-Fresno",
               "California State University-Fullerton",
               "California State University-East Bay",
               "California State University-Long Beach",
               "California State University-Los Angeles",
               "California State University-Northridge",
               "California State University-Sacramento")

CSU <- df[df$INSTNM %in% CSU_names, ]
CSU$cohort = "CSU"

UC_names <- c("University of California-Berkeley",
              "University of California-Davis",
              "University of California-Irvine",
              "University of California-Los Angeles",
              "University of California-Riverside",
              "University of California-San Diego",
              "University of California-San Francisco",
              "University of California-Santa Barbara",
              "University of California-Santa Cruz")

UC <- df[df$INSTNM %in% UC_names, ]
UC$cohort = "UC"

elite_private_names <- c("Harvard University", "Yale University", 
                         "Princeton University", "Brown University", 
                         "Cornell University", "Duke University", 
                         "University of Pennsylvania", "Dartmouth University", 
                         "Stanford University",
                         "Columbia University in the City of New York",
                         "Massachusetts Institute of Technology",
                         "California Institute of Technology")
elite <- df[df$INSTNM %in% elite_private_names,]
elite$cohort = "Elite Private"

INEXPFTE <- as.numeric(as.character(combined_data$INEXPFTE))

high_exp_names <- head(combined_data[
  sort.list(INEXPFTE, decreasing = TRUE), ]$INSTNM,
  n = 40)

high_exp<- df[df$INSTNM %in% high_exp_names, ]
high_exp$cohort = "Highest Expenditure"

cohort_comparison <- rbind(CSU, UC, elite, high_exp)

# Scatter plots of these metrics
gap_metrics = c("gap_completion_white_black", "gap_completion_white_hispanic",
                "gap_completion_white_asian",  "gap_earnings_high_low",
                "gap_earnings_high_mid", "gap_earnings_mid_low")
for (g in gap_metrics) {
  cohort_plot(g, cohort_comparison)
  all_plot(g, df)
  zoom_plot(g, df)
}