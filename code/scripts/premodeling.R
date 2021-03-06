###################################################
# Premodeling Script
###################################################
# Computes gap metrics from raw data and writes full datasets to csv

# Create new dataframe with relevant columns

data <- read.csv("data/school_data.csv")
cols = c(382, 383, 397, 398, 399, 400, 401, 402, 403, 404, 405, 1507, 
         1508, 1509, 1655, 1656, 1657)
combined_data = data[, c(4, 17, cols)]
write.csv(combined_data, file = "data/combined_data.csv")

school_data <- read.csv("data/combined_data.csv")
school_data <- school_data[,-1]


# Converting data to numeric
for (i in 3:19) {
  school_data[, i] = as.numeric(as.character(school_data[, i]))
}

# Separate completion rates by race to remove rows with NA values
cr_w_b <- school_data[, 1:6]
cr_w_b <- na.omit(cr_w_b)
cr_w_b$gap_completion_white_black = (cr_w_b$C150_4_WHITE-cr_w_b$C150_4_BLACK)/cr_w_b$C150_4_WHITE
cr_w_b2 <- cr_w_b[cr_w_b$C150_4_WHITE != 0, ]
cr_w_b2 <- cr_w_b2[cr_w_b2$C150_4_BLACK != 0, ]
# Two historically black universities have 0% completion rates for students, remove these
cr_w_b <- cr_w_b2[is.finite(cr_w_b$gap_completion_white_black),]
write.csv(cr_w_b, file="data/Completion_W_B.csv")

cr_w_h <- school_data[, c(1:5,7)]
cr_w_h <- na.omit(cr_w_h)
cr_w_h$gap_completion_white_hispanic = (cr_w_h$C150_4_WHITE-cr_w_h$C150_4_HISP)/cr_w_h$C150_4_WHITE
cr_w_h1 <- cr_w_h[is.finite(cr_w_h$gap_completion_white_hispanic),]
# Seems like some NA completion rates are reported as 0
# Try removing these
cr_w_h2 <- cr_w_h[cr_w_h$C150_4_WHITE != 0, ]
cr_w_h2 <- cr_w_h2[cr_w_h2$C150_4_HISP != 0, ]
cr_w_h2 <- cr_w_h2[is.finite(cr_w_h$gap_completion_white_hispanic),]
write.csv(cr_w_h2, file="data/Completion_W_H.csv")

cr_w_a <- school_data[, c(1:5, 8)]
cr_w_a <- na.omit(cr_w_a)
cr_w_a$gap_completion_white_asian = (cr_w_a$C150_4_WHITE-cr_w_a$C150_4_ASIAN)/cr_w_a$C150_4_WHITE
cr_w_a2 <- cr_w_a[cr_w_a$C150_4_WHITE != 0, ]
cr_w_a2 <- cr_w_a2[cr_w_a2$C150_4_ASIAN != 0, ]
cr_w_a2 <- cr_w_a2[is.finite(cr_w_a$gap_completion_white_asian),]
write.csv(cr_w_a2, file="data/Completion_W_A.csv")

# Separate incomes rates out to remove rows with NA values
income_rates <- school_data[, c(1:4, 17:19)]
income_rates <-na.omit(income_rates)
income_rates$gap_earnings_high_low = (income_rates$MN_EARN_WNE_INC3_P10-income_rates$MN_EARN_WNE_INC1_P10)/income_rates$MN_EARN_WNE_INC3_P10
income_rates$gap_earnings_high_mid = (income_rates$MN_EARN_WNE_INC3_P10-income_rates$MN_EARN_WNE_INC2_P10)/income_rates$MN_EARN_WNE_INC3_P10
income_rates$gap_earnings_mid_low = (income_rates$MN_EARN_WNE_INC2_P10-income_rates$MN_EARN_WNE_INC1_P10)/income_rates$MN_EARN_WNE_INC2_P10

write.csv(income_rates, file="data/Income.csv")
