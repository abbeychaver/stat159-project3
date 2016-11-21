school_data <- read.csv("data/combined_data.csv")
school_data <- school_data[,-1]

# Converting data to numeric
for (i in 3:19) {
  school_data[, i] = as.numeric(as.character(school_data[, i]))
}

# Gap metrics of well-represented and underrepresented racial groups
school_data$gap_completion_white_black = school_data$C150_4_WHITE-school_data$C150_4_BLACK
school_data$gap_completion_white_hisp = school_data$C150_4_WHITE-school_data$C150_4_HISP
school_data$gap_completion_white_asian = school_data$C150_4_WHITE-school_data$C150_4_ASIAN
school_data$gap_completion_white_aian = school_data$C150_4_WHITE-school_data$C150_4_AIAN
school_data$gap_completion_white_nhpi = school_data$C150_4_WHITE-school_data$C150_4_NHPI
school_data$gap_completion_white_2mor = school_data$C150_4_WHITE-school_data$C150_4_2MOR
school_data$gap_completion_white_nra = school_data$C150_4_WHITE-school_data$C150_4_NRA
school_data$gap_completion_white_unkn = school_data$C150_4_WHITE-school_data$C150_4_UNKN

# Gap metrics of well-represented and underrepresented income groups
school_data$gap_earnings_high_low = (school_data$MN_EARN_WNE_INC3_P10-school_data$MN_EARN_WNE_INC1_P10)/school_data$MN_EARN_WNE_INC3_P10
school_data$gap_earnings_high_mid = (school_data$MN_EARN_WNE_INC3_P10-school_data$MN_EARN_WNE_INC2_P10)/school_data$MN_EARN_WNE_INC3_P10

write.csv(school_data, file = "data/gap_metrics_data.csv")
