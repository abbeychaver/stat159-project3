school_data <- read.csv("data/combined_data.csv")
school_data <- school_data[,-1]

# Converting data to numeric
for (i in 3:19) {
  school_data[, i] = as.numeric(as.character(school_data[, i]))
}

# Replacing missing values with column mean
school_data$TUITFTE[is.na(school_data$TUITFTE)] <- round(mean(school_data$TUITFTE, na.rm = TRUE))
school_data$INEXPFTE[is.na(school_data$INEXPFTE)] <- round(mean(school_data$INEXPFTE, na.rm = TRUE))
school_data$C150_4_WHITE[is.na(school_data$C150_4_WHITE)] <- round(mean(school_data$C150_4_WHITE, na.rm = TRUE))
school_data$C150_4_BLACK[is.na(school_data$C150_4_BLACK)] <- round(mean(school_data$C150_4_BLACK, na.rm = TRUE))
school_data$C150_4_HISP[is.na(school_data$C150_4_HISP)] <- round(mean(school_data$C150_4_HISP, na.rm = TRUE))
school_data$C150_4_ASIAN[is.na(school_data$C150_4_ASIAN)] <- round(mean(school_data$C150_4_ASIAN, na.rm = TRUE))
school_data$C150_4_AIAN[is.na(school_data$C150_4_AIAN)] <- round(mean(school_data$C150_4_AIAN, na.rm = TRUE))
school_data$C150_4_NHPI[is.na(school_data$C150_4_NHPI)] <- round(mean(school_data$C150_4_NHPI, na.rm = TRUE))
school_data$C150_4_2MOR[is.na(school_data$C150_4_2MOR)] <- round(mean(school_data$C150_4_2MOR, na.rm = TRUE))
school_data$C150_4_NRA[is.na(school_data$C150_4_NRA)] <- round(mean(school_data$C150_4_NRA, na.rm = TRUE))
school_data$C150_4_UNKN[is.na(school_data$C150_4_UNKN)] <- round(mean(school_data$C150_4_UNKN, na.rm = TRUE))
school_data$LO_INC_DEBT_MDN[is.na(school_data$LO_INC_DEBT_MDN)] <- round(mean(school_data$LO_INC_DEBT_MDN, na.rm = TRUE))
school_data$MD_INC_DEBT_MDN[is.na(school_data$MD_INC_DEBT_MDN)] <- round(mean(school_data$MD_INC_DEBT_MDN, na.rm = TRUE))
school_data$HI_INC_DEBT_MDN[is.na(school_data$HI_INC_DEBT_MDN)] <- round(mean(school_data$HI_INC_DEBT_MDN, na.rm = TRUE))
school_data$MN_EARN_WNE_INC1_P10[is.na(school_data$MN_EARN_WNE_INC1_P10)] <- round(mean(school_data$MN_EARN_WNE_INC1_P10, na.rm = TRUE))
school_data$MN_EARN_WNE_INC2_P10[is.na(school_data$MN_EARN_WNE_INC2_P10)] <- round(mean(school_data$MN_EARN_WNE_INC2_P10, na.rm = TRUE))
school_data$MN_EARN_WNE_INC3_P10[is.na(school_data$MN_EARN_WNE_INC3_P10)] <- round(mean(school_data$MN_EARN_WNE_INC3_P10, na.rm = TRUE))

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
