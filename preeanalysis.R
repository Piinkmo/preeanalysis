
# Clear workspace and free memory.
rm(list = ls())
gc()
# Set working directory
setwd("/users/yutingjia/desktop/preeclampsia")

# Load library
library(haven) # To read .XPT files
library(tidyverse)
library(ggplot2)



#Load data
demo <- read_xpt("dataset/P_DEMO.XPT") # RIDRETH1, RIAGENDR, RIDAGEYR, WTINTPRP
# 1	Mexican American	
# 2	Other Hispanic	
# 3	Non-Hispanic White	
# 4	Non-Hispanic Black	
# 5	Other Race - Including Multi-Racial
bm <- read_xpt("dataset/P_BMX.XPT") # BMXBMI
diabetes <- read_xpt("dataset/P_DIQ.XPT") # DIQ010
glucose <- read_xpt("dataset/P_GLU.XPT") # LBXGLU
insulin <- read_xpt("dataset/P_INS.XPT") # LBXIN

# Join datasets using SEQN as the key
data <- demo %>%
  left_join(bm, by = "SEQN") %>%
  left_join(diabetes, by = "SEQN") %>%
  left_join(glucose, by = "SEQN") %>%
  left_join(insulin, by = "SEQN")
head(data)

filtered_data <- data %>%
  dplyr::filter(RIAGENDR == 2,               # Female
                BMXBMI > 30,                 # BMI > 30
                RIDAGEYR >= 18 & RIDAGEYR < 65) %>%  # Age between 18 and 65
  dplyr::select(RIDRETH1, RIAGENDR, BMXBMI, RIDAGEYR, WTINTPRP, DIQ010, LBXGLU, LBXIN)


#########################################

# Assuming combined_data has already been filtered and contains the relevant columns
obesityrela <- data %>%
  mutate(obese = ifelse(BMXBMI >= 30, 1, 0), 
         RIDRETH1_combined = case_when(
           RIDRETH1 == 1 | RIDRETH1 == 2 ~ "Combined Mexican-American & Hispanic", 
           RIDRETH1 == 3 ~ "Non-Hispanic White",               # Keep category 3
           RIDRETH1 == 4 ~ "Non-Hispanic Black",                # Keep category 4# Combine race categories 1 and 2
           TRUE ~ as.character(RIDRETH1)                      # Other categories remain unchanged
         ))  # 1 for obese, 0 for not obese

obesity_data <- obesityrela %>%
  group_by(RIDRETH1_combined) %>%
  summarise(obesity_rate = mean(obese, na.rm = TRUE) * 100)  # Convert to percentage

ggplot(obesity_data, aes(x = factor(RIDRETH1_combined), y = obesity_rate)) +
  geom_boxplot() +
  labs(x = "Race Category", y = "Percentage of Obesity", 
       title = "Percentage of Obesity by Race Category") +
  theme_minimal()


####
calculate_weighted_stats <- function(data, weight_col, value_col) {
  
  # Extract weights and values
  weights <- data[[weight_col]]
  values <- data[[value_col]]
  
  # Calculate weighted mean
  weighted_mean <- sum(weights * values, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  
  # Calculate weighted variance
  weighted_variance <- sum(weights * (values - weighted_mean)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  
  # Calculate standard error from weighted variance
  standard_error <- sqrt(weighted_variance / length(values[!is.na(values)]))
  
  return(list(weighted_mean = weighted_mean, 
              weighted_variance = weighted_variance, 
              standard_error = standard_error))
}

diabetes_data <- filtered_data %>%
  dplyr::group_by(RIDRETH1_combined) %>%
  dplyr::summarise(
    weighted_mean_diabetes = calculate_weighted_stats(cur_data(), "WTINTPRP", "diabetes_status")$weighted_mean * 100,  # Convert to percentage
    se_diabetes = calculate_weighted_stats(cur_data(), "WTINTPRP", "diabetes_status")$standard_error * 100  # Convert SE to percentage
  )

# Create a bar plot for weighted diabetes percentages with error bars
ggplot(diabetes_data, aes(x = RIDRETH1_combined, y = weighted_mean_diabetes)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  geom_errorbar(aes(ymin = weighted_mean_diabetes - se_diabetes, ymax = weighted_mean_diabetes + se_diabetes), width = 0.2) +
  labs(x = "Race Category (Combined)", y = "Weighted Percentage of Diabetes", 
       title = "Weighted Percentage of Diabetes by Combined Race Category with Standard Errors") +
  theme_minimal()
