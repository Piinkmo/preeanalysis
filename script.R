
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
diabetes <- read_xpt("dataset/P_DIQ.XPT") # DIQ010: diabetes
glucose <- read_xpt("dataset/P_GLU.XPT") # LBXGLU: fasting plasma glucose(mg/dl)
insulin <- read_xpt("dataset/P_INS.XPT") # LBXIN: fasting plasma insulin(μU/dl)

# Join datasets using SEQN as the key
data <- demo %>%
  left_join(bm, by = "SEQN") %>%
  left_join(diabetes, by = "SEQN") %>%
  left_join(glucose, by = "SEQN") %>%
  left_join(insulin, by = "SEQN")
head(data)

fwtage <- data %>%
  dplyr::filter(RIAGENDR == 2,               # Female
                RIDAGEYR >= 18 & RIDAGEYR < 65) %>%  # Age between 18 and 65 , DIQ010 == 1
  dplyr::select(RIDRETH1, RIAGENDR, BMXBMI, RIDAGEYR, WTINTPRP, DIQ010, LBXGLU, LBXIN)


############################################################
############################################################
############################################################
# Create obesity column and combine race categories
obesityrela <- fwtage %>%
  mutate(obese = ifelse(BMXBMI >= 30, 1, 0), 
         RIDRETH1_combined = case_when(
           RIDRETH1 %in% c(1, 2) ~ "Mexican American & Other Hispanic", 
           RIDRETH1 == 3 ~ "Non-Hispanic White",               
           RIDRETH1 == 4 ~ "Non-Hispanic Black", 
           RIDRETH1 == 5 ~ "Other Race"
         ))

# Calculate weighted obesity percentage by race category
obesity_data <- obesityrela %>%
  group_by(RIDRETH1_combined) %>%
  summarise(obesity_rate = sum(obese * WTINTPRP, na.rm = TRUE) / sum(WTINTPRP, na.rm = TRUE) * 100)

############################################################
############################################################
############################################################
# Create bar plot to display weighted obesity percentages by race
ggplot(obesity_data, aes(x = factor(RIDRETH1_combined), y = obesity_rate, fill = RIDRETH1_combined)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(obesity_rate, 1)), 
            vjust = -0.5,  # Position the labels just above the bars
            size = 5) +    # Adjust text size
  labs(x = "Race Category", y = "Percent", 
       title = "Weighted Percentage of Obesity by Race (Females, Age 18-65)") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),    # Remove x-axis title if desired
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # Adjust x-axis labels
    legend.position = "none",          # Remove legend
    plot.title = element_text(hjust = 0.5, size = 14),    # Center title
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    plot.background = element_rect(fill = "lightcyan1", color = NA)
  )

############################################################
############################################################
############################################################
# Create Box plot
ggplot(obesityrela, aes(x = factor(RIDRETH1_combined), y = BMXBMI, fill = RIDRETH1_combined)) +
  geom_boxplot() +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20) # custom breaks
  ) + 
  labs(x = "Race", y = "Percent", 
       title = "Weighted Percentage of Obesity by Race (Females, Age 18-65)") +
  geom_text(data = obesity_data, aes(x = RIDRETH1_combined, y = 40, label = paste0(round(obesity_rate, 1), "%")),
            color = "black", size = 5, vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.background = element_rect(fill = "lightcyan1", color = NA)
        #panel.grid.major = element_blank(),  # Remove major grid lines
        #panel.grid.minor = element_blank()
  )

# Create a function to calculate weighted variance
weighted_variance <- function(obese, weight) {
  weighted_mean <- sum(obese * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  variance <- sum((weight^2) * weighted_mean - weighted_mean^2, na.rm = TRUE) / sum(weight, na.rm = TRUE)^2
  return(variance)
}

weighted_sd <- function(obese, weight) {
  variance <- weighted_variance(obese, weight)
  return(sqrt(variance))
}

# Calculate weighted obesity percentage and standard deviation by race category
obesity_data <- obesityrela %>%
  group_by(RIDRETH1_combined) %>%
  summarise(
    obesity_rate = sum(obese * WTINTPRP, na.rm = TRUE) / sum(WTINTPRP, na.rm = TRUE) * 100,
    obesity_sd = weighted_sd(obese, WTINTPRP) * 100  
  )

############################################################
############################################################
############################################################
# Desity Plot of IR for each of race within: Female & Age (18-65)

obesityrelaWithIR <- obesityrela %>%
  mutate(IR = (LBXGLU * LBXIN) / 405)

obesityDiabetes <- obesityrelaWithIR %>%
  mutate(HavingDiabetes = ifelse(DIQ010 == 1, "Diabetes", "Non-Diabetes"))

ggplot(data = obesityDiabetes, aes(x = IR, fill = RIDRETH1_combined))+
  geom_density(alpha = 0.5, adjust = 1.2) +
  facet_wrap(~ HavingDiabetes, scales = "free_y") +
  scale_x_continuous(limits = c(0, 20)) +  
  labs(title = "Density Plot of Insulin Resistance (IR) by Race for Females (Age 18-65)",
       x = "HOMA-IR",
       y = "Density",
       fill = "Race Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    legend.position = "right",  
    axis.text = element_text(size = 10)  
  )


## HOMA-IR Density Plot Comment:

# This plot shows the insulin resistance levels (HOMA-IR) for females aged 18-65 across different racial groups. Insulin resistance means the body is less responsive to insulin, which can lead to type 2 diabetes and other metabolic issues.
# 
# ### What does the plot tell us?
# 
# Most people have low insulin resistance:
#   
#   The curves for all racial groups peak between 0 and 3, indicating that most women have low insulin resistance and their bodies respond well to insulin.
# A small number of people have higher insulin resistance:
#   
#   There’s a long tail on the right side of the plot, showing that a few women in each racial group have much higher insulin resistance, meaning they might be at a higher risk for diabetes. However, these people are in the minority.
# 
# No major differences between racial groups:
#   The curves for different racial groups (represented by different colors) are almost overlapping. This suggests that there are no significant differences in insulin resistance levels between the races. Women from all racial groups show similar patterns in insulin resistance.
# 
# In summary:
#   Most women have healthy insulin levels, with only a small percentage having higher insulin resistance and potentially a higher risk of metabolic issues.
# Race doesn't seem to play a major role in determining insulin resistance, as the distributions for all groups are very similar.
# 
# To put it simply: Most women have normal insulin resistance levels, and there isn’t much difference between the races. Only a small group has higher resistance and might need more attention.
