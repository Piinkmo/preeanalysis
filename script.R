---
  title: "Preeclampsia Analysis"
author: "Yuting Jia"
date: "2024-09-04"
output:
  html_document:
  latex_engine: xelatex
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```




# Clear workspace and free memory.
rm(list = ls())
gc()
# Set working directory
setwd("/users/yutingjia/desktop/preeclampsia")

# Load library
library(haven) # To read .XPT files
library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)
library(tidyr)
library(kableExtra)

```


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
bp <- read_xpt("dataset/P_BPQ.XPT")
dr1 <- read_xpt("dataset/P_DR1TOT.XPT")
dr2 <- read_xpt("dataset/P_DR2TOT.XPT")

# Join datasets using SEQN as the key
data <- demo %>%
  left_join(bm, by = "SEQN") %>%
  left_join(diabetes, by = "SEQN") %>%
  left_join(glucose, by = "SEQN") %>%
  left_join(insulin, by = "SEQN") %>%
  left_join(bp, by = "SEQN")%>%
  left_join(dr1, by = "SEQN")%>%
  left_join(dr2, by = "SEQN")

fwtage <- data %>%
  dplyr::filter(RIAGENDR == 2,               # Female
                RIDAGEYR >= 18 & RIDAGEYR < 65) %>%  # Age between 18 and 65 , DIQ010 == 1
  dplyr::select(SEQN, RIDRETH1, RIAGENDR, BMXBMI, RIDAGEYR, WTINTPRP, DIQ010, LBXGLU, LBXIN, BPQ030,DR1TSODI,DR1TPOTA,DR1TVD,DR2TSODI,DR2TPOTA,DR2TVD)

# Recode race variable (RIDRETH1)
data_filtered <- fwtage %>%
  mutate(Race = case_when(
    RIDRETH1 == 1 ~ "Mexican American",
    RIDRETH1 == 2 ~ "Other Hispanic",
    RIDRETH1 == 3 ~ "Non-Hispanic White",
    RIDRETH1 == 4 ~ "Non-Hispanic Black",
    RIDRETH1 == 5 ~ "Other Race"
  ))%>%
  filter(!RIDRETH1 %in% c(4, 5))

weighted <- function(obese, weight) {
  weighted_mean <- sum(obese * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  return(weighted_mean)
}

```


nutrition <- data_filtered %>%
  mutate(
    Sodium = rowMeans(select(., DR1TSODI, DR2TSODI), na.rm = TRUE),
    Potassium = rowMeans(select(., DR1TPOTA, DR2TPOTA), na.rm = TRUE),
    VitaminD = rowMeans(select(., DR1TVD, DR2TVD), na.rm = TRUE)
  )

nutrition_data <- nutrition %>%
  rowwise() %>%
  mutate(
    weighted_Sodium = weighted(Sodium, WTINTPRP),
    weighted_Potassium = weighted(Potassium, WTINTPRP),
    weighted_VitaminD = weighted(VitaminD, WTINTPRP)
  )

nutrition_long <- nutrition_data %>%
  select(Race, Sodium, Potassium, VitaminD) %>%
  pivot_longer(
    cols = -Race,
    names_to = "Nutrition",
    values_to = "Mean_Intake"
  )

ggplot(data = nutrition_long, aes(x = Mean_Intake, fill = Race))+
  geom_density(alpha = 0.5, adjust = 1.2) +
  facet_wrap(~ Nutrition, scales = "free", nrow = 1) +
  labs(title = "Density Plots of Mean Nutrient Intakes",
       x = "level",
       y = "Density",
       fill = "Race Category") +
  theme_minimal() +
  theme(
    trip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)  
  )
```


ggplot(data = nutrition_data, aes(x = Sodium, y= ..density.. * 100,fill = Race))+
  geom_density(alpha = 0.5, adjust = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(title = "Density Plots of Sodium",
       x = "level",
       y = "Density",
       fill = "Race Category") +
  theme_minimal() +
  theme(
    trip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)  
  )
```



nutrition_data = nutrition_data %<% 
  mutate(Race == "Other Hispanic"  & Race == "Non-Hispanic White")

ggplot(data = nutrition_data, aes(x = Potassium, y= ..density.. * 100, fill = Race))+
  geom_density(alpha = 0.5, adjust = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(title = "Density Plots of Potassium",
       x = "level",
       y = "Density",
       fill = "Race Category") +
  theme_minimal() +
  theme(
    trip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)  
  )
```


ggplot(data = nutrition_data, aes(x = VitaminD, y= ..density.. * 100, fill = Race))+
  geom_density(alpha = 0.5, adjust = 1.2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))+
  labs(title = "Density Plots of VitaminD",
       x = "level",
       y = "Density",
       fill = "Race Category") +
  theme_minimal() +
  theme(
    trip.text = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)  
  )
```



nutrition_percentiles <- nutrition_data %>%
  select(Race, weighted_Sodium, weighted_Potassium, weighted_VitaminD) %>%
  pivot_longer(cols = -Race, names_to = "Nutrition", values_to = "Weighted_Intake") %>%
  group_by(Nutrition, Race) %>%
  summarise(
    P50 = quantile(Weighted_Intake, 0.50, na.rm = TRUE),
    P75 = quantile(Weighted_Intake, 0.75, na.rm = TRUE),
    P95 = quantile(Weighted_Intake, 0.95, na.rm = TRUE),
    .groups = 'drop'
  )

nutrition_percentiles %>%
  kbl(caption = "Nutrient Intake Percentiles by Race for Females (Weighted), 2017-2020") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

```

## Interpretation of Nutrient Intake Percentiles by Race for Females (Weighted) 2017-2020

This table summarizes nutrient intake percentiles for **Potassium**, **Sodium**, and **Vitamin D** across three racial groups: **Mexican American**, **Non-Hispanic White**, and **Other Hispanic**.

### Potassium Intake

- **Mexican Americans** have higher potassium intake across all percentiles (P50: **2115.75 mg**, P75: **2780.25 mg**, P95: **3851.38 mg**), indicating better potassium consumption compared to the other groups.
- **Non-Hispanic Whites** rank second in potassium intake, with a median intake of **1982.50 mg**.
- **Other Hispanics** exhibit the lowest potassium intake (P50: **1882.50 mg**), which might suggest a gap in consuming potassium-rich foods like fruits and vegetables.

### Sodium Intake

- **Non-Hispanic Whites** have the highest sodium intake, with particularly concerning levels at the 95th percentile (**4897.08 mg**), surpassing recommended dietary limits.
- **Mexican Americans** also show high sodium intake, with the 95th percentile reaching **5482.25 mg**, which could increase the risk for hypertension.
- **Other Hispanics** have lower sodium intake across the board, with a median intake of **2267.75 mg**, which is closer to recommended guidelines.

### Vitamin D Intake

- **Mexican Americans** consume the most **Vitamin D** at the upper percentiles, with the 95th percentile reaching **10.83 mcg**, which aligns with recommended intake.
- **Non-Hispanic Whites** and **Other Hispanics** have similar median Vitamin D intake (P50: **2.45 mcg** and **2.28 mcg**, respectively), but may not be reaching optimal intake levels for this nutrient.

## Summary

This table reveals differences in nutrient intake across racial groups:
  - **Mexican Americans** have higher potassium and Vitamin D intake but also show high sodium consumption.
- **Non-Hispanic Whites** have the highest sodium intake, potentially contributing to higher risks of cardiovascular diseases.
- **Other Hispanics** tend to have lower intake levels for these nutrients.



# Create diabetes and HBP_Percentageh blood pressure indicators
data_filtered <- data_filtered %>%
  mutate(diabetes = ifelse(DIQ010 == 1, 1, 0),
         HBP_Percentageh_bp = ifelse(BPQ030 == 1, 1, 0),
         obesity = ifelse(BMXBMI >=30, 1, 0))

# Calculate the weighted diabetes and HBP_Percentageh blood pressure percentages by race group
health_data <- data_filtered %>%
  group_by(Race) %>%
  summarise(
    Diabetes_Percentage = sum(diabetes * WTINTPRP, na.rm = TRUE) / sum(WTINTPRP, na.rm = TRUE) * 100,
    HBP_Percentage = sum(HBP_Percentageh_bp * WTINTPRP, na.rm = TRUE) / sum(WTINTPRP, na.rm = TRUE) * 100,
    Obesity_Percentage = sum(obesity * WTINTPRP, na.rm = TRUE) / sum(WTINTPRP, na.rm = TRUE) * 100
  )

# Reshape the data for plotting (long format)
health_data_long <- health_data %>%
  tidyr::pivot_longer(cols = c(Diabetes_Percentage, HBP_Percentage, Obesity_Percentage), 
                      names_to = "Health_Condition", 
                      values_to = "Percentage")

# Plotting the bar plot
ggplot(health_data_long, aes(x = Race, y = Percentage, fill = Health_Condition)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  # Add percentage labels
  labs(x = "Race", y = "Percentage", 
       title = "Diabetes, Hypertension and Obesity Percentage by Race") +
  theme_minimal() +
  scale_fill_manual(values = c("Diabetes_Percentage" = "deepskyblue3", "HBP_Percentage" = "darkolivegreen3", "Obesity_Percentage" = "darkorange")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 20, hjust = 1),
        plot.background = element_rect(fill = "lightcyan1", color = NA)
  )

```


# Create a function to calculate weighted variance
weighted_variance <- function(obese, weight) {
  weighted_mean <- sum(obese * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)
  variance <- sum((weight^2) * (weighted_mean - weighted_mean^2), na.rm = TRUE) / sum(weight, na.rm = TRUE)^2
  return(variance)
}

weighted_sd <- function(obese, weight) {
  variance <- weighted_variance(obese, weight)
  return(sqrt(variance))
}

# Create obesity column and combine race categories
obesityrela <- fwtage %>%
  mutate(obese = ifelse(BMXBMI >= 30, 1, 0), 
         RIDRETH1_combined = case_when(
           RIDRETH1 == 1 ~ "Mexican American", 
           RIDRETH1 == 2 ~ "Other Hispanic",
           RIDRETH1 == 3 ~ "Non-Hispanic White",
           RIDRETH1 == 4 ~ "Non-Hispanic Black",
           RIDRETH1 == 5 ~ "Other Race"
         ))%>%
  filter(!RIDRETH1 %in% c(4, 5))

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
# Create bar plot to display weighted obesity percentages by race
ggplot(obesity_data, aes(x = factor(RIDRETH1_combined), y = obesity_rate, fill = RIDRETH1_combined)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(obesity_rate, 1)), 
            vjust = -0.5,  # Position the labels just above the bars
            size = 5) +    # Adjust text size
  labs(x = "Race Category", y = "Percent", 
       title = "Weighted Percentage of Obesity by Race (Females, Age 18-65), 2017-2020") +
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

```

This bar plot displays the weighted percentage of obesity among females aged 18-65, categorized by race. The percentages on the bars represent the proportion of women in each racial group who are considered obese, based on a BMI threshold greater than 30.


# Create Box plot
ggplot(obesityrela, aes(x = factor(RIDRETH1_combined), y = BMXBMI, fill = RIDRETH1_combined)) +
  geom_boxplot() +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20) # custom breaks
  ) + 
  labs(x = "Race", y = "BMI", 
       title = "Distribution of BMI and Obesity Percentage by Race for Females (Age 18-65)") +
  geom_text(data = obesity_data, aes(x = RIDRETH1_combined, y = 40, label = paste0(round(obesity_rate, 1), "%")),
            color = "black", size = 5, vjust = -0.5) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14),
        plot.background = element_rect(fill = "lightcyan1", color = NA)
        #panel.grid.major = element_blank(),  # Remove major grid lines
        #panel.grid.minor = element_blank()
  )

```

This box plot represents the distribution of BMI for females aged 18-65 across different racial groups, with the red dashed line marking the BMI threshold for obesity (BMI = 30). Each box plot shows the distribution of BMI values within each racial group, and the percentages on top of the boxes indicate the weighted percentage of obesity in that group (i.e., the proportion of individuals with a BMI > 30).


# Desity Plot of IR for each of race within: Female & Age (18-65)

obesityrelaWithIR <- obesityrela %>%
  mutate(IR = (LBXGLU * LBXIN) / 405)

obesityDiabetes <- obesityrelaWithIR %>%
  mutate(HavingDiabetes = ifelse(DIQ010 == 1, "Diabetes", "Non-Diabetes"))

ggplot(data = obesityrelaWithIR, aes(x = IR, fill = RIDRETH1_combined))+
  geom_density(alpha = 0.5, adjust = 1.2) +
  scale_x_continuous(limits = c(0, 20)) +  
  labs(title = "Density Plot of Insulin Resistance (IR) by Race for Females (Age 18-65), 2017-2020",
       x = "HOMA-IR",
       y = "Density",
       fill = "Race Category") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14),
    legend.position = "right",  
    axis.text = element_text(size = 10)  
  )
```

## What is HOMA-IR?

HOMA-IR (Homeostasis Model Assessment of Insulin Resistance) is a widely used method for assessing insulin resistance, which refers to the reduced sensitivity of cells to insulin. When cells become resistant to insulin, the body needs to produce more insulin to keep blood sugar levels within a normal range.

Lower HOMA-IR values: Indicate better insulin sensitivity (the body’s cells respond well to insulin).

higher HOMA-IR values: Indicate insulin resistance (the body’s cells are less responsive to insulin, meaning the pancreas needs to produce more insulin to keep blood sugar levels normal).



ggplot(obesityrelaWithIR, aes(x = factor(RIDRETH1_combined), y = BMXBMI, fill = RIDRETH1_combined)) +
  geom_boxplot() +
  geom_hline(yintercept = 30, linetype = "dashed", color = "red") + 
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, by = 20) # custom breaks
  ) + 
  labs(x = "Race", y = "BMI", 
       title = "Box Plots of BMI by Race (Females, Age 18-65), 2017-2020") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.background = element_rect(fill = "lightcyan1", color = NA),
    legend.position = "none",
    axis.text.x = element_text(angle = 40, hjust = 1)
  )

```

### Findings:

Individuals in the Diabetes panel tend to have higher BMI values across all racial groups compared to those in the Non-Diabetes panel.

In each racial group, the median BMI for individuals with diabetes is noticeably higher than for those without diabetes, which suggests a correlation between higher BMI and diabetes prevalence.

### Conclusion:

The plot highlights the strong relationship between higher BMI and diabetes across racial groups. It also suggests that Non-Hispanic Black and Mexican American & Other Hispanic women may be at the highest risk for obesity and diabetes-related health issues, while Other Race women appear to have a lower risk.


IR_percentiles <- obesityDiabetes %>%
  select(RIDRETH1_combined,IR) %>%
  pivot_longer(cols = -RIDRETH1_combined, names_to = "Diabetes", values_to = "IR") %>%
  group_by(RIDRETH1_combined) %>%
  summarise(
    P50 = quantile(IR, 0.50, na.rm = TRUE),
    P75 = quantile(IR, 0.75, na.rm = TRUE),
    P95 = quantile(IR, 0.95, na.rm = TRUE),
    .groups = 'drop'
  )

IR_percentiles %>%
  kbl(caption = "IR Percentiles by Race (Females, Age 18-65), 2017-2020") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE)

```

## Interpretation of IR Percentiles by Race

This table summarizes insulin resistance (IR) percentiles for **Mexican American**, **Non-Hispanic White**, and **Other Hispanic** populations.

### Mexican American

- The **Mexican American** group exhibits the highest insulin resistance levels across all percentiles:
  - Median (P50): **3.48**
  - 75th Percentile (P75): **6.40**
  - 95th Percentile (P95): **13.47**
  - This suggests that **Mexican Americans** are at a higher risk for metabolic conditions such as type 2 diabetes, with many individuals exhibiting elevated insulin resistance.

### Non-Hispanic White

- **Non-Hispanic Whites** show the lowest insulin resistance at the median (P50: **2.10**) and 75th percentile (P75: **3.73**).
- However, at the 95th percentile, insulin resistance increases to **11.18**, suggesting that while the majority of this group has lower IR, a subset of individuals is at risk for high IR levels.

### Other Hispanic

- **Other Hispanics** fall between the other two groups in terms of insulin resistance:
  - Median (P50): **2.75**
  - 75th Percentile (P75): **5.01**
  - 95th Percentile (P95): **10.11**
  - This indicates moderate insulin resistance levels within this group.

## Summary

The table shows significant variation in insulin resistance across racial groups:
  - **Mexican Americans** have the highest IR levels, which may contribute to increased diabetes risk.
- **Non-Hispanic Whites** generally have lower IR levels but exhibit a sharp increase in the upper percentiles.
- **Other Hispanics** exhibit moderate IR levels.


# Function to calculate weighted standard error
weighted_se <- function(x, w) {
  # Remove NAs
  w <- w[!is.na(x)]
  x <- x[!is.na(x)]
  
  # Calculate the weighted mean
  weighted_mean <- sum(w * x) / sum(w)
  
  # Calculate the weighted standard deviation (variance)
  variance <- sum(w * (x - weighted_mean)^2) / sum(w)
  weighted_sd <- sqrt(variance)
  
  # Calculate the effective sample size
  n_eff <- (sum(w))^2 / sum(w^2)
  
  # Return the weighted standard error
  return(weighted_sd / sqrt(n_eff))
}

# Calculate weighted obesity percentage, standard deviation, and standard error by race category
obesity_data <- obesityrela %>%
  group_by(RIDRETH1_combined) %>%
  summarise(
    obesity_rate = sum(obese * WTINTPRP, na.rm = TRUE) / sum(WTINTPRP, na.rm = TRUE) * 100,
    obesity_sd = weighted_sd(obese, WTINTPRP) * 100,  
    obesity_se = weighted_se(obese, WTINTPRP) * 100  # Calculate weighted standard error
  )


```

The weighted standard error quantifies how precise the weighted mean obesity rate is as an estimate of the true population obesity rate.

Smaller values of the weighted SE indicate that the sample mean (obesity rate) is a more accurate estimate of the population mean, while larger values indicate more variability and uncertainty around the estimate.

(In clinical trials or biomedical studies, SE below 5% may be considered small.)
