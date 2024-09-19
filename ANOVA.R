#selecting working directiory
setwd('D:/Projects/tubitak_2209')

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(car)  # For ANOVA and regression diagnostics
library(rstatix)

# Load your dataset
df <- read.csv('df_clean.csv')

# Convert necessary columns to factors
df$phase <- factor(df$phase, levels = c("Training", "Treatment", "Test"))
df$ExpCond <- factor(df$ExpCond, levels = c("punishment", "control"))

# Create a unique ID for participants if not already present
df$participant <- as.factor(df$participant)


# subsetting
training_data <- subset(df, phase == "Training")
treatment_data <- subset(df, phase == "Treatment")
test_data <- subset(df, phase == "Test")

#outliers VAR.
training_data %>%
  group_by(ExpCond, block_number_within_phase) %>%
  identify_outliers(response_rate) 
treatment_data %>%
  group_by(ExpCond, block_number_within_phase) %>%
  identify_outliers(response_rate) 
test_data %>%
  group_by(ExpCond, block_number_within_phase) %>%
  identify_outliers(response_rate) 


#normality KARŞILANMIYOR.
training_data %>%
  group_by(ExpCond, block_number_within_phase) %>%
  shapiro_test(response_rate) 
treatment_data %>%
  group_by(ExpCond, block_number_within_phase) %>%
  shapiro_test(response_rate) 
test_data %>%
  group_by(ExpCond, block_number_within_phase) %>%
  shapiro_test(response_rate) 


#ANOVAs
anova_training <- aov(response_rate ~ ExpCond * block_number_within_phase + Error(participant/block_number_within_phase), data = training_data)
summary(anova_training)

anova_treatment <- aov(response_rate ~ ExpCond * block_number_within_phase + Error(participant/block_number_within_phase), data = treatment_data)
summary(anova_treatment)

anova_test <- aov(response_rate ~ ExpCond * block_number_within_phase + Error(participant/block_number_within_phase), data = test_data)
summary(anova_test)
