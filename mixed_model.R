#selecting working directiory
setwd('D:/Projects/tubitak_2209')

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(lmerTest)

# Load your dataset
df <- read.csv('df_clean.csv')

# Mixed-effects model for the Training phase
training_data <- subset(df, phase == "Training")
# Fit a mixed model
mixed_model_training <- lmer(response_rate ~ ExpCond * block_number_within_phase + (1 | participant), data = training_data)
# Summary of the model
summary(mixed_model_training)


# Mixed-effects model for the Treatment phase
treatment_data <- subset(df, phase == "Treatment")
mixed_model_treatment <- lmer(response_rate ~ ExpCond * block_number_within_phase + (1 | participant), data = treatment_data)
summary(mixed_model_treatment)


# Mixed-effects model for the Test phase
test_data <- subset(df, phase == "Test")
mixed_model_test <- lmer(response_rate ~ ExpCond * block_number_within_phase + (1 | participant), data = test_data)
summary(mixed_model_test)


