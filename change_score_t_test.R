# Load necessary libraries
library(dplyr)
library(ggplot2)
library(BayesFactor)

# Load the data
df <- read.csv('df_clean.csv')
response_option = "food" #money/food

df <- df %>% filter(response == response_option)


# Calculate the R1 change score for each group
df_treatment <- df %>%
  filter(phase == "Treatment" | phase == "Test") %>%
  group_by(participant, phase, ExpCond) %>%
  summarise(final_response_rate = last(response_rate)) %>%
  spread(key = phase, value = final_response_rate) %>%
  mutate(R1_change_score = Treatment / (Treatment + Test)) %>%
  na.omit()

ggplot(df_treatment, aes(x = ExpCond, y = R1_change_score, group = ExpCond)) +
  geom_boxplot()


# t test to test for differences among groups
t_test_result <- wilcox.test(R1_change_score ~ ExpCond, data = df_treatment,alternative = "two.sided")
t_test_result




# Initialize a data frame to store Bayesian t-test results
## Compute Bayes factor
bf = ttestBF(formula = R1_change_score ~ ExpCond, data = df_treatment)
bf

chains = posterior(bf, iterations = 10000)
plot(chains[,2])


# Plot the R1 change scores by ExpCond
ggplot(df_treatment, aes(x = ExpCond, y = R1_change_score, fill = ExpCond)) +
  geom_boxplot() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "R1 Change Score by Experimental Condition (ExpCond)", y = "R1 Change Score", x = "Experimental Condition")