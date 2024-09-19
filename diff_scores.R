# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
df <- read.csv('df_clean.csv')


df_diff <- df %>%
  filter((phase == "Treatment" & block_number_within_phase == 4) | 
           (phase == "Test" & block_number_within_phase <= 4)) %>%
  mutate(block_number_within_phase = case_when(
    phase == "Treatment" ~ (block_number_within_phase - 3),
    phase == "Test" ~ (block_number_within_phase + 1)
  )) %>% 
  select(-food, -money, -phase) %>%
  pivot_wider(names_from = block_number_within_phase, 
                              values_from = response_rate) %>%
  select(participant, ExpCond, "1", "2", "3", "4", "5") %>%
  rename(b1 = "1", b2 = "2", b3 = "3", b4 = "4", b5 = "5") %>%
  mutate(
    first_diff = b2- b1,
    sec_diff = b3 - b1,
    three_diff = b4 - b1,
    fourth_diff = b5 -b1)



# 'wilcox.test' kullanarak grupları karşılaştırma

# first_diff için karşılaştırma
test_first_diff <- t.test(first_diff ~ ExpCond, data = df_diff, alternative = "two.sided")
print("first_diff için Wilcoxon testi sonucu:")
print(test_first_diff)

# sec_diff için karşılaştırma
test_sec_diff <- t.test(sec_diff ~ ExpCond, data = df_diff, alternative = "two.sided")
print("sec_diff için Wilcoxon testi sonucu:")
print(test_sec_diff)

# three_diff için karşılaştırma
test_three_diff <- t.test(three_diff ~ ExpCond, data = df_diff, alternative = "two.sided")
print("three_diff için Wilcoxon testi sonucu:")
print(test_three_diff)

# fourth_diff için karşılaştırma
test_fourth_diff <- t.test(fourth_diff ~ ExpCond, data = df_diff, alternative = "two.sided")
print("fourth_diff için Wilcoxon testi sonucu:")
print(test_fourth_diff)



  