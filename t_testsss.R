# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
df <- read.csv('df_clean.csv')

# Filtreleme öncesi kontrol
unique(df$block_number_within_phase)
unique(df$phase)

# Veriyi filtreleyin
block <- 1
phase_name <- "Test"
df_filt <- df %>% filter(
  block_number_within_phase == block) %>%
  filter(phase == phase_name)

# Filtrelenmiş veri setini kontrol edin
print(df_filt)


# T-testi (Wilcoxon rank-sum testi)
t_test_result <- wilcox.test(response_rate ~ ExpCond, data = df_filt, alternative = "two.sided")
print(t_test_result)
