# Çalışma dizini ve kütüphaneler
setwd('D:/Projects/tubitak_2209')

# Gerekli kütüphanelerin yüklenmesi
library(dplyr)
library(rstatix)

# Verinin yüklenmesi
df <- read.csv('df_clean.csv')

# Test phase'e ait veriyi filtrele
df_test_phase <- df %>% filter(phase == "Test")

# Her blok ve ExpCond için tek örneklem t-testi yapma
t_test_results <- df_test_phase %>%
  group_by(block_number_within_phase, ExpCond) %>%
  summarise(
    t_test = list(t.test(response_rate, mu = 0.5, alternative = "two.sided")),
    p_value = t_test[[1]]$p.value,
    statistic = t_test[[1]]$statistic,
    mean_response_rate = mean(response_rate)
  )

# Test sonuçlarını yazdırma
print("Her blok ve ExpCond için tek örneklem t-testi sonuçları:")
print(t_test_results)
