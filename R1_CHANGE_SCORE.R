# Çalışma dizini ve kütüphaneler
setwd('D:/Projects/tubitak_2209')

# Gerekli kütüphanelerin yüklenmesi
library(tidyverse)
library(dplyr)
library(rstatix)

# Verinin yüklenmesi
df <- read.csv('df_clean.csv')
df_diff <- df %>%
  filter((phase == "Treatment" & block_number_within_phase == 4) | 
           (phase == "Training" & block_number_within_phase == 5)) %>%
  select(-block_number_within_phase, -response_rate, -money, -counterbalanced_groups) %>%
  pivot_wider(names_from = phase, 
              values_from = food) %>%
  mutate(
    R1_change = Treatment / (Treatment + Training))


# Her bir ExpCond için tek örneklem t-testi uygulamak
test_results <- df_diff %>%
  group_by(ExpCond) %>%
  summarise(
    t_test = list(t.test(R1_change, mu = 0.5))
  )
# Gerekli kütüphanelerin yüklenmesi
library(ggplot2)

# Gruplar bazında ortalama ve standart hatayı hesaplama
df_summary <- df_diff %>%
  group_by(ExpCond) %>%
  summarise(
    mean_R1_change = mean(R1_change, na.rm = TRUE),
    se_R1_change = sd(R1_change, na.rm = TRUE) / sqrt(n())
  )

# Renk paleti belirleme
colors <- c("punishment" = "#8B0000", "control" = "#556B2F", "€0.10" = "#CCCCCC", "€0.50" = "#999999", "€2.00" = "#666666")

# Test sonuçlarının hesaplanması ve test_results'un oluşturulması
test_results <- df_diff %>%
  group_by(ExpCond) %>%
  summarise(
    t_test = list(t.test(R1_change, mu = 0.5))
  ) %>%
  mutate(
    t_statistic = sapply(t_test, function(x) x$statistic),
    p_value = sapply(t_test, function(x) x$p.value),
    mean_difference = sapply(t_test, function(x) x$estimate - 0.5)
  ) %>%
  select(-t_test)

# Bar grafiği oluşturma
p <- ggplot(df_summary, aes(x = ExpCond, y = mean_R1_change, fill = ExpCond)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_R1_change - se_R1_change, ymax = mean_R1_change + se_R1_change), 
                width = 0.2) +
  scale_fill_manual(values = colors) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_y_continuous(limits = c(0,0.6),
                     breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6)) +
  labs(x = "Gruplar", y = "Yemek Tepkisi Değişim Oranı") +
  theme_minimal() +
  theme(legend.position = "none")

# Yıldız ekleme kodu
for (i in 1:nrow(test_results)) {
  if (test_results$p_value[i] < 0.05 && test_results$mean_difference[i] < 0) {
    p <- p + annotate("text", x = i, y = df_summary$mean_R1_change[i] + df_summary$se_R1_change[i] + 0.03, 
                      label = "*", size = 6)
  }
}

# İki örneklem t-testinin sonucu ve yıldız ekleme
if(t_test_result$p.value < 0.05) {
  p <- p + annotate("text", x = 1.5, y = max(df_summary$mean_R1_change) + 0.1, label = "*", size = 6)
}

# Grafiği gösterme
print(p)

# Grafiği kaydetme
ggsave("R1_CHANGE.png", plot = p, width = 15, height = 10, units = "cm", dpi = 300)


# ExpCond değişkenini kullanarak iki grubun isimlerini belirleyelim.
group_names <- unique(df_diff$ExpCond)

# Grupların ismini kontrol edelim
print(group_names)

# İlk iki grup için iki örneklem t-testi uygulamak
group1 <- df_diff %>% filter(ExpCond == group_names[1]) %>% pull(R1_change)
group2 <- df_diff %>% filter(ExpCond == group_names[2]) %>% pull(R1_change)

# İki örneklem t-testi
t_test_result <- t.test(group1, group2)

# Sonuçları görüntüleme
t_test_result

# Hangi grubun ortalama 'R1_change' skoru daha düşük olduğunu belirleyelim
mean_group1 <- mean(group1, na.rm = TRUE)
mean_group2 <- mean(group2, na.rm = TRUE)

if (mean_group1 < mean_group2) {
  print(paste("Group", group_names[1], "has a lower mean R1_change score."))
} else if (mean_group1 > mean_group2) {
  print(paste("Group", group_names[2], "has a lower mean R1_change score."))
} else {
  print("Both groups have the same mean R1_change score.")
}

