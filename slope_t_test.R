# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load the data
df <- read.csv('df_clean.csv')

# Filter to include only the last block of Treatment and all blocks of Test phase
df_slope <- df %>%
  filter((phase == "Treatment" & block_number_within_phase == 4) | 
           (phase == "Test" & block_number_within_phase <= 4)) %>%
  mutate(block_number_within_phase = case_when(
    phase == "Treatment" ~ (block_number_within_phase - 3),
    phase == "Test" ~ (block_number_within_phase + 1)
  )) 

model <- lm(response_rate ~ ExpCond * block_number_within_phase, data = df_slope)
summary(model)

df_slope %>% 
  group_by(ExpCond, block_number_within_phase) %>%
  summarise(mean_response_rate = mean(response_rate)) %>%
  ggplot() +
  geom_point(aes(x = block_number_within_phase, y = mean_response_rate, color = ExpCond))


# Calculate slopes for each participant and experimental condition
slopes_df <- df_slope %>%
  group_by(participant, ExpCond) %>%
  do(model = lm(response_rate ~ block_number_within_phase, data = .)) %>%
  mutate(slope = coef(model)[["block_number_within_phase"]]) %>%
  select(participant, ExpCond, slope)


ggplot(slopes_df, aes(x = ExpCond, y = slope, group = ExpCond)) +
  geom_boxplot()

# t test to test for differences among groups
t_test_result <- t.test(slope ~ ExpCond, data = slopes_df,alternative = "two.sided")
t_test_result



# Gerekli kütüphanelerin yüklenmesi
library(ggplot2)

# Mean slopes ve standart hatalar için verilerin hesaplanması
mean_slopes <- slopes_df %>%
  group_by(ExpCond) %>%
  summarise(
    mean_slope = mean(slope),
    se_slope = sd(slope) / sqrt(n())
  )

# Renk paleti belirleme
colors <- c("punishment" = "#8B0000", "control" = "#556B2F", "€0.10" = "#CCCCCC", "€0.50" = "#999999", "€2.00" = "#666666")

# Mean slopes ile bar grafiği oluşturma
p <- ggplot(mean_slopes, aes(x = ExpCond, y = mean_slope, fill = ExpCond)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_slope - se_slope, ymax = mean_slope + se_slope), 
                width = 0.2, position = position_dodge(0.9)) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  labs(x = "Gruplar",
       y = "Ortalama Eğim")

# İki grup için örneklem verilerini çekelim
group1 <- slopes_df %>% filter(ExpCond == "punishment") %>% pull(slope)
group2 <- slopes_df %>% filter(ExpCond == "control") %>% pull(slope)

# İki örneklem t-testi uygulamak
t_test_result <- t.test(group1, group2)

# Eğer anlamlı fark varsa, yıldız ekle
if (t_test_result$p.value < 0.05) {
  p <- p + annotate("text", x = 1.5, y = max(mean_slopes$mean_slope) + 0.05, label = "*", size = 6)
}


p <- p + scale_x_discrete(labels = c("control" = "Kontrol", "punishment" = "Ceza")) + guides(fill = "none")
# Grafiği gösterme
print(p)


# Grafiği kaydetme
ggsave("slope.png", plot = p, width = 15, height = 10, units = "cm", dpi = 300)


#SLOPEEE---------------
library(ggplot2)
library(dplyr)
# Veriyi özetleme: her koşul ve blok için ortalama tepki oranını hesapla
df_summary <- df %>%
  group_by(ExpCond, block_number) %>%
  summarise(mean_response_rate = mean(response_rate, na.rm = TRUE)) %>%
  ungroup()
# Nokta ve çizgi grafiği oluşturma
ggplot(df_summary, aes(x = block_number, y = mean_response_rate, color = ExpCond, group = ExpCond)) +
  geom_point(size = 3) +           # Noktaları çizme
  geom_line(size = 1.2) +          # Çizgileri çizme
  labs(
    title = "Deney Koşuluna Göre R1 Tepki Oranının Ortalama Eğilimleri",
    x = "Blok Numarası",
    y = "Ortalama R1 Tepki Oranı",
    color = "Deney Koşulu"
  ) +
  theme_minimal() +                # Minimal tema
  theme(
    plot.title = element_text(hjust = 0.5),   # Başlığı ortala
    legend.position = "top"                   # Efsane konumunu ayarla
  )
