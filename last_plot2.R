# Gerekli kütüphaneleri yükle
library(ggplot2)
library(dplyr)

# Veriyi R'da yükle
df <- read.csv('df_clean.csv')

# Yalnızca Treatment'ın son bloğu ve Test fazının tüm bloklarını dahil etmek için filtrele
df_slope <- df %>%
  filter((phase == "Treatment" & block_number_within_phase == 4) | 
           (phase == "Test" & block_number_within_phase <= 4)) %>%
  mutate(block_number_within_phase = case_when(
    phase == "Treatment" ~ (block_number_within_phase - 3),
    phase == "Test" ~ (block_number_within_phase + 1)
  )) 

# Renk paleti belirleme
colors <- c("punishment" = "#8B0000", "control" = "#556B2F")

# Nokta ve çizgi grafiği oluştur ve renkli SE bantları ekle
ggplot(df_slope, aes(x = block_number_within_phase, y = response_rate, color = ExpCond, group = ExpCond)) +
  geom_point(size = 1.5, alpha = 0.3, position = position_jitter(width = 0.1, height = 0)) +  # Bireysel noktalar
  geom_smooth(method = "lm", se = TRUE, aes(fill = ExpCond), linetype = "dashed", size = 1) +  # Renkli SE bantları ve regresyon çizgileri
  scale_color_manual(values = colors) +  # Çizgi renklerini özelleştirme
  scale_fill_manual(values = colors) +  # SE bantlarının renklerini özelleştirme
  labs(
    x = "Blok Numarası",
    y = "Ortalama Yemek Tepkisi Oranı"
  ) +
  theme_minimal() +  # Minimal tema
  theme(
    legend.position = "none")

# Grafiği kaydetme
ggsave("slope_with_colored_se_bands.png", width = 15, height = 10, units = "cm", dpi = 300)
