# Control grubu için filtreleme
df_slope_control <- df_slope %>% filter(ExpCond == "control")

# Control grubu için ikinci derece polinomik model
model_poly_control <- lm(response_rate ~ poly(block_number_within_phase, 2), data = df_slope_control)
summary(model_poly_control)

# Punishment grubu için ikinci derece polinomik model
df_slope_punishment <- df_slope %>% filter(ExpCond == "punishment")
model_poly_punishment <- lm(response_rate ~ poly(block_number_within_phase, 2), data = df_slope_punishment)
summary(model_poly_punishment)

# Polinomik modellerin görselleştirilmesi
library(ggplot2)

df_slope %>%
  ggplot(aes(x = block_number_within_phase, y = response_rate, color = ExpCond)) +
  geom_point() +
  geom_smooth(data = df_slope_control, method = "lm", formula = y ~ poly(x, 2), color = "blue", se = FALSE) +
  geom_smooth(data = df_slope_punishment, method = "lm", formula = y ~ poly(x, 2), color = "red", se = FALSE) +
  labs(title = "Polinomik Regresyon - Control ve Punishment Grupları")

# Control grubu için logaritmik model
model_log_control <- lm(response_rate ~ log(block_number_within_phase), data = df_slope_control)
summary(model_log_control)

# Punishment grubu için logaritmik model
model_log_punishment <- lm(response_rate ~ log(block_number_within_phase), data = df_slope_punishment)
summary(model_log_punishment)

# Logaritmik modellerin görselleştirilmesi
df_slope %>%
  ggplot(aes(x = block_number_within_phase, y = response_rate, color = ExpCond)) +
  geom_point() +
  geom_smooth(data = df_slope_control, method = "lm", formula = y ~ log(x), color = "blue", se = FALSE) +
  geom_smooth(data = df_slope_punishment, method = "lm", formula = y ~ log(x), color = "red", se = FALSE) +
  labs(title = "Logaritmik Regresyon - Control ve Punishment Grupları")


# Control grubu için lineer regresyon modeli
model_linear_control <- lm(response_rate ~ block_number_within_phase, data = df_slope_control)
summary(model_linear_control)

# Punishment grubu için lineer regresyon modeli
model_linear_punishment <- lm(response_rate ~ block_number_within_phase, data = df_slope_punishment)
summary(model_linear_punishment)

# Lineer modellerin görselleştirilmesi
df_slope %>%
  ggplot(aes(x = block_number_within_phase, y = response_rate, color = ExpCond)) +
  geom_point() +
  geom_smooth(data = df_slope_control, method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  geom_smooth(data = df_slope_punishment, method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  labs(title = "Lineer Regresyon - Control ve Punishment Grupları")


# Control ve Punishment grupları için veri filtreleme
df_slope_control <- df_slope %>% filter(ExpCond == "control")
df_slope_punishment <- df_slope %>% filter(ExpCond == "punishment")

# Control grubu için modeller
model_linear_control <- lm(response_rate ~ block_number_within_phase, data = df_slope_control)
model_poly_control <- lm(response_rate ~ poly(block_number_within_phase, 2), data = df_slope_control)
model_log_control <- lm(response_rate ~ log(block_number_within_phase), data = df_slope_control)

# Punishment grubu için modeller
model_linear_punishment <- lm(response_rate ~ block_number_within_phase, data = df_slope_punishment)
model_poly_punishment <- lm(response_rate ~ poly(block_number_within_phase, 2), data = df_slope_punishment)
model_log_punishment <- lm(response_rate ~ log(block_number_within_phase), data = df_slope_punishment)

# Model özetleri ve AIC, BIC karşılaştırmaları
library(broom)
library(dplyr)

# Control grubu için model değerlendirmesi
control_model_comparison <- tibble(
  Model = c("Linear", "Polynomial", "Logarithmic"),
  R2 = c(summary(model_linear_control)$r.squared, summary(model_poly_control)$r.squared, summary(model_log_control)$r.squared),
  Adj_R2 = c(summary(model_linear_control)$adj.r.squared, summary(model_poly_control)$adj.r.squared, summary(model_log_control)$adj.r.squared),
  AIC = c(AIC(model_linear_control), AIC(model_poly_control), AIC(model_log_control)),
  BIC = c(BIC(model_linear_control), BIC(model_poly_control), BIC(model_log_control))
)

# Punishment grubu için model değerlendirmesi
punishment_model_comparison <- tibble(
  Model = c("Linear", "Polynomial", "Logarithmic"),
  R2 = c(summary(model_linear_punishment)$r.squared, summary(model_poly_punishment)$r.squared, summary(model_log_punishment)$r.squared),
  Adj_R2 = c(summary(model_linear_punishment)$adj.r.squared, summary(model_poly_punishment)$adj.r.squared, summary(model_log_punishment)$adj.r.squared),
  AIC = c(AIC(model_linear_punishment), AIC(model_poly_punishment), AIC(model_log_punishment)),
  BIC = c(BIC(model_linear_punishment), BIC(model_poly_punishment), BIC(model_log_punishment))
)

# Sonuçları görüntüleyin
print("Control grubu model karşılaştırması:")
print(control_model_comparison)
print("Punishment grubu model karşılaştırması:")
print(punishment_model_comparison)


