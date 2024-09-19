#importing libraries and data. failed list ---------------
library(readr)
library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)


#selecting working directiory
setwd('D:/Projects/tubitak_2209')

failed_list <- c("17","1","2","6","38","41","44","52","56","40","12","10","43","24","47","49","30","51")

df_raw <- read.xlsx('df.xlsx')
glimpse(df_raw)

# analyze-----------------------
df <- df_raw %>%
  mutate(phase = case_when(
    block_number <= 5 ~ "Training",
    block_number > 5 & block_number <= 9 ~ "Treatment",
    block_number > 9 ~ "Test"
  ),
  phase = factor(phase, levels = c("Training", "Treatment", "Test")),
  block_number_within_phase =  case_when(
    block_number <= 5 ~ block_number,  # Training phase, use block_number as is
    block_number > 5 & block_number <= 9 ~ (block_number - 5),  # Treatment phase, start from 1
    block_number > 9 ~ (block_number - 9)  # Test phase, start from 1
  )
  )



# Update the 'df_longer' data frame with correct phase filtering ------------

df_longer <- df %>%
  filter(response != "wrong_key") %>%
  group_by(participant, block_number_within_phase, response, phase) %>%
  summarise(total_response = n(), .groups = 'drop') %>%
  complete(
    participant, 
    nesting(block_number_within_phase, phase), 
    response, 
    fill = list(total_response = 0)
  ) %>%
  # Ensure the correct number of blocks for each phase
  filter(
    !(phase == "Training" & block_number_within_phase > 5) & 
      !(phase == "Treatment" & block_number_within_phase > 4) & 
      !(phase == "Test" & block_number_within_phase > 4)
  ) %>%
  left_join(
    df %>% select(participant, block_number_within_phase, ExpCond, counterbalanced_groups) %>%
      distinct(),
    by = c("participant", "block_number_within_phase")
  ) %>%
  mutate(participant = gsub("[^0-9]", "", participant)) %>%
  mutate(participant = as.integer(participant)) %>%
  mutate(participant = factor(participant, levels = sort(unique(participant), na.last = TRUE))) %>%
  # Exclude participants in failed_list
  filter(!participant %in% failed_list) 
  #%>% mutate(response_rate = (response_rate / 60))


df_wider <- df_longer %>% pivot_wider(names_from = response, values_from =  total_response) %>%
  mutate(response_rate = ifelse((food + money) == 0, 0, (food / (food + money))))



# Recalculate summary statistics for plotting
data_across_pp <- df_wider %>%
  group_by(block_number_within_phase, ExpCond, phase) %>%
  summarise(
    response_rate_mean = mean(response_rate),
    response_rate_sd = sd(response_rate),
    n = n()
  ) %>%
  mutate(
    response_rate_SEM = response_rate_sd / sqrt(n),
    response_rate_CI = response_rate_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

position_dodge_value = 0.3
# Plot with filtered data
p1.0 <- ggplot() +
  geom_point(data = df_wider, aes(x = block_number_within_phase,
                                  y = response_rate,
                                  color = ExpCond,
                                  ),
             size = 0.77,
             alpha = 0.4,
             position = position_jitterdodge(0.1)) +
  geom_point(
    data = data_across_pp,
    aes(
      x = block_number_within_phase, y = response_rate_mean, color = ExpCond
    ),
    stat = "identity",
    alpha = 0.8,
    size = 5,
    position = position_dodge(position_dodge_value),
    show.legend = FALSE
  )+
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = block_number_within_phase,
      y = response_rate_mean,
      ymin = response_rate_mean - response_rate_CI,
      ymax = response_rate_mean + response_rate_CI,
      color = ExpCond,
      group = ExpCond,
    ),
    linewidth = 1.0,
    width = .00,
    alpha = 0.8,
    position = position_dodge(position_dodge_value)
  ) +
  geom_smooth(
    data = data_across_pp,
    aes(
      x = block_number_within_phase, y = response_rate_mean, color = ExpCond, group = ExpCond
    ),
    method = "lm",  
    se = FALSE,  
    linetype = "dashed",  
    size = 1.22,
    alpha = 0.1
  ) +
  labs(y = "Aburcubur Tepki Oranı", x = "Bloklar") +
  theme_minimal() +
  scale_color_manual(
    labels = c("Kontrol Grubu", "Ceza Grubu"),
    values = c("#556B2F", "#8B0000"),
    name = ""
  ) +
  facet_wrap(~phase, nrow = 1, labeller = as_labeller(c("Training" = "Eğitim", 
                                                        "Treatment" = "Mudahale",
                                                        "Test"="Test"))) + 
  guides(color = "none")

p1.0


write_csv(df_wider, "df_clean.csv")
# Grafiği kaydetme
ggsave("dotplot.png", plot = p1.0, width = 15, height = 10, units = "cm", dpi = 300)
