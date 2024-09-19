#importing libraries and data. failed list ---------------
library(readr)
library(tidyr)
library(dplyr)
library(openxlsx)
library(ggplot2)


#selecting working directiory
setwd('D:/Projects/tubitak_2209')

failed_list <- c("1","3","6","12","13","14","22","24","27","38","40","41","44","49","52","56")

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
  filter(!participant %in% failed_list) %>%
  mutate(response_rate = (total_response / 60))

# Recalculate summary statistics for plotting
data_across_pp <- df_longer %>%
  group_by(block_number_within_phase, ExpCond, response, phase) %>%
  summarise(
    total_response_mean = mean(total_response),
    total_response_sd = sd(total_response),
    n = n()
  ) %>%
  mutate(
    total_response_SEM = total_response_sd / sqrt(n),
    total_response_CI = total_response_SEM * qt((1 - 0.05) / 2 + .5, n - 1)
  )

# Plot with filtered data
p1.0 <- ggplot() +
  geom_point(
    data = data_across_pp,
    aes(
      x = block_number_within_phase, y = total_response_mean, color = ExpCond
    ),
    stat = "identity",
    alpha = 0.8,
    size = 5,
    position = position_dodge(0.3),
    show.legend = FALSE
  )+
  geom_errorbar(
    data = data_across_pp,
    aes(
      x = block_number_within_phase,
      y = total_response_mean,
      ymin = total_response_mean - total_response_CI,
      ymax = total_response_mean + total_response_CI,
      color = ExpCond,
      group = ExpCond,
    ),
    linewidth = 1.0,
    width = .00,
    alpha = 0.8,
    position = position_dodge(0.5)
  ) +
  #scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 5.5)) +
  #scale_x_continuous(breaks = c(1, 2, 3, 4, 5)) +  # Include 5 blocks for Training
  theme(
    axis.title.x = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    axis.title.y = element_text(
      color = "black",
      size = 14,
      face = "bold"
    ),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "grey"),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10, face = "bold"),
    strip.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(y = "Response Rate", x = "Blocks") +
  scale_color_manual(
    labels = c("No-Punishment", "Punishment"),
    values = c("darkgreen", "darkred"),
    name = ""
  ) +
  facet_wrap(~response + phase, nrow = 2)

p1.0

write_csv(df_longer, "df_clean.csv")
