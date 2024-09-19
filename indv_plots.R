# Klasör oluşturma (varsa atlar)
dir.create("individual_plots", showWarnings = FALSE)

# Her katılımcı için döngü başlatılır
participants <- unique(df_longer$participant)

for (p in participants) {
  # Her katılımcı için filtrelenmiş veri seti
  df_participant <- df_longer %>% filter(participant == p)
  
  # Grafik oluşturma
  p_individual <- ggplot(df_participant, aes(x = block_number_within_phase, y = response_rate)) +
    geom_point(alpha = 0.8, size = 3) +
    geom_line(alpha = 0.5) +
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5)) +
    theme(
      axis.title.x = element_text(color = "black", size = 14, face = "bold"),
      axis.title.y = element_text(color = "black", size = 14, face = "bold"),
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
    labs(title = paste("Participant:", p), y = "Response Rate", x = "Blocks") +
    facet_wrap(~response + phase, nrow = 2)
  
  # Grafiği kaydetme
  ggsave(filename = paste0("individual_plots/participant_", p, ".png"), plot = p_individual, width = 10, height = 6)
}

