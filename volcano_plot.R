volcano_plot <- function(results) {
  padj_min <- min(results$padj, na.rm = TRUE)
  y_scale <- 10^-(0:as.integer(-log10(padj_min)+1))
  
  if (length(y_scale) > 10) {
    y_scale <- seq(from = -log10(y_scale[1]), 
                   to = -log10(y_scale[length(y_scale)]), 
                   length.out = 10)
  }
  
  results %>%
    mutate(color = case_when(
      padj < 0.05 & log2FoldChange >= log2(1.5) ~ "A",
      padj < 0.05 & log2FoldChange <= -log2(1.5) ~ "B",
      TRUE ~ "C"
    )) %>%
    ggplot(aes(x = log2FoldChange, y = -log10(padj), color = color)) +
    geom_point(show.legend = FALSE) +
    geom_hline(aes(yintercept = -log10(0.05)), linetype = "dashed") +
    geom_vline(aes(xintercept = 0)) +
    scale_color_manual(values = c("green", "red", "black")) +
    theme_bw() +
    theme(panel.grid = element_blank()) +
    labs(x = "Fold Change (log2)", y = "P value") +
    scale_y_continuous(breaks = y_scale,
                       labels = 10^-y_scale,
                       expand = c(0, 0))
}
