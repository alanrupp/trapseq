pair_plot <- function(gene, info, color = NULL, shape = NULL) {
  info <- select(info, Sample_ID, pair, cells, quo(color), quo(shape))
  
  p <- fpm[gene, ] %>%
    as.data.frame() %>%
    set_names("fpm") %>%
    rownames_to_column("Sample_ID") %>%
    left_join(., select(info, Sample_ID, diet, treatment, cells, pair),
              by = "Sample_ID") %>%
    ggplot(aes(x = cells, y = fpm, group = pair)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(trans = "log2")
  # custom color aesthetic
  if (!is.null(color)) {
    p <- p + 
      geom_line(aes(color = !!color)) + 
      geom_point(aes(color = !!color))
  }
  # custom shape aesthetic
  if (!is.null(shape)) {
    p <- p + geom_line(aes(shape = shape)) + 
      geom_point(aes(shape = shape))
  }
  return(p)
}