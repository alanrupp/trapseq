expressed_genes <- function(counts, threshold = 1, samples = 3) {
    counts %>%
    apply(., 2, function(x) x / sum(x) * 10^6) %>%
    apply(., 2, function(x) x >= threshold) %>%
    .[rowSums(.) >= samples, ] %>%
    rownames(.)
}