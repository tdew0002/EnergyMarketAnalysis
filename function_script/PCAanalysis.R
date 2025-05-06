run_pca_umap_clusters_animated <- function(unit_level_df, start_time, end_time, k = 4, show_labels = FALSE) {
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(factoextra)
  library(umap)
  library(gganimate)
  library(patchwork)
  
  # 1. Filter by time
  df <- unit_level_df %>%
    filter(timestamp_30m >= as.POSIXct(start_time),
           timestamp_30m <= as.POSIXct(end_time))
  
  # 2. Prepare numeric data
  df_num <- df %>%
    select(-timestamp_30m, -REGIONID, -DUID) %>%
    select(where(is.numeric))
  
  log_safe <- function(x) if (any(x < 0, na.rm = TRUE)) x else log1p(x)
  df_num <- df_num[, colSums(is.na(df_num)) < nrow(df_num)]
  df_num <- as.data.frame(lapply(df_num, log_safe))
  df_num <- df_num[, apply(df_num, 2, function(x) sd(x, na.rm = TRUE) > 0)]
  df_num <- as.data.frame(lapply(df_num, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  
  df_scaled <- scale(df_num)
  
  # 3. PCA
  pca <- prcomp(df_scaled)
  pca_scores <- as.data.frame(pca$x[, 1:2])
  pca_scores$timestamp <- df$timestamp_30m
  
  # 4. K-Means Clustering
  km <- kmeans(df_scaled, centers = k)
  pca_scores$cluster <- as.factor(km$cluster)
  
  # 5. Static PCA Plot
  p_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = 0.8) +
    labs(title = paste("Clustered PCA (", start_time, "to", end_time, ")"), color = "Cluster") +
    theme_minimal(base_size = 13)
  
  if (show_labels) {
    p_pca <- p_pca + geom_text_repel(aes(label = format(timestamp, "%b %d\n%H:%M")), size = 2.5)
  }
  
  # 6. PCA Biplot (top 6 variables)
  top_vars <- head(order(rowSums(abs(pca$rotation[, 1:2])), decreasing = TRUE), 6)
  var_df <- as.data.frame(pca$rotation[top_vars, 1:2])
  var_df$var <- rownames(var_df)
  
  p_biplot <- p_pca +
    geom_segment(data = var_df,
                 aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
                 arrow = arrow(length = unit(0.2, "cm")),
                 inherit.aes = FALSE, color = "black", linewidth = 0.8) +
    geom_text(data = var_df,
              aes(x = PC1 * 5, y = PC2 * 5, label = var),
              inherit.aes = FALSE, size = 3.5, fontface = "bold", hjust = 0)
  
  # 7. UMAP
  umap_res <- umap(df_scaled)
  umap_df <- as.data.frame(umap_res$layout)
  colnames(umap_df) <- c("UMAP1", "UMAP2")
  umap_df$timestamp <- df$timestamp_30m
  umap_df$cluster <- as.factor(km$cluster)
  
  p_umap <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
    geom_point() +
    labs(title = "UMAP Cluster Projection", color = "Cluster") +
    theme_minimal(base_size = 13)
  
  if (show_labels) {
    p_umap <- p_umap + geom_text_repel(aes(label = format(timestamp, "%b %d\n%H:%M")), size = 2.5)
  }
  
  # 8. Animated PCA
  pca_anim <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    labs(title = "PCA Cluster Animation: {frame_time}",
         subtitle = "Bidding Behaviour over Time",
         x = "PC1", y = "PC2", color = "Cluster") +
    transition_time(timestamp) +
    ease_aes('linear') +
    theme_minimal(base_size = 13)
  
  # 9. Animated UMAP
  umap_anim <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = cluster)) +
    geom_point(size = 2, alpha = 0.8) +
    labs(title = "UMAP Cluster Animation: {frame_time}",
         subtitle = "Projection of Bidding Patterns",
         x = "UMAP1", y = "UMAP2", color = "Cluster") +
    transition_time(timestamp) +
    ease_aes('linear') +
    theme_minimal(base_size = 13)
  
  # Show all static plots in layout
  print((p_pca / p_biplot / p_umap) +
          plot_annotation(
            title = paste("Clustered Behaviour from", start_time, "to", end_time),
            theme = theme(plot.title = element_text(size = 16, face = "bold"))
          ))
  
  return(list(
    pca_plot = p_pca,
    biplot = p_biplot,
    umap_plot = p_umap,
    pca_animation = pca_anim,
    umap_animation = umap_anim,
    scores = pca_scores,
    umap = umap_df,
    kmeans = km
  ))
}
