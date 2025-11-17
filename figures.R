source("themes.R")
source("data.R")

library(patchwork)

#PCA ########################################################

pca_elbow_plot(X, center = T, scale. = T)
ggsave("Figures/PCA-elbow.png", width = 6, height = 5, dpi = 600)

#How many components should we use according to a scree plot?
max(which(eigen(cor(X))$values >= 1))

#KMEANS #####################################################

source("kmeans.R")

kg <- kmeans_clusters(X.pca[, 1:13], 3, Bstart = 10)
kg.df <- data.frame(
  pc1 = X.pca[, 1],
  pc2 = X.pca[, 2],
  labels = factor(kg)
)

kmeans.plot <- ggplot(kg.df, aes(x = pc1, y = pc2, color = labels)) +
  geom_point(size = 0.5) +
  scale_color_paper(guide = "none") +
  labs(
    x = expression(PC[1]),
    y = expression(PC[2])
  ) +
  theme_paper(text_size = 64)
ggsave("Figures/kmeans.png", kmeans.plot, width = 5, height = 4, dpi = 600)

# Genre Classification #######################################

source("cluster_genres.R")

genre.cat <- p1 + p3

ggsave(
  "Figures/cluster_genre_heatmaps.png",
  genre.cat,
  width = 10,
  height = 4,
  dpi = 600
)
