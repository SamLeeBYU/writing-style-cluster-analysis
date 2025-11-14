source("themes.R")
source("data.R")

#PCA ########################################################

pca_elbow_plot(X, center=T, scale.=T)
ggsave("Figures/PCA-elbow.png", width = 6, height = 5, dpi = 600)

#KMEANS #####################################################

source("kmeans.R")

kg <- kmeans_clusters(X.pca[,1:13], 3, Bstart = 10)
kg.df <- data.frame(
  pc1 = X8[,1],
  pc2 = X8[,2],
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
