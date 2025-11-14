source("themes.R")
source("data.R")

pca_elbow_plot(X)
ggsave("Figures/PCA-elbow.png", width = 6, height = 4, dpi = 600)
