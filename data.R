source("pca.R")

# Read in the data
# Credit: Patric Platts
collins <- read.table(
  "https://tofu.byu.edu/docs/files/stat666/datasets/collins.txt",
  header = TRUE,
  sep = "\t",
  skip = 10, # skip the 10 lines of description before the header
  check.names = FALSE
)

X <- collins[, -c(1, 20:24)]

#PCA on the correlation matrix (this is done by scaling)
X.pca <- pca(X, center=T, scale.=T)
