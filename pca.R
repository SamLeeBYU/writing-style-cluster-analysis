pca <- function(X, k = NULL, center = TRUE, scale. = FALSE) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  if (is.null(k)) {
    k = min(n - 1, p)
  }

  if (k > min(n - 1L, p)) {
    stop("k must be <= min(n - 1, p).")
  }

  ## center/scale columns
  Xc <- if (center || scale.) {
    scale(X, center = center, scale = scale.)
  } else {
    X
  }

  ## covariance matrix and spectral decomposition
  S <- crossprod(Xc) / (n - 1L) # p x p covariance
  eig <- eigen(S, symmetric = TRUE)

  ## loadings (eigenvectors) and scores (principal components)
  V <- eig$vectors[, 1:k, drop = FALSE] # p x k
  scores <- Xc %*% V # n x k

  colnames(scores) <- paste0("PC", seq_len(k))
  attr(scores, "eigenvalues") <- eig$values[1:k]

  scores # n x k matrix of principal components
}

pca_elbow_plot <- function(
  X,
  k.star = 13,
  max_k = NULL,
  center = TRUE,
  scale. = FALSE,
  target_lines = c(0.80, 0.90)
) {
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  k_full <- min(n - 1L, p)
  if (is.null(max_k)) {
    max_k <- k_full
  }
  max_k <- min(max_k, k_full)

  pcs <- pca(X, k = k_full, center = center, scale. = scale.)
  eigvals <- attr(pcs, "eigenvalues")

  var_prop <- eigvals / sum(eigvals)
  cum_var_prop <- cumsum(var_prop)

  df <- data.frame(
    k = 1:max_k,
    cum_var = cum_var_prop[1:max_k]
  )

  ggplot(df, aes(k, cum_var, color = "PC")) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    geom_hline(
      data = data.frame(y = target_lines),
      aes(yintercept = y),
      linetype = "dashed",
      linewidth = 0.5,
      color = "grey60"
    ) +
    geom_vline(      
      aes(xintercept = k.star),
      linetype = "dashed",
      linewidth = 0.5,
      color = "grey60"
    )+
    scale_color_paper(guide = "none") +
    scale_x_continuous(
      breaks = seq_len(max_k),
      minor_breaks = NULL
    ) +
    labs(
      x = "Number of principal components",
      y = "Cumulative variance explained",
      # title = "Elbow Plot for Principal Components",
      # subtitle = "Cumulative proportion of variance explained"
    ) +
    theme_paper(text_size = 64)
}
