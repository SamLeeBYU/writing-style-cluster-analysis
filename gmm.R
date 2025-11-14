source("data.R")
source("calinski.R")

library(mclust)

gmm_clusters <- function(X, k) {
  fit <- Mclust(X, G = k, modelNames = "VVI")
  fit$classification
}

gmm_ch <- function(X, k.grid = 3:7, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ch_vec <- sapply(k.grid, function(k) {
    g <- gmm_clusters(X, k) #sample(1:k, nrow(X), replace = T)
    ch.score(X, g)
  })

  data.frame(k = k.grid, CH = ch_vec)
}

gmm_jaccard_boot <- function(X, k, R = 200, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(X)
  cl0 <- gmm_clusters(X, k)

  boot_fun <- function(data, indices) {
    idx <- indices
    Xb <- data[idx, , drop = FALSE]

    cl_boot <- gmm_clusters(Xb, k)

    idx_star <- sort(unique(idx))
    cl0_star <- cl0[idx_star]

    first_pos <- tapply(seq_along(idx), idx, `[`, 1)
    pos_star <- as.integer(first_pos[as.character(idx_star)])
    clb_star <- cl_boot[pos_star]

    gamma_C <- numeric(k)

    for (c in seq_len(k)) {
      C_pts <- idx_star[cl0_star == c]
      if (length(C_pts) == 0L) {
        gamma_C[c] <- 0
      } else {
        jacc_c <- sapply(seq_len(k), function(d) {
          D_pts <- idx_star[clb_star == d]
          if (length(D_pts) == 0L) {
            return(0)
          }
          inter <- length(intersect(C_pts, D_pts))
          union <- length(union(C_pts, D_pts))
          if (union == 0L) 0 else inter / union
        })
        gamma_C[c] <- max(jacc_c)
      }
    }

    gamma_C
  }

  b <- boot(
    data = X,
    statistic = boot_fun,
    R = R,
    parallel = "multicore"
  )

  gamma_bar <- colMeans(b$t)
  gamma_mcse <- apply(b$t, 2, sd) / sqrt(R)

  mean_gamma <- mean(gamma_bar)
  mean_gamma_mcse <- sd(rowMeans(b$t)) / sqrt(R)

  list(
    gamma_bar = gamma_bar,
    gamma_mcse = gamma_mcse,
    mean_gamma = mean_gamma,
    mean_gamma_mcse = mean_gamma_mcse,
    boot_mat = b$t,
    boot_obj = b
  )
}

k.grid <- 3:7

gmm.summary <- function(X, iters = 500, seed = 666) {
  gmm_ch_summary <- gmm_ch(X, k.grid, seed = seed)
  print(gmm_ch_summary)
  gmm_stab <- lapply(k.grid, function(k) {
    gmm_jaccard_boot(X, k = k, R = iters, seed = seed)
  })
  names(gmm_stab) <- paste0("k", k.grid)

  gmm_stab_summary <- data.frame(
    k = k.grid,
    mean_gamma = sapply(gmm_stab, \(z) z$mean_gamma),
    mean_gamma_mcse = sapply(gmm_stab, \(z) z$mean_gamma_mcse)
  )
  print(gmm_stab_summary)
}

gmm.summary(X = X.pca[, 1:13], 500)
#  k       CH
# 1 3 65.27188
# 2 4 65.08038
# 3 5 87.18404
# 4 6 85.52907
# 5 7 73.02151
#   k mean_gamma mean_gamma_mcse
# k3 3  0.5604275     0.005184017
# k4 4  0.5876058     0.006601812
# k5 5  0.6217609     0.004314503
# k6 6  0.5483159     0.004132778
# k7 7  0.4651430     0.004417613
