source("data.R")
source("calinski.R")

library(boot)

kmeans_clusters <- function(X, k, Bstart = 50, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  fit <- kmeans(X, centers = k, nstart = Bstart, iter.max = 100)
  fit$cluster
}

kmeans_ch <- function(X, k.grid = 3:7, Bstart = 50, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ch_vec <- sapply(k.grid, function(k) {
    g <- kmeans_clusters(X, k, Bstart = Bstart)
    ch.score(X, g)
  })

  data.frame(k = k.grid, CH = ch_vec)
}

kmeans_jaccard_boot <- function(
  X,
  k,
  R = 200,
  Bstart = 50,
  seed = NULL
) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(X)
  cl0 <- kmeans_clusters(X, k, Bstart = Bstart)
  boot_fun <- function(data, indices) {
    idx <- indices
    Xb <- data[idx, , drop = FALSE]

    cl_boot <- kmeans_clusters(Xb, k, Bstart = Bstart)

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

kmeans.summary <- function(X, iters = 500, seed = 666) {
  kmeans_ch_summary <- kmeans_ch(X, k.grid, Bstart = 10, seed = 666)
  print(kmeans_ch_summary)

  kmeans_stab <- lapply(k.grid, function(k) {
    kmeans_jaccard_boot(
      X,
      k = k,
      R = iters,
      Bstart = 10,
      seed = seed
    )
  })
  names(kmeans_stab) <- paste0("k", k.grid)

  kmeans_stab_summary <- data.frame(
    k = k.grid,
    mean_gamma = sapply(kmeans_stab, \(z) z$mean_gamma),
    mean_gamma_mcse = sapply(kmeans_stab, \(z) z$mean_gamma_mcse)
  )

  print(kmeans_stab_summary)
}

kmeans.summary(X = X.pca[, 1:13], iters = 500)

# k       CH
# 1 3 187.6944
# 2 4 151.8520
# 3 5 134.3061
# 4 6 124.2679
# 5 7 115.2718
#    k mean_gamma mean_gamma_mcse
# k3 3  0.9059998     0.002375117
# k4 4  0.6883734     0.005286859
# k5 5  0.6472300     0.004920278
# k6 6  0.6775243     0.005006445
# k7 7  0.6394362     0.004726481
