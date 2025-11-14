source("data.R")
source("calinski.R")

library(boot)

ward_clusters <- function(X, k) {
  hc <- hclust(dist(X), method = "ward.D2")
  cutree(hc, k = k)
}

ward_ch <- function(X, k.grid = 3:7) {
  hc <- hclust(dist(X), method = "ward.D2")
  ch_vec <- sapply(k.grid, function(k) {
    groups <- cutree(hc, k = k)
    ch.score(X, groups)
  })
  data.frame(k = k.grid, CH = ch_vec)
}

ward_jaccard_boot <- function(X, k, R = 200, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(X)
  cl0 <- ward_clusters(X, k)

  boot_fun <- function(data, indices) {
    idx <- indices
    Xb <- data[idx, , drop = FALSE]

    cl_boot <- ward_clusters(Xb, k)

    #original points that appear in bootstrap sample
    idx_star <- sort(unique(idx))

    #labels of those points in original clustering
    cl0_star <- cl0[idx_star]

    #align bootstrap labels to same original indices
    first_pos <- tapply(seq_along(idx), idx, `[`, 1)
    pos_star <- as.integer(first_pos[as.character(idx_star)])
    clb_star <- cl_boot[pos_star]

    #Jaccard per original cluster
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
X8 <- X.pca[, 1:8]

ward_ch_summary <- ward_ch(X8, k.grid)

ward_stab <- lapply(k.grid, function(k) {
  ward_jaccard_boot(X8, k = k, R = 500, seed = 666)
})
names(ward_stab) <- paste0("k", k.grid)

stab_summary <- data.frame(
  k = k.grid,
  mean_gamma = sapply(ward_stab, \(z) z$mean_gamma),
  mean_gamma_mcse = sapply(ward_stab, \(z) z$mean_gamma_mcse)
)

ward_ch_summary
stab_summary
