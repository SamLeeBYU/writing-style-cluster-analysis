source("data.R")
source("calinski.R")

unsup_nb_bernoulli <- function(
  X,
  K,
  maxit = 200,
  tol = 1e-6,
  laplace = 1.0,
  seed = 1
) {
  # X: n x p matrix of 0/1
  set.seed(seed)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)
  # init via k-means on rows' sums
  z <- matrix(runif(n * K), n, K)
  z <- z / rowSums(z)
  pi_k <- colMeans(z)
  theta <- matrix(runif(K * p, 0.25, 0.75), K, p) # P(X_j=1 | k)

  loglik <- function() {
    llk <- 0
    for (k in 1:K) {
      llk <- llk +
        sum(
          z[, k] *
            (log(pi_k[k]) +
              rowSums(X * log(theta[k, ]) + (1 - X) * log(1 - theta[k, ])))
        )
    }
    llk
  }

  prev <- -Inf
  for (it in 1:maxit) {
    # E-step: responsibilities r_ik
    log_phi <- sapply(1:K, function(k) {
      rowSums(
        X %*%
          diag(log(theta[k, ]), p) +
          (1 - X) %*% diag(log(1 - theta[k, ]), p)
      ) +
        log(pi_k[k])
    })
    m <- apply(log_phi, 1, max)
    r <- exp(log_phi - m)
    r <- r / rowSums(r)

    # M-step
    Nk <- colSums(r) + 1e-12
    pi_k <- as.numeric(Nk / n)
    # Laplace smoothing
    for (k in 1:K) {
      num <- colSums(r[, k] * X) + laplace
      den <- Nk[k] + 2 * laplace
      theta[k, ] <- pmin(pmax(num / den, 1e-6), 1 - 1e-6)
    }

    # convergence
    cur <- loglik()
    if (abs(cur - prev) < tol * (1 + abs(prev))) {
      break
    }
    prev <- cur
  }
  list(
    prior = pi_k,
    theta = theta,
    resp = r,
    labels = max.col(r),
    loglik = prev,
    it = it
  )
}

nb_clusters <- function(X, k) {
  fit <- unsup_nb_bernoulli(X, K = k)
  fit$label
}

nb_ch <- function(X, k.grid = 3:7, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ch_vec <- sapply(k.grid, function(k) {
    g <- nb_clusters(X, k)
    ch.score(X, g)
  })

  data.frame(k = k.grid, CH = ch_vec)
}

nb_jaccard_boot <- function(X, k, R = 200, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  n <- nrow(X)
  cl0 <- nb_clusters(X, k)

  boot_fun <- function(data, indices) {
    idx <- indices
    Xb <- data[idx, , drop = FALSE]

    cl_boot <- nb_clusters(Xb, k)

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
X8 <- X.pca[, 1:8]

nb_ch_summary <- nb_ch(X8, k.grid, seed = 666)

nb_stab <- lapply(k.grid, function(k) {
  nb_jaccard_boot(X8, k = k, R = 500, seed = 666)
})
names(nb_stab) <- paste0("k", k.grid)

nb_stab_summary <- data.frame(
  k = k.grid,
  mean_gamma = sapply(nb_stab, \(z) z$mean_gamma),
  mean_gamma_mcse = sapply(nb_stab, \(z) z$mean_gamma_mcse)
)

nb_ch_summary
nb_stab_summary
