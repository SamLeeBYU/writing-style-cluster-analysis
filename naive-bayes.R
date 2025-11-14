source("data.R")
source("calinski.R")

#NOTE: DEFUNCT DO NOT USE

unsup_nb_gaussian <- function(
  X,
  K,
  maxit = 200,
  tol = 1e-6,
  seed = 1
) {
  set.seed(seed)
  X <- as.matrix(X)
  n <- nrow(X)
  p <- ncol(X)

  ## --- Initialization via k-means ---
  km <- kmeans(X, centers = K, nstart = 20)
  labels <- km$cluster
  pi_k <- tabulate(labels, K) / n
  mu <- km$centers

  ## Correct sigma2 initialization (K × p)
  sigma2 <- t(sapply(1:K, function(j) {
    apply(X[labels == j, , drop = FALSE], 2, var)
  }))
  sigma2 <- pmax(sigma2, 1e-6)

  ## --- Correct mixture log-likelihood ---
  loglik_fun <- function(pi_k, mu, sigma2) {
    logphi <- sapply(1:K, function(j) {
      X.j <- X[,]
      -0.5 *
        rowSums(
          log(2 * pi * sigma2[j, ]) +
            ((X - matrix(mu[j, ], n, p, TRUE))^2) /
              matrix(sigma2[j, ], n, p, TRUE)
        ) +
        log(pi_k[j])
    })
    m <- apply(logphi, 1, max)
    sum(m + log(rowSums(exp(logphi - m))))
  }

  prev <- loglik_fun(pi_k, mu, sigma2)

  ## --- EM algorithm ---
  for (it in 1:maxit) {
    ## E-step: responsibilities
    logphi <- sapply(1:K, function(j) {
      -0.5 *
        rowSums(
          log(2 * pi * sigma2[j, ]) +
            ((X - matrix(mu[j, ], n, p, TRUE))^2) /
              matrix(sigma2[j, ], n, p, TRUE)
        ) +
        log(pi_k[j])
    })

    m <- apply(logphi, 1, max)
    r <- exp(logphi - m)
    r <- r / rowSums(r)

    ## M-step
    Nk <- colSums(r) + 1e-12
    pi_k <- Nk / n
    mu <- (t(r) %*% X) / Nk

    for (j in 1:K) {
      diff <- X - matrix(mu[j, ], n, p, TRUE)
      sigma2[j, ] <- colSums(r[, j] * diff^2) / Nk[j]
      sigma2[j, ] <- pmax(sigma2[j, ], 1e-6)
    }

    ## Evaluate new log-likelihood
    cur <- loglik_fun(pi_k, mu, sigma2)

    ## Check convergence
    if (abs(cur - prev) < tol * (1 + abs(prev))) {
      prev <- cur
      break
    }
    prev <- cur
  }

  ## Return final quantities
  list(
    prior = pi_k,
    mu = mu,
    sigma2 = sigma2,
    resp = r,
    labels = max.col(r),
    loglik = prev,
    it = it
  )
}

nb_clusters <- function(X, k) {
  fit <- unsup_nb_gaussian(X, K = k)
  fit$label
}

D <- rbind(
  MASS::mvrnorm(n = 30, mu = rep(0, 3), Sigma = diag(3)),
  MASS::mvrnorm(n = 30, mu = rep(1, 3), Sigma = diag(3)),
  MASS::mvrnorm(n = 30, mu = rep(2, 3), Sigma = diag(3))
)

nb_ch <- function(X, k.grid = 3:7, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ch_vec <- sapply(k.grid, function(k) {
    g <- nb_clusters(X, k) #sample(1:k, nrow(X), replace = T)
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
