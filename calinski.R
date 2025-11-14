#Computes the Calinski-Harabasz Score
ch.score <- function(X, G) {
  #X is nxk data matrix
  #G is a vector of group indices corresponding to which groups
  #each row belongs in

  K <- unique(G)

  n <- nrow(X)
  n.j <- tapply(as.data.frame(X), G, nrow)
  mu.j <- tapply(as.data.frame(X), G, colMeans)
  mu <- colMeans(X)

  B.k <- sapply(K, function(k) {
    n.j[k] * crossprod(mu - mu.j[[k]])
  }) |>
    sum()

  W.k <- sapply(K, function(k) {
    m.j <- mu.j[[k]]
    X.g <- X[G == k, ]
    s.g <- apply(X.g, 1, function(x) {
      crossprod(x - m.j)
    }) |>
      sum()
  }) |>
    sum()

  ch.k <- (B.k / (length(K) - 1)) / (W.k / (n - length(K)))
  ch.k
}
