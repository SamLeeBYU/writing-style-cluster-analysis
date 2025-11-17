source("kmeans.R")

#Use X.pca[,1:13]
X <- X.pca[,1:13]

#Compute ps for each value of k

#For each k,
# Do a train test split (or you could also do KF-validation)
# Determine cluster membership using k clusters and using kmeans clustering on the training data set
# "Predict" cluster membership via nearest centroid for X test

#use kmeans_clusters to create models on the training data

# Then, for each cluster j,
# Loop over each cluster in X test
# Compute the ps(k) by computing D (which is the indicator) for the corresponding predicted clusters:
# Sum up how many of the cases we "predicted" correctly
# Normalize by dividing by 1/n(n-1)

# ps(k) = minimum of those normalized sums above

power_score <- function(datasets, label_col = "label", n_folds = 10, seed = 666, Bstart = 100) {
  set.seed(seed)
  ks <- names(datasets)
  
  results <- data.frame(
    k = as.numeric(gsub("k", "", ks)),
    PS = NA
  )
  
  power_score <- function(X_train, X_test, y_test, k, Bstart) {
    km_fit <- kmeans(X_train, centers = k, nstart = Bstart)
    centroids <- km_fit$centers
    train_clusters <- km_fit$cluster
    
    predict_clusters <- function(x) {
      dists <- apply(centroids, 1, function(centroid) sum((x - centroid)^2))
      which.min(dists)
    }
    
    test_clusters <- apply(X_test, 1, predict_clusters)
    
    ps_per_cluster <- numeric(k)
    for (j in 1:k) {
      idx <- which(test_clusters == j)
      if (length(idx) < 2) {
        ps_per_cluster[j] <- 0
      } else {
        D <- outer(as.numeric(y_test[idx]), as.numeric(y_test[idx]), FUN = function(a, b) as.numeric(a == b))
        ps_per_cluster[j] <- sum(D) / (length(idx) * (length(idx) - 1))
      }
    }
    
    PS <- min(ps_per_cluster)
    return(PS)
  }
  
  for (ki in seq_along(ks)) {
    data <- datasets[[ki]]
    y <- factor(data[[label_col]])
    X <- data[, setdiff(names(data), label_col)]
    n <- nrow(X)
    folds <- sample(rep(1:n_folds, length.out = n))
    
    ps_folds <- numeric(n_folds)
    
    for (fold in 1:n_folds) {
      train_idx <- which(folds != fold)
      test_idx  <- which(folds == fold)
      
      X_train <- X[train_idx, ]
      X_test  <- X[test_idx, ]
      y_train <- y[train_idx]
      y_test  <- y[test_idx]
      
      k <- results$k[ki]
      ps_folds[fold] <- power_score(X_train, X_test, y_test, k, Bstart)
    }
    
    results$PS[ki] <- mean(ps_folds)
  }
  
  return(results)
}

datasets <- list(
  k3 = data_k3,
  k4 = data_k4,
  k5 = data_k5,
  k6 = data_k6,
  k7 = data_k7
)

results_power_score <- power_score(datasets, label_col = "label", n_folds = 1000)
results_power_score

cbind(results_all, PS = results_power_score$PS)


