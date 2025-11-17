
library(randomForest)
library(nnet)
library(xgboost)
library(tidyverse)

supervised_cv_all <- function(datasets, label_col = "label", n_folds = 10, seed = 666) {
  set.seed(seed)
  ks <- names(datasets)
  
  results <- data.frame(
    k = as.numeric(gsub("k", "", ks)),
    Random_Forest = NA,
    Multinomial_Logistic = NA,
    XGBoost = NA
  )
  
  for (ki in seq_along(ks)) {
    data <- datasets[[ki]]
    y <- factor(data[[label_col]])
    X <- data[, setdiff(names(data), label_col)]
    n <- nrow(X)
    folds <- sample(rep(1:n_folds, length.out = n))
    
    rf_mis <- numeric(n_folds)
    multinom_mis <- numeric(n_folds)
    xgb_mis <- numeric(n_folds)
    
    for (fold in 1:n_folds) {
      train_idx <- which(folds != fold)
      test_idx  <- which(folds == fold)
      
      X_train <- X[train_idx, ]
      X_test  <- X[test_idx, ]
      y_train <- y[train_idx]
      y_test  <- y[test_idx]
      
      # Random Forest
      rf_model <- randomForest(x = X_train, y = y_train, ntree = 500)
      rf_pred <- predict(rf_model, X_test)
      rf_mis[fold] <- mean(rf_pred != y_test)
      
      # Multinomial Logistic Regression
      multinom_model <- multinom(y_train ~ ., data = X_train, trace = FALSE)
      multinom_pred <- predict(multinom_model, X_test, type = "class")
      multinom_mis[fold] <- mean(multinom_pred != y_test)
      
      # XGBoost
      y_train_num <- as.numeric(y_train) - 1
      y_test_num <- as.numeric(y_test) - 1
      dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_num)
      dtest  <- xgb.DMatrix(data = as.matrix(X_test), label = y_test_num)
      params <- list(objective = "multi:softmax", num_class = length(unique(y)), eval_metric = "merror")
      xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
      xgb_pred <- predict(xgb_model, dtest)
      xgb_mis[fold] <- mean(xgb_pred != y_test_num)
    }
    
    results$Random_Forest[ki] <- mean(rf_mis)
    results$Multinomial_Logistic[ki] <- mean(multinom_mis)
    results$XGBoost[ki] <- mean(xgb_mis)
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

results_all <- supervised_cv_all(datasets, label_col = "label", n_folds = 10)
results_all




supervised_cv_confidence <- function(datasets, label_col = "label", n_folds = 10, seed = 666) {
  set.seed(seed)
  ks <- names(datasets)
  
  results <- data.frame(
    k = as.numeric(gsub("k", "", ks)),
    Random_Forest = NA,
    Multinomial_Logistic = NA,
    XGBoost = NA
  )
  
  for (ki in seq_along(ks)) {
    data <- datasets[[ki]]
    y <- factor(data[[label_col]])
    X <- data[, setdiff(names(data), label_col)]
    n <- nrow(X)
    folds <- sample(rep(1:n_folds, length.out = n))
    
    rf_conf <- numeric(n_folds)
    multinom_conf <- numeric(n_folds)
    xgb_conf <- numeric(n_folds)
    
    for (fold in 1:n_folds) {
      train_idx <- which(folds != fold)
      test_idx  <- which(folds == fold)
      
      X_train <- X[train_idx, ]
      X_test  <- X[test_idx, ]
      y_train <- y[train_idx]
      y_test  <- y[test_idx]
      
      # Random Forest
      rf_model <- randomForest(x = X_train, y = y_train, ntree = 500)
      rf_pred_prob <- predict(rf_model, X_test, type = "prob")
      rf_conf[fold] <- mean(apply(rf_pred_prob, 1, max))
      
      # Multinomial Logistic Regression
      multinom_model <- nnet::multinom(y_train ~ ., data = X_train, trace = FALSE)
      multinom_pred_prob <- predict(multinom_model, X_test, type = "probs")
      multinom_conf[fold] <- mean(apply(multinom_pred_prob, 1, max))
      
      # XGBoost
      y_train_num <- as.numeric(y_train) - 1
      y_test_num <- as.numeric(y_test) - 1
      dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train_num)
      dtest  <- xgb.DMatrix(data = as.matrix(X_test), label = y_test_num)
      params <- list(objective = "multi:softprob", num_class = length(unique(y)), eval_metric = "mlogloss")
      xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
      xgb_pred_prob <- matrix(predict(xgb_model, dtest), ncol = length(unique(y)), byrow = TRUE)
      xgb_conf[fold] <- mean(apply(xgb_pred_prob, 1, max))
    }
    
    results$Random_Forest[ki] <- mean(rf_conf)
    results$Multinomial_Logistic[ki] <- mean(multinom_conf)
    results$XGBoost[ki] <- mean(xgb_conf)
  }
  
  return(results)
}

# Example usage
results_all_conf <- supervised_cv_confidence(datasets, label_col = "label", n_folds = 5)
results_all_conf



