

library(randomForest)
library(randomForest)
library(nnet)
library(xgboost)

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













library(ggplot2)

data_k5$category <- cut(
  data_k5$genre,
  breaks = c(0, 3, 6, 7, 9, 15),
  labels = c(
    "Press",
    "Non-press Nonfiction",
    "Biography",
    "Scholarship & Official Documents",
    "Fiction"
  )
)

library(dplyr)

df <- data_k5 %>%
  group_by(category, label) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(category) %>%
  mutate(prop = n / sum(n))


library(ggplot2)

category_colors <- c(
  "Press" = "#1b9e77",
  "Non-press Nonfiction" = "#d95f02",
  "Biography" = "#7570b3",
  "Scholarship & Official Documents" = "#e7298a",
  "Fiction" = "#66a61e"
)

ggplot(df, aes(x = category, y = prop, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ label, nrow = 1) +
  scale_fill_manual(values = category_colors) +
  theme_minimal() +
  labs(
    x = "Genre Category",
    y = "Proportion",
    fill = "Category",
    title = "Proportion of Genre Categories within Each Cluster"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12)
  )




library(ggplot2)

tbl <- table(data_k5$genre, data_k5$label)
df_heat <- as.data.frame(tbl)
colnames(df_heat) <- c("genre", "cluster", "count")

ggplot(df_heat, aes(x = cluster, y = genre, fill = count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(x = "Cluster", y = "Genre", fill = "Count")

