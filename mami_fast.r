mami_fast <- function(train, test, train_labels, k1 = 5, k2 = 10, 
                    distance = "euclidean", scaling = TRUE, dmatrix = NULL) {

  n_train <- nrow(train)
  n_test <- nrow(test)
  total <- n_train + n_test
  
  # Compute distance matrix if not supplied
  if (is.null(dmatrix)) {
    D <- rbind(train, test)
    if (scaling) D <- scale(D)
    D1 <- as.matrix(dist(D, method = distance))
  } else {
    D1 <- dmatrix
  }

  # === First-layer neighbors (vectorized) ===
  test_idx <- (n_train + 1):total
  dists_test <- D1[test_idx, 1:n_train, drop = FALSE]

  # Get indices of k1 nearest neighbors for each test point
  K1 <- t(apply(dists_test, 1, function(x) order(x)[1:k1]))
  K1_l <- matrix(train_labels[K1], nrow = n_test)

  # === Second-layer neighbors ===
  # Build all second-layer neighborhoods for each K1 neighbor
  get_K2_labels <- function(idx_vec) {
    apply(matrix(idx_vec, ncol = 1), 1, function(idx) {
      order(D1[idx, 1:n_train])[1:k2]
    })
  }

  # Returns list of matrices (k1 × k2) of labels
  K2_labels <- lapply(seq_len(n_test), function(i) {
    neighbors <- K1[i, ]
    idx_mat <- get_K2_labels(neighbors)
    matrix(train_labels[idx_mat], nrow = k1, byrow = FALSE)
  })

  # === Compute coverage score (vectorized where possible) ===
  C_list <- lapply(seq_len(n_test), function(i) {
    as.integer(K2_labels[[i]] == K1_l[i, ])
  })

  # === Compute normalized coverage vectors ===
  COV_ALL <- lapply(seq_len(n_test), function(i) {
    lbls <- K1_l[i, ]
    counts <- table(lbls)
    class_names <- names(counts)
    cov_vec <- numeric(length(counts))
    names(cov_vec) <- class_names
    for (j in seq_along(class_names)) {
      mask <- lbls == as.integer(class_names[j])
      cov_vec[j] <- sum(C_list[[i]][mask]) / (k1 * k2)
    }
    cov_vec
  })

  # === Predict: class with max coverage ===
  PRED <- as.integer(sapply(COV_ALL, function(x) names(which.max(x))))

  # === Convert to matrix of per-class coverage ===
  class_labels <- sort(unique(train_labels))
  COV <- matrix(0, n_test, length(class_labels))
  colnames(COV) <- as.character(class_labels)

  for (i in seq_len(n_test)) {
    matched <- names(COV_ALL[[i]])
    COV[i, matched] <- COV_ALL[[i]]
  }

  return(list(prediction = PRED, coverage = COV))
}