my_knn <- function(train, test, train_labels, k = 5, 
                   distance = "euclidean", scaling = TRUE, 
                   weighted = FALSE, dmatrix=NULL) {

  
if(is.null(dmatrix)){

    D  = rbind(train, test)

    if(scaling){
        D1 = as.matrix(dist(scale(D), method=distance))
    }else{
        D1 = as.matrix(dist(D))
    }

}else{
    D1 = dmatrix
}
  
  # Initialize
  N_test = nrow(test)
  K_indices = matrix(NaN, N_test, k)
  K_labels  = matrix(NaN, N_test, k)
  PRED = rep(NA, N_test)
  
  count = 1
  for (i in (nrow(train) + 1):(nrow(train) + nrow(test))) {
    vec = D1[i, 1:nrow(train)]
    vec_s = sort(vec)
    
    idx = as.numeric(names(head(vec_s, k)))
    lbl = train_labels[idx]
    
    K_indices[count, ] <- idx
    K_labels[count, ] <- lbl
    
    if (!weighted) {
      # Unweighted majority vote
      tbl = table(lbl)
      PRED[count] = as.numeric(names(tbl)[which.max(tbl)])
    } else {
      # Weighted vote: 1 / distance
      dists = vec[idx]
      dists[dists == 0] <- 1e-6  # Avoid division by zero
      weights = 1 / dists
      wtbl = tapply(weights, lbl, sum)
      PRED[count] = as.numeric(names(wtbl)[which.max(wtbl)])
    }
    
    count <- count + 1
  }
  
  # Return predictions and optionally the votes or neighbor info
  return(list(prediction = PRED, neighbors = K_indices, labels = K_labels))
}