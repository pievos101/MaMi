mami_weighted <- function(train, test, train_labels, k1=5, k2=10){

D  = rbind(train, test)
D1 = as.matrix(dist(scale(D)))
#D1 = as.matrix(dist(D))

# Calculate first layer neigborhood for each test point
K1 = matrix(NaN,nrow(test),k1)
K1_l = matrix(NaN,nrow(test),k1)
K1_dist = matrix(NaN,nrow(test),k1)

count = 1 
for (xx in (nrow(train)+1):(nrow(train)+nrow(test))){

    vec = D1[xx,1:nrow(train)]
    vec_s = sort(vec)
    K1_l[count,] = train_labels[as.numeric(names(head(vec_s,k1)))]
    K1[count,] = as.numeric(names(head(vec_s,k1)))
    K1_dist[count,] = head(vec_s,k1)
    
    count = count + 1
    #print(vec_s)

}
#print(K1_l[1,])
#print(K1_dist[1,])

#print(K1)

# Calculate Second layer Neighborhood for each point 
K2 = matrix(NaN, k1, k2)
K2_dist = matrix(NaN, k1, k2)

K2_list = list()
K2_list_dist = list()

for(xx in 1:nrow(K1)){
    for(yy in 1:k1){
        vec = D1[K1[xx,yy],1:nrow(train)]
        vec_s = sort(vec)
        K2[yy,] = train_labels[as.numeric(names(head(vec_s,k2)))]
        K2_dist[yy,] = head(vec_s,k2)
    }
  K2_list[[xx]] = K2
  K2_list_dist[[xx]] = K2_dist
  
  K2 = matrix(NaN, k1, k2)
  K2_dist = matrix(NaN, k1, k2)
  
}
#print(K2_list[[1]])
#print(K2_list_dist[[1]])

C_list = list()
C_list_sim = list()
C = rep(NaN, k1)
C_sim = rep(NaN, k1)

# Avoid division by zero
epsilon <- 1e-8
#weights <- 1 / (dists + epsilon)

# Cac Coverage
for(xx in 1:length(K2_list)){
    for(yy in 1:k1){
        agree = K2_list[[xx]][yy,]==K1_l[xx,yy]
        C[yy] = sum(agree) 
        C_sim[yy] = mean(1/(K2_list_dist[[xx]][yy,][agree]+1))
    }
    C_list[[xx]] = C
    C_list_sim[[xx]] = C_sim*C
    C_sim = rep(NaN, k1)
    C = rep(NaN, k1)
    
}
#print(C_list[[1]])
#print(C_list_sim[[1]])


COV_ALL = list()
COV_ALL_sim = list()


# Now infer the prediction
for(xx in 1:nrow(K1_l)){
    l1 = table(K1_l[xx,])
    maj = as.numeric(names(l1)[1])
    COV = rep(NaN, length(l1))
    COV_sim = rep(NaN, length(l1))
    names(COV) = names(l1)
    names(COV_sim) = names(l1)
    
    for(yy in 1:length(l1)){
        ids = which(K1_l[xx,] == as.numeric(names(l1[yy])))
        #ids = which(K1_l[xx,] == maj)
        #COV[yy] = sum(C_list[[xx]][ids])/(length(ids)*k2)
        COV[yy] = sum(C_list[[xx]][ids])/(k1*k2)
        COV_sim[yy] = sum(C_list_sim[[xx]][ids])
    }

    #print(COV)
  COV_ALL[[xx]] = COV 
  COV_ALL_sim[[xx]] = COV_sim 
  
}

#print(COV_ALL_sim[[1]])

PRED = as.numeric(sapply(COV_ALL_sim, function(x){names(which.max(x))}))

COV = matrix(0, length(PRED), length(unique(train_labels)))
for(xx in 1:dim(COV)[1]){

    ids = as.numeric(names(COV_ALL_sim[[xx]])) #+ 1
    #print(ids)
    #print(COV_ALL[[xx]])
    COV[xx, ids] = COV_ALL_sim[[xx]]

}


#COV  = as.numeric(sapply(COV_ALL, function(x){max(x)}))

# NORMALIZE
#COV <- t(apply(COV, 1, function(row) row / sum(row)))

return(list(prediction=PRED, coverage=COV))

}