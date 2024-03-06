mami <- function(train, test, train_labels, k1=5, k2=5){

D  = rbind(train, test)
D1 = as.matrix(dist(scale(D)))


# Calculate first layer neigborhood for each test point
K1 = matrix(NaN,nrow(test),k1)
K1_l = matrix(NaN,nrow(test),k1)
count = 1 
for (xx in (nrow(train)+1):(nrow(train)+nrow(test))){

    vec = D1[xx,1:nrow(train)]
    vec_s = sort(vec)
    K1_l[count,] = train_labels[as.numeric(names(head(vec_s,k1)))]
    K1[count,] = as.numeric(names(head(vec_s,k1)))
    count = count + 1
    #print(vec_s)

}

# Calculate Second layer Neighborhood for each point 
K2 = matrix(NaN, k1, k2)
K2_list = list()

for(xx in 1:nrow(K1)){
    for(yy in 1:k1){
        vec = D1[K1[xx,yy],1:nrow(train)]
        vec_s = sort(vec)
        K2[yy,] = train_labels[as.numeric(names(head(vec_s,k2)))]
    }
  K2_list[[xx]] = K2
  K2 = matrix(NaN, k1, k2)
}

C_list = list()
C = rep(NaN, k1)
# Cac Coverage
for(xx in 1:length(K2_list)){
    for(yy in 1:k1){
        C[yy] = sum(K2_list[[xx]][yy,]==K1_l[xx,yy])         
    }
    C_list[[xx]] = C
    C = rep(NaN, k1)
}

COV_ALL = list()

# Now infer the prediction
for(xx in 1:nrow(K1_l)){
    l1 = table(K1_l[xx,])
    COV = rep(NaN, length(l1))
    names(COV) = names(l1)
    for(yy in 1:length(l1)){
        ids = which(K1_l[xx,] == as.numeric(names(l1[yy])))
        COV[yy] = sum(C_list[[xx]][ids])/(k1*k2)
    }
    #print(COV)
  COV_ALL[[xx]] = COV 
}


PRED = as.numeric(sapply(COV_ALL, function(x){names(which.max(x))}))
COV  = as.numeric(sapply(COV_ALL, function(x){max(x)}))

return(list(prediction=PRED, coverage=COV))

}