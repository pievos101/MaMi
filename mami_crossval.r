# cross validation
mami_crossval <- function(train, test, train_labels,
                            k1=c(3,4,5,10), k2=c(3,4,5,10)){


SCORES = numeric(length(k1)*length(k2))
K1_val = numeric(length(k1)*length(k2))
K2_val = numeric(length(k1)*length(k2))

count = 1
for(xx in 1:length(k1)){
    for(yy in 1:length(k2)){

        res   = mami(train, test, train_labels, k1=k1[xx], k2=k2[yy])
        score = sum(res$coverage)
        #print("----------------")
        #print(score)
        #print("----------------")
        SCORES[count] = score
        K1_val[count] = k1[xx]
        K2_val[count] = k2[yy] 
        count = count + 1     
    }
}
id = which.max(rev(SCORES))

return(list(k1=rev(K1_val)[id], k2=rev(K2_val)[id]))

}