#
source("../mami.r")
source("../mami_weighted.r")

source("../mami_crossval.r")
source("../get_dataset.r")
library(mlbench)
library(ModelMetrics)
library(MLmetrics)


IN = c("IRIS","ECOLI","YEAST","IONOSPHERE",
"WINE","PARKINSON", "HEART", "SONAR", "WDBC")

IN = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")


for(DATASET in IN){

#DATASET = "IONOSPHERE"

res = get_dataset(DATASET)
DATA  = res$train
labels = res$target
t = table(labels)
ids = which(t<=5)
LL = as.numeric(names(t[ids]))
ids = !is.element(labels, LL)
DATA = DATA[ids,]
labels = labels[ids]
labels = as.numeric(as.factor(labels))
n_iter = 50 


if(DATASET=="PARKINSON"){
    DATA = DATA[,-2]
}


# distance type
method = c("euclidean", "manhattan", "canberra", "minkowski")

probs = TRUE

for(kk in 1:length(method)){

RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("MaMi","wMAMI","diff")

for(xx in 1:n_iter){
  
    print(DATASET)
    # train test split
    ids = sample(1:nrow(DATA), ceiling(0.8*nrow(DATA)))
    train = DATA[ids,]
    test  = DATA[-ids,]
    train_labels = labels[ids]
    test_labels = labels[-ids]
    if(length(setdiff(unique(train_labels), unique(test_labels)))!=0){
        #print("Problem")
        #xx = xx - 1
        next
    }
    rownames(train) = 1:nrow(train)
    rownames(test) = (nrow(train)+1):(nrow(train)+nrow(test))


    if(FALSE){
        # swap 
        labs = unique(train_labels)
        c1 = which(train_labels==labs[1])
        c2 = which(train_labels==labs[2])
        
        L = min(c(length(c1),length(c2)))

        # Calculate how many samples to take
        n_s <- ceiling(L * 0.25)

        # Sample without replacement
        s1 <- sample(c1, size = n_s, replace = FALSE)
        s2 <- sample(c2, size = n_s, replace = FALSE)

        # Swap
        train_labels[s1] = labs[2]
        train_labels[s2] = labs[1]
    }


    #res = mami_crossval(train, test, train_labels)
    #print(res$k1);print(res$k2);
    res = mami(train, test, train_labels, k1=3, k2=5, 
                distance=method[kk], scaling=TRUE)
    pred   = res$prediction
    pred2  = res$coverage
    #print("MAMI ------------------------")
    #print(pred2)
    colnames(pred2) = sort(unique(train_labels))
    #ids = which(pred==0)
    #pred2[ids] = 1-pred2[ids]
    library(aricode)
    library(pROC)
    if(probs){
        MAMI_perf = multiclass.roc(test_labels, pred2)$auc[1]
    }else{
        pred2[pred2!=0] = 1
        MAMI_perf = multiclass.roc(test_labels, pred2)$auc[1]
       # MAMI_perf = MLmetrics::F1_Score(pred, test_labels)
    }
    #ARI(pred, test_labels)

    ## WEIGHTED MAMI
    #res = mami_crossval(train, test, train_labels)
    #print(res$k1);print(res$k2);
    res = mami_weighted(train, test, train_labels, k1=3, k2=5, 
                    distance=method[kk], scaling=TRUE)
    pred   = res$prediction
    pred2  = res$coverage
    #print("MAMI ------------------------")
    #print(pred2)
    colnames(pred2) = sort(unique(train_labels))
    #ids = which(pred==0)
    #pred2[ids] = 1-pred2[ids]
    library(aricode)
    library(pROC)
    if(probs){
        MAMI_perf_w = multiclass.roc(test_labels, pred2)$auc[1]
    }else{
        pred2[pred2!=0] = 1
        MAMI_perf_w = multiclass.roc(test_labels, pred2)$auc[1]
       # MAMI_perf = MLmetrics::F1_Score(pred, test_labels)
    }
    #ARI(pred, test_labels)

RES[xx,1] = MAMI_perf
RES[xx,2] = MAMI_perf_w
RES[xx,3] = MAMI_perf - MAMI_perf_w

print(RES)

}
IN = paste(DATASET,"_AUC_dist_",method[kk],".txt", sep="")
write.table(RES, file=IN)
}
}
