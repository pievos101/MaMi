#
source("../mami.r")
source("../mami_crossval.r")
source("../get_dataset.r")
library(mlbench)
library(ModelMetrics)
library(MLmetrics)

DATASET = "PARKINSON"
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


if(DATASET=="IONOSPHERE"){
    DATA = DATA[,-2]
}


# swap percentage
noise = c(0, 0.01, 0.05, 0.10, 0.20)
#noise = c(0.20)

probs = TRUE

for(kk in 1:length(noise)){

RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("MaMi","kNN","wKNN")

for(xx in 1:n_iter){
  

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

    if(noise[kk]!=0){
        # swap 
        labs = unique(train_labels)
        c1 = which(train_labels==labs[1])
        c2 = which(train_labels==labs[2])
        
        L = min(c(length(c1),length(c2)))

        # Calculate how many samples to take
        n_s <- ceiling(L * noise[kk])

        # Sample without replacement
        s1 <- sample(c1, size = n_s, replace = FALSE)
        s2 <- sample(c2, size = n_s, replace = FALSE)

        # Swap
        train_labels[s1] = labs[2]
        train_labels[s2] = labs[1]
    }


    #res = mami_crossval(train, test, train_labels)
    #print(res$k1);print(res$k2);
    res = mami(train, test, train_labels, k1=3, k2=5)
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

    # Now with caret
    # NON-WEIGHTED
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "knn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(k=3))
    knnPredict2 <- predict(knnFit, newdata = test, type = "prob")
    knnPredict <- predict(knnFit, newdata = test)
    #print("KNN ------------------------")
    #print(knnPredict2)
    
    #knnPredict <- knnPredict[,2]
    colnames(knnPredict2) = sort(unique(train_labels))
    if(probs){
        KNN_perf = multiclass.roc(test_labels, knnPredict2)$auc[1]
    }else{
        KNN_perf = multiclass.roc(test_labels, knnPredict2)$auc[1]
        #KNN_perf = MLmetrics::F1_Score(knnPredict, test_labels)
    }

    # WEIGHTED
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "kknn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(kmax = 3, distance = 2, kernel = "triangular"))
    knnPredict2 <- predict(knnFit, newdata = test, type = "prob")
    knnPredict <- predict(knnFit, newdata = test)
    #print("KNN ------------------------")
    #print(knnPredict2)
    
    #knnPredict <- knnPredict[,2]
    colnames(knnPredict2) = sort(unique(train_labels))
    if(probs){
        KNN_perf_w = multiclass.roc(test_labels, knnPredict2)$auc[1]
    }else{
        KNN_perf_w = multiclass.roc(test_labels, knnPredict2)$auc[1]
        #KNN_perf = MLmetrics::F1_Score(knnPredict, test_labels)
    }

RES[xx,1] = MAMI_perf
RES[xx,2] = KNN_perf
RES[xx,3] = KNN_perf_w

print(RES)
print(noise[kk])
}
IN = paste(DATASET,"_AUC_noise_",noise[kk],".txt", sep="")
write.table(RES, file=IN)
}

