#
source("mami.r")
source("mami_crossval.r")
source("get_dataset.r")
library(mlbench)

res = get_dataset("WDBC")
DATA  = res$train
labels = res$target
t = table(labels)
ids = which(t<=5)
LL = as.numeric(names(t[ids]))
ids = !is.element(labels, LL)
DATA = DATA[ids,]
labels = labels[ids]

n_iter = 100 
RES = matrix(NaN, n_iter, 2)

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

    #res = mami_crossval(train, test, train_labels)
    #print(res$k1);print(res$k2);
    res = mami(train, test, train_labels, k1=5, k2=50)
    pred   = res$prediction
    pred2  = res$coverage
    colnames(pred2) = sort(unique(train_labels))
    #ids = which(pred==0)
    #pred2[ids] = 1-pred2[ids]
    library(aricode)
    library(pROC)
    MAMI_perf = multiclass.roc(test_labels, pred2)$auc[1]
    #ARI(pred, test_labels)

    # Now with caret
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "knn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(k=5))
    knnPredict <- predict(knnFit, newdata = test, type = "prob")
    #knnPredict <- knnPredict[,2]
    colnames(knnPredict) = sort(unique(train_labels))
    KNN_perf = multiclass.roc(test_labels, knnPredict)$auc[1] #ARI(knnPredict, test_labels)


RES[xx,1] = MAMI_perf
RES[xx,2] = KNN_perf
print(RES)
}

colnames(RES) = c("MaMi","kNN")
boxplot(RES, col="cadetblue", ylab="Adjusted R-index")