#
source("mami.r")
data(iris)
DATA = iris[,-5]
labels = as.numeric(iris[,5])

n_iter = 50 
RES = matrix(NaN, n_iter, 2)

for(xx in 1:n_iter){

    # train test split
    ids = sample(1:nrow(DATA), ceiling(0.8*nrow(DATA)))
    train = DATA[ids,]
    test  = DATA[-ids,]
    train_labels = labels[ids]
    test_labels = labels[-ids]
    rownames(train) = 1:nrow(train)
    rownames(test) = (nrow(train)+1):(nrow(train)+nrow(test))

    res = mami(train, test, train_labels)
    pred = res$prediction

    library(aricode)
    MAMI_perf = ARI(pred, test_labels)

    # Now with caret
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=train_labels, 
                    method = "knn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(k=5))
    knnPredict <- predict(knnFit,newdata = test)
    KNN_perf = ARI(knnPredict, test_labels)


RES[xx,1] = MAMI_perf
RES[xx,2] = KNN_perf
print(RES)
}

colnames(RES) = c("MaMi","kNN")
boxplot(RES, col="cadetblue", ylab="Adjusted R-index")