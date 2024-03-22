#
source("mami.r")
library(mlbench)

data(iris)
DATA = iris[,-5]
labels = as.numeric(iris[,5])

# train test split
    ids = sample(1:nrow(DATA), ceiling(0.8*nrow(DATA)))
    train = DATA[ids,]
    test  = DATA[-ids,]
    train_labels = labels[ids]
    test_labels = labels[-ids]
    rownames(train) = 1:nrow(train)
    rownames(test) = (nrow(train)+1):(nrow(train)+nrow(test))


res = mami(train, test, train_labels, k1=5, k2=10)

library(caret)
#ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
#knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit <- train(x=train, y=as.factor(train_labels), 
                method = "knn",
                preProcess =  c("center","scale"),
                tuneGrid=data.frame(k=5))
knnPredict <- predict(knnFit, newdata = test, type = "prob")