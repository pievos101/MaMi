#
source("../mami.r")
source("../mami_weighted.r")
source("../boundary.r")
library(mlbench)

source("/home/bastian/GitHub/MaMi/get_dataset.r")

DATASET = c("HEART")

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


# train test split
    ids = sample(1:nrow(DATA), ceiling(0.8*nrow(DATA)))
    train = DATA[ids,]
    test  = DATA[-ids,]
    train_labels = labels[ids]
    test_labels = labels[-ids]
    rownames(train) = 1:nrow(train)
    rownames(test) = (nrow(train)+1):(nrow(train)+nrow(test))


res = mami(train, test, train_labels, k1=3, k2=5)
res2 = mami_weighted(train, test, train_labels, k1=3, k2=5)



library(caret)
#ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
#knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit <- train(x=train, y=as.factor(train_labels), 
                method = "knn",
                preProcess =  c("center","scale"),
                tuneGrid=data.frame(k=5))
knnPredict <- predict(knnFit, newdata = test, type = "prob")