# Call make_classication from Python 

# Load the reticulate package
library(reticulate)

# Use a specific Python environment (optional)
# use_virtualenv("myenv") or use_python("/usr/bin/python3")

# Import Python modules
sklearn <- import("sklearn.datasets")

n_iter = 30

RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("MaMi","kNN","wKNN")



for(xx in 1:n_iter){

    # Create dataset with proper integer parameters
    dataset <- sklearn$make_classification(
    n_samples = as.integer(1000),
    n_features = as.integer(20),
    n_informative = as.integer(5),
    n_redundant = as.integer(2),
    n_classes = as.integer(2),
    weights = list(0.5, 0.5),
    flip_y = 0,
    random_state = as.integer(42)
    )

    # Convert output to R
    X <- dataset[[1]]
    y <- dataset[[2]] + 1 # HACK!

    # Turn into data frame
    data <- as.matrix(X)
    target <- as.numeric(y)

    NN = paste("V", 1:ncol(data), sep="")
    colnames(data) = NN
    names(target) = NN

    # Train-test split 

    ## 80% of the sample size
    smp_size <- floor(0.80 * nrow(data))

    train_ind <- sample(seq_len(nrow(data)), size = smp_size)

    train <- data[train_ind, ]
    test  <- data[-train_ind, ]

    train_labels = target[train_ind]
    test_labels  = target[-train_ind]

    library(caret)
    library(ModelMetrics)
    library(MLmetrics)

    probs = TRUE

    # Call the Major-Minority algorithm
    source("~/GitHub/MaMi/mami.r")
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

}