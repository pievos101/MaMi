#
source("/home/bastian/GitHub/MaMi/mami.r")
source("/home/bastian/GitHub/MaMi/mami_crossval.r")
source("/home/bastian/GitHub/MaMi/get_dataset.r")
library(mlbench)
library(ModelMetrics)
library(MLmetrics)
library(CalibratR)

library(reticulate)
source_python("/home/bastian/GitHub/MaMi/calibration_metric.py")
#source_python("/home/bastian/GitHub/MaMi/calibration_metric_2.py")
#source_python("/home/bastian/GitHub/MaMi/calibration_metric_3.py")

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

if(DATASET=="IONOSPHERE"){
    DATA = DATA[,-2]
}

n_iter = 50 
RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("MaMi","kNN","wKNN")

probs = TRUE
#calibMethod = "BRIER"
setK = c(1,2,3,5,10,50)

for(kk in 1:length(setK)){

RES = matrix(NaN, n_iter, 3)
colnames(RES) = c("MaMi","kNN","wKNN")

for(xx in 1:n_iter){

    cat("K=",setK[kk],sep="","\n")
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
    res = mami(train, test, train_labels, k1=setK[kk], k2=5)
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
        #print(pred2)
        #pred2 = t(apply(pred2, 1, function(row) row / sum(row)))
        ppp = pred2[,2] #apply(pred2, 1, max) 
        ppp2 = pred2
        #MAMI_perf = getECE(test_labels-1, ppp, n_bins=10)
        #MAMI_perf = get_ECE_equal_width(test_labels-1, ppp)
        MAMI_perf = ece(np_array(ppp), np_array(test_labels-1, dtype="int"), 
            mode='l1')
        #MAMI_perf = ModelMetrics::brier(actual=test_labels-1, predicted=ppp)
        #MAMI_perf = tce_multiclass(np_array(ppp), np_array(test_labels-1, dtype="int"))
        #print(MAMI_perf)
        #MAMI_perf = tce(np_array(ppp), np_array(test_labels-1, dtype="int"))
        #print(MAMI_perf)
        #, as.integer(10), as.character('l2'))
        #cstat = CalibratR::reliability_diagramm(test_labels-1, ppp)
        #MAMI_perf = cstat$calibration_error$brier_class_0
        #LAB_MATRIX = matrix(0, length(test_labels), 2)
        #LAB_MATRIX[(test_labels-1)==1, 2] = 1
        #LAB_MATRIX[(test_labels-1)==0, 1] = 1
        #MAMI_perf = classwise_ECE(np_array(LAB_MATRIX), np_array(pred2))
        #ece = ECE(as.integer(10))
        #MAMI_perf = ece$measure(np_array(ppp2), np_array(test_labels-1))

    #print(MAMI_perf)
    #ARI(pred, test_labels)

    # NON-WEIGHTED KNN
    # Now with caret
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "knn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(k=setK[kk]))
    knnPredict2 <- predict(knnFit, newdata = test, type = "prob")
    knnPredict  <- predict(knnFit, newdata = test)
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
        ppp = as.matrix(knnPredict2)[,2] #apply(knnPredict2, 1, max)
        ppp2 = as.matrix(knnPredict2) 
        #KNN_perf = getECE(test_labels-1, ppp, n_bins=10)
        #KNN_perf = get_ECE_equal_width(test_labels-1, ppp)
        KNN_perf = ece(np_array(ppp), np_array(test_labels-1), mode='l1')
        #KNN_perf = ModelMetrics::brier(actual=test_labels-1, predicted=ppp)
        
        #KNN_perf = tce_multiclass(np_array(ppp), np_array(test_labels-1))
        #KNN_perf = tce(np_array(ppp), np_array(test_labels-1, dtype="int"))
        #cstat = CalibratR::reliability_diagramm(test_labels-1, ppp)
        #KNN_perf = cstat$calibration_error$ 

        #KNN_perf = classwise_ECE(np_array(LAB_MATRIX), np_array(ppp2))
        
        #ece = ECE(as.integer(10))
        #KNN_perf = ece$measure(np_array(ppp2), np_array(test_labels-1))

    # WEIGHTED KNN
    # Now with caret
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "kknn",
                    preProcess =  c("center","scale"),
                    tuneGrid = data.frame(kmax = setK[kk], distance = 2, kernel = "triangular"))
    knnPredict2 <- predict(knnFit, newdata = test, type = "prob")
    knnPredict  <- predict(knnFit, newdata = test)
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
        ppp = as.matrix(knnPredict2)[,2] # apply(knnPredict2, 1, max)
        ppp2 = as.matrix(knnPredict2) # apply(knnPredict2, 1, max)
         
        #KNN_perf_w = getECE(test_labels-1, ppp, n_bins=10)
        #KNN_perf_w = get_ECE_equal_width(test_labels-1, ppp)
        KNN_perf_w = ece(np_array(ppp), np_array(test_labels-1), mode='l1')
        #KNN_perf_w = ModelMetrics::brier(actual=test_labels-1, predicted=ppp) 
        #KNN_perf_w = tce_multiclass(np_array(ppp), np_array(test_labels-1))
        #KNN_perf_w = tce(np_array(ppp), np_array(test_labels-1, dtype="int"))
        #cstat = CalibratR::reliability_diagramm(test_labels-1, ppp)
        #KNN_perf = cstat$calibration_error$          

        #KNN_perf_w = classwise_ECE(np_array(LAB_MATRIX), np_array(ppp2))
       
        #ece = ECE(as.integer(10))
        #KNN_perf_w = ece$measure(np_array(ppp2), np_array(test_labels-1))

RES[xx,1] = MAMI_perf
RES[xx,2] = KNN_perf
RES[xx,3] = KNN_perf_w

print(RES)
}
write.table(RES, file=paste(DATASET,"_ECE3_",setK[kk],".txt",sep=""))
colnames(RES) = c("MaMi","kNN","wkNN")
boxplot(RES, col="cadetblue", ylab="Calib")
}

