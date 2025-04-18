#
source("/home/bastian/GitHub/MaMi/mami.r")
source("/home/bastian/GitHub/MaMi/mami_crossval.r")
source("/home/bastian/GitHub/MaMi/get_dataset.r")
library(mlbench)
library(ModelMetrics)
library(MLmetrics)
library(CalibratR)


library(reticulate)
source_python("/home/bastian/GitHub/MaMi/calibration_metric_2.py")


DATASET = c("PARKINSON")


#wclass = 0


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

probs = TRUE
calibMethod = "BRIER"
setK = c(1,2,3,5,10,50)

for(aa in 1:length(setK)){

RES_0 = matrix(NaN, n_iter, 3)
colnames(RES_0) = c("MaMi","kNN","wKNN")
RES_1 = matrix(NaN, n_iter, 3)
colnames(RES_1) = c("MaMi","kNN","wKNN")

for(xx in 1:n_iter){

    cat("K=",setK[aa],sep="","\n")

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
    res = mami(train, test, train_labels, k1=setK[aa], k2=5)
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
        ppp = pred2[,2] #apply(pred2, 1, max)
        #ppp = rep(NaN, nrow(pred2))
        #for(zz in 1:nrow(pred2)){
        #    if((test_labels[zz])==1){
        #        ppp[zz] = pred2[zz,1] 
        #    }else{
        #        ppp[zz] = pred2[zz,2]
        #    }
        #}

        MAMI_perf = getECE(test_labels-1, ppp)
        cstat = CalibratR::reliability_diagramm(test_labels-1, ppp)
        
        #if(wclass==0){
        MAMI_perf_0 = cstat$calibration_error$brier_class_0 
        #}else{
        MAMI_perf_1 = cstat$calibration_error$brier_class_1     
        #}
       

    #ARI(pred, test_labels)

    # NON-WEIGHTED KNN
    # Now with caret
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "knn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(k=setK[aa]))
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
        ppp = knnPredict2[,2] #apply(pred2, 1, max)
        #ppp = rep(NaN, nrow(pred2))
        #for(zz in 1:nrow(pred2)){
        #    if((test_labels[zz])==1){
        #        ppp[zz] = pred2[zz,1] 
        #    }else{
        #        ppp[zz] = pred2[zz,2]
        #    }
        #}

        KNN_perf = getECE(test_labels-1, ppp)
        cstat = CalibratR::reliability_diagramm(test_labels-1, ppp)
        
        #if(wclass==0){
        KNN_perf_0 = cstat$calibration_error$CLE_class_0 
        #}else{
        KNN_perf_1 = cstat$calibration_error$CLE_class_1     
        #} 
    
    # WEIGHTED KNN
    # Now with caret
    library(caret)
    #ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    #knnFit <- train(Direction ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    knnFit <- train(x=train, y=as.factor(train_labels), 
                    method = "kknn",
                    preProcess =  c("center","scale"),
                    tuneGrid=data.frame(kmax = setK[aa], distance = 2, kernel = "triangular"))
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
        #KNN_perf_w = MLmetrics::F1_Score(knnPredict, test_labels)
    }
        ppp = knnPredict2[,2] #apply(pred2, 1, max)
        #ppp = rep(NaN, nrow(pred2))
        #for(zz in 1:nrow(pred2)){
        #    if((test_labels[zz])==1){
        #        ppp[zz] = pred2[zz,1] 
        #    }else{
        #        ppp[zz] = pred2[zz,2]
        #    }
        #}

        KNN_perf_w = getECE(test_labels-1, ppp)
        cstat = CalibratR::reliability_diagramm(test_labels-1, ppp)
        
        #if(wclass==0){
        KNN_perf_w_0 = cstat$calibration_error$CLE_class_0 
        #}else{
        KNN_perf_w_1 = cstat$calibration_error$CLE_class_1     
        #}             

RES_0[xx,1] = MAMI_perf_0
RES_0[xx,2] = KNN_perf_0
RES_0[xx,3] = KNN_perf_w_0

RES_1[xx,1] = MAMI_perf_1
RES_1[xx,2] = KNN_perf_1
RES_1[xx,3] = KNN_perf_w_1

print(RES_0)
print(RES_1)

}

fname = paste(DATASET,"_",setK[aa],"_CLASS_",0,".txt", sep="")
write.table(RES_0, file=fname)
fname = paste(DATASET,"_",setK[aa],"_CLASS_",1,".txt", sep="")
write.table(RES_1, file=fname)


}

#colnames(RES) = c("MaMi","kNN")
#boxplot(RES, col="cadetblue", ylab="Adjusted R-index")