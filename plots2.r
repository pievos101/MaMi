### NOISE

DATASETS = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")
#DATASETS = c("ECOLI","GLASS","YEAST")
K = c(0, 0.01, 0.05, 0.10, 0.20)
D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_AUC_noise_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","k","MaMi","kNN","wKNN")
DATA = as.data.frame(DATA)
DATA$MaMi = as.numeric(DATA$MaMi)
DATA$kNN = as.numeric(DATA$kNN)
DATA$k = as.factor(as.numeric(DATA$k))
library(reshape)
library(ggplot2)

DATA = melt(DATA)
colnames(DATA) = c("data","k","Method","value")


p <- ggplot(DATA, aes(x=k, y=value, fill=Method)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_boxplot()+
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("ROC-AUC") +
  xlab("Fraction of swapped classes (added noise)") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  #ylim(-0.025,0.025) +
  facet_wrap(~factor(data))


### BRIER
DATASETS = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")
DATASETS = c("ECOLI","GLASS","YEAST")
K = c(1,2,3,5,10,50)
D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_AUC_k1_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","k","MaMi","kNN")
DATA = as.data.frame(DATA)
DATA$MaMi = as.numeric(DATA$MaMi)
DATA$kNN = as.numeric(DATA$kNN)
DATA$k = as.factor(as.numeric(DATA$k))
library(reshape)
library(ggplot2)

DATA = melt(DATA)
colnames(DATA) = c("data","k","method","value")

p <- ggplot(DATA, aes(x=k, y=value, fill=method)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("AUC") +
  xlab("k1-nearest neighbors") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  #ylim(-0.025,0.025) +
  facet_wrap(~factor(data))


### ECE ###
DATASETS = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")
K = c(1,2,3,5,10,50)
D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_ECE_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","k","MaMi","kNN")
DATA = as.data.frame(DATA)
DATA$MaMi = as.numeric(DATA$MaMi)
DATA$kNN = as.numeric(DATA$kNN)
DATA$k = as.factor(as.numeric(DATA$k))
library(reshape)
library(ggplot2)

DATA = melt(DATA)
colnames(DATA) = c("data","k","method","value")

p <- ggplot(DATA, aes(x=k, y=value, fill=method)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("Expected Calibration Error (ECE)") +
  xlab("k1-nearest neighbors") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  #ylim(-0.025,0.025) +
  facet_wrap(~factor(data))

####################################################
####################################################
DATASETS = c("IRIS","ECOLI","YEAST","IONOSPHERE",
"WINE","PARKINSON", "HEART", "SONAR", "WDBC")
K = c(1,2,3,5,10,50)
D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"X_",K[yy],".txt", sep="")    
        D = read.table(IN)
        D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","k","value")
DATA = as.data.frame(DATA)
DATA$value = as.numeric(DATA$value)
DATA$k = as.factor(as.numeric(DATA$k))

p <- ggplot(DATA, aes(x=k, y=value)) +
  geom_boxplot(fill="#56B4E9", outlier.shape = NA) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("ROC-AUC performance difference (MaMi - kNN)") +
  xlab("k1-nearest neighbors") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  ylim(-0.025,0.025) +
  facet_wrap(~factor(data))

#####################################################
#####################################################

DATASETS = c("IRIS","ECOLI","YEAST","IONOSPHERE",
"WINE","PARKINSON", "HEART", "SONAR", "WDBC")
K = "X"
D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_",K[yy],".txt", sep="")    
        D = read.table(IN)
        D = D[,1] - D[,2]
        D = cbind(DATASETS[xx], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","value")
DATA = as.data.frame(DATA)
DATA$value = as.numeric(DATA$value)


p <- ggplot(DATA, aes(x=data, y=value)) +
  geom_boxplot(fill="#56B4E9", outlier.shape = NA) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("ROC-AUC performance difference (MaMi - kNN)") +
  xlab("Benchmark Datasets") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  coord_flip()
  #ylim(-0.025,0.025) +
  #facet_wrap(~factor(data))
