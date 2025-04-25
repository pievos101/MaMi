

### DISTANCE TYPE AUC DIFFERENCE - TWO NOISE LEVELS

DATASETS = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")
#DATASETS = c("ECOLI","GLASS","YEAST")
#DATASETS = c("IRIS","ECOLI","YEAST","IONOSPHERE",
#"WINE","PARKINSON", "HEART", "SONAR", "WDBC")

K = c("euclidean", "manhattan", "canberra", "minkowski")

D_ALL1 = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_AUC_dist_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL1 = rbind(D_ALL1, D)
    }
    
}

D_ALL2 = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_AUC_dist_noise_25_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL2 = rbind(D_ALL2, D)
    }
    
}

D_ALL1 = cbind("0", D_ALL1)
D_ALL2 = cbind("0.25", D_ALL2)


colnames(D_ALL1) = c("noise","data","k","MaMi","wMAMI","AUCdiff")
colnames(D_ALL2) = c("noise","data","k","MaMi","wMAMI","AUCdiff")


DATA = rbind(D_ALL1, D_ALL2)
colnames(DATA) = c("noise","data","k","MaMi","wMAMI","AUCdiff")
DATA = as.data.frame(DATA)
DATA$MaMi = as.numeric(DATA$MaMi)
DATA$wMAMI = as.numeric(DATA$wMAMI)
DATA$k = as.factor(DATA$k)
DATA$noise = as.factor(DATA$noise)


DATA = DATA[,c(1,2,3,6)] # Just PLOT DIFF

library(reshape)
library(ggplot2)

#DATA = melt(DATA)
colnames(DATA) = c("noise","data","Method","value")


p <- ggplot(DATA, aes(x=Method, y=value, fill=noise)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  #geom_boxplot()+
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("ROC-AUC Difference (MaMi - wMAMI)") +
  xlab("Distance Method") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=13)) +
  #theme(legend.position="none",text = element_text(size=12)) +
  #ylim(-0.025,0.025) +
  facet_wrap(~factor(data)) +
  coord_flip()




### DISTANCE TYPE AUC DIFFERENCE

DATASETS = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")
#DATASETS = c("ECOLI","GLASS","YEAST")
#DATASETS = c("IRIS","ECOLI","YEAST","IONOSPHERE",
#"WINE","PARKINSON", "HEART", "SONAR", "WDBC")

K = c("euclidean", "manhattan", "canberra", "minkowski")

D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_AUC_dist_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","k","MaMi","wMAMI","AUCdiff")
DATA = as.data.frame(DATA)
DATA$MaMi = as.numeric(DATA$MaMi)
DATA$wMAMI = as.numeric(DATA$wMAMI)
DATA$k = as.factor(DATA$k)

DATA = DATA[,c(1,2,5)] # Just PLOT DIFF

library(reshape)
library(ggplot2)

DATA = melt(DATA)
colnames(DATA) = c("data","k","Method","value")


p <- ggplot(DATA, aes(x=k, y=value, fill=Method)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  #geom_boxplot()+
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("ROC-AUC Difference (MaMi - wMAMI)") +
  xlab("Distance Method") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(legend.position="none",text = element_text(size=12)) +
  #ylim(-0.025,0.025) +
  facet_wrap(~factor(data)) +
  coord_flip()


### DISTANCE TYPE

DATASETS = c("IONOSPHERE","PARKINSON", "HEART", "SONAR")
#DATASETS = c("ECOLI","GLASS","YEAST")
K = c("euclidean", "manhattan", "canberra", "minkowski")

D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_AUC_dist_",K[yy],".txt", sep="")    
        D = read.table(IN)
        #D = D[,1] - D[,2]
        D = cbind(DATASETS[xx],K[yy], D)
        D_ALL = rbind(D_ALL, D)
    }
    
}

DATA = D_ALL
colnames(DATA) = c("data","k","MaMi","wMAMI")
DATA = as.data.frame(DATA)
DATA$MaMi = as.numeric(DATA$MaMi)
DATA$wMAMI = as.numeric(DATA$wMAMI)
DATA$k = as.factor(DATA$k)
library(reshape)
library(ggplot2)

DATA = melt(DATA)
colnames(DATA) = c("data","k","Method","value")


p <- ggplot(DATA, aes(x=k, y=value, fill=Method)) +
  geom_boxplot(outlier.shape = NA) +
  #geom_boxplot()+
  #geom_hline(yintercept=0, linetype="dashed", color = "red", size=1) +
  ylab("ROC-AUC") +
  xlab("Distance Method") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  #ylim(-0.025,0.025) +
  facet_wrap(~factor(data)) +
  coord_flip()


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
