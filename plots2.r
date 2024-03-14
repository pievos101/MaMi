
DATASETS = c("IRIS","ECOLI","YEAST","IONOSPHERE",
"WINE","PARKINSON", "GLASS", "SONAR", "WDBC")
K = c(1,2,3,5,10,50)
D_ALL = NULL

for (xx in 1:length(DATASETS)){
    for(yy in 1:length(K)){
        IN = paste(DATASETS[xx],"_",K[yy],".txt", sep="")    
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
  xlab("k2-nearest neighbors") +  
  #theme_bw() +
  theme_minimal()  + 
  theme(text = element_text(size=12)) +
  ylim(-0.025,0.025) +
  facet_wrap(~factor(data))
