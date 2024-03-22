# plots
D = read.table("IRIS.txt")
ONE = cbind("IRIS",D[,1]-D[,2])
#
D = read.table("ECOLI.txt")
TWO = cbind("ECOLI",D[,1]-D[,2])
#
D = read.table("YEAST.txt")
THREE = cbind("YEAST",D[,1]-D[,2])
#
D = read.table("IONOSPHERE.txt")
FOUR = cbind("IONOSPHERE",D[,1]-D[,2])
#
D = read.table("PARKINSON.txt")
FIVE = cbind("PARKINSON",D[,1]-D[,2])
#
D = read.table("GLASS.txt")
SIX = cbind("GLASS",D[,1]-D[,2])
#
D = read.table("WINE.txt")
SEVEN = cbind("WINE",D[,1]-D[,2])
#
D = read.table("SONAR.txt")
EIGHT = cbind("SONAR",D[,1]-D[,2])


DATA = rbind(ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT)
colnames(DATA) = c("data","value")
DATA = as.data.frame(DATA)
DATA$value = as.numeric(DATA$value)

p <- ggplot(DATA, aes(x=k, y=value, fill=data)) +
  geom_boxplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "red", size=2) +
  ylab("ROC-AUC performance difference (MaMi - kNN)") +
  xlab("Data sets") + 
   
  theme_minimal()  + 