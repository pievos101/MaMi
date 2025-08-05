library(ggplot2)
library(reshape)

zero = read.table("Noise_00.txt")
one = read.table("Noise_01.txt")
two = read.table("Noise_02.txt")
three = read.table("Noise_03.txt")
#four = read.table("Noise_04.txt")
#five = read.table("Noise_05.txt")



L = list()
L[[1]] = zero
L[[2]] = one
L[[3]] = two
L[[4]] = three
#L[[5]] = four
#L[[6]] = five

library(reshape)
library(ggplot2)

L_melt = melt(L)
colnames(L_melt) = c("Method","value","signal")
L_melt$signal = factor(L_melt$signal, 
		labels=c("0","0.1","0.2","0.3"))#,"0.4","0.5"))

p1 = ggplot(L_melt, aes(x = signal, y = value, fill = Method)) +
  geom_boxplot(outlier.shape = NA, outlier.fill = "white", outlier.color = "black") +
  theme_minimal(base_size = 14) +
  #scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Classification performance",
    y = "ROC-AUC",
    x = "Noise (fraction of randomly assigned samples)"
  ) #+
  #theme(legend.position = "none")

print(p1)

###############################################################
#### TIME ####################

library(ggplot2)
library(reshape)

zero = read.table("Time3_500.txt")
one = read.table("Time3_1000.txt")
two = read.table("Time3_5000.txt")
three = read.table("Time3_10000.txt")
four = read.table("Time3_20000.txt")


L = list()
L[[1]] = zero
L[[2]] = one
L[[3]] = two
L[[4]] = three
L[[5]] = four
#L[[6]] = five

library(reshape)
library(ggplot2)

L_melt = melt(L)
colnames(L_melt) = c("Method","value","signal")
L_melt$signal = factor(L_melt$signal, 
		labels=c("500","1000","5000","10000","20000"))#,"0.4","0.5"))

p1 = ggplot(L_melt, aes(x = signal, y = value, fill = Method)) +
  geom_boxplot(outlier.shape = NA, outlier.fill = "white", outlier.color = "black") +
  theme_minimal(base_size = 14) +
  #scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Varying sample size and k2-neighborhood",
    y = "Time elapsed in seconds",
    x = "Sample size"
  ) #+
  #theme(legend.position = "none")

print(p1)

#############################################################
#############################################################

###############################################################
#### TIME ####################

library(ggplot2)
library(reshape)

zero = read.table("Time3_500.txt")
one = read.table("Time3_1000.txt")
two = read.table("Time3_5000.txt")
three = read.table("Time3_10000.txt")
four = read.table("Time3_20000.txt")

#five = read.table("Noise_05.txt")
zero = zero/mean(zero$kNN) - 1
one = one/mean(one$kNN) -1
two = two/mean(two$kNN)-1
three = three/mean(three$kNN)-1
four = four/mean(four$kNN)-1


L = list()
L[[1]] = zero
L[[2]] = one
L[[3]] = two
L[[4]] = three
L[[5]] = four
#L[[6]] = five

library(reshape)
library(ggplot2)

L_melt = melt(L)
colnames(L_melt) = c("Method","value","signal")
L_melt$signal = factor(L_melt$signal, 
		labels=c("500","1000","5000","10000","20000"))#,"0.4","0.5"))

p1 = ggplot(L_melt, aes(x = signal, y = value, fill = Method)) +
  geom_boxplot(outlier.shape = NA, outlier.fill = "white", outlier.color = "black") +
  theme_minimal(base_size = 14) +
  #scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Varying sample size and k2-neighborhood",
    y = "Computational performance loss compared to kNN",
    x = "Sample size"
  ) #+
  #theme(legend.position = "none")

print(p1)