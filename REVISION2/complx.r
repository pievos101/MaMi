# tmp 

k1 = 3
k2 = 3

d = 20
n = 500
N = seq(1000, 10000, by=1000)

res_kNN = numeric(length(N))
res_mami = numeric(length(N))


for(xx in 1:length(N)){

n = N[xx]
res_kNN[xx]  = k1*log(n)*d
res_mami[xx] = k1*log(n)*d + k1*k2*log(n)

}

maxval = max(c(res_kNN, res_mami))
minval = min(c(res_kNN, res_mami))


plot(res_kNN, type="l", ylim=c(minval, maxval), ylab="Speed", xlab="Sample Size")
lines(res_mami, type="l", col="red")
axis(1, 1:length(N), N)

plot(res_mami/res_kNN, type="l", ylab="Speed loss", xlab="Sample Size")
axis(1, 1:length(N), N)
