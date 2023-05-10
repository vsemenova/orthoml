library(hdm)
# Fixed Effect Recovery in  Approximately Sparse Correlated Random Coefficients Model

directoryname<-"/Users/virasemenova/Dropbox (MIT)/MSR/OrthoML/MS1670_All_files/Code/"

set.seed(1)
N = 20
M = runif(N)
xi = 1/seq(1:N)^2
a = M+ xi


f = model.matrix(~-1+factor(1:N))-1/N
ctr = list(numIter = 3, tol = 10^-5, threshold = NULL)
a.lasso = rlasso(a ~ -1+M+f, post=FALSE, control=ctr)

a.hat = predict(a.lasso)
M.ind=sort(M,index.return=T)$ix

pdf(paste0(directoryname,"/Figures/FixedEffectRecovery.pdf"), width =6, height = 4)
plot(M[M.ind], a[M.ind], type="p", pch=16,  col=1, xlab="M", ylab="a", cex=.8)
points(M[M.ind], a.hat[M.ind], type="p", col=4, pch=18)
dev.off()
