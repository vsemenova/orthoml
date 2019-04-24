# Object of class dml
# Methods:
# summary
# predict,coef,tidy
dml<-function(res,names,...){
  value<-list( b.hat = res$b.hat, st.error.hat = res$st.error.hat, names = names)
  attr(value,'class')<-'dml'
  value
}

tidy.dml<-function(m,measures=c("Estimate","Std.Error","t value")) {
  res<-list()
  res$Estimate<-m$b.hat
  res$Std.Error<-m$st.error.hat
  res$`t value`<-m$b.hat/m$st.error.hat
  res$`Pr(>|t|)`<-(1-pnorm(abs(m$b.hat/m$st.error.hat)) )*2
  res<-do.call(cbind,res)
  res<-res[,measures]
  rownames(res)<-m$names
  return(as.matrix(res))
}
summary.dml<-function(m,measures=c("Estimate","Std.Error","t value")) {
  res<-list()
  res$Estimate<-m$b.hat
  res$Std.Error<-m$st.error.hat
  res$`t value`<-m$b.hat/m$st.error.hat
  res$`Pr(>|t|)`<-(1-pnorm(abs(m$b.hat/m$st.error.hat)) )*2
  res<-do.call(cbind,res)
  res<-res[,measures]
  rownames(res)<-m$names
  return(as.matrix(res))
}
predict.dml<-function(m,newdata) {
  res<-as.matrix(newdata)%*%m$b.hat
} 
coef.dml<-function(m) {
  res<-m$b.hat
}