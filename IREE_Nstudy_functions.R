#Functions for analysis
#Function to id aonr, and then report the yield based on aonr
aonr.out<-function(mod){
  b0<-fixef(mod)[1]
  b1<-fixef(mod)[2]
  b2<-fixef(mod)[3]
  onr<-data.frame(Nfert=(-b1)/(2*b2))
  predatt<-data.frame(Nfert=c(0:200))
  predatt$location<-NA
  predatt$seedyld<-predict(mod, predatt, level=0)
  predatt[as.numeric(round(onr)+1),]
}

#Here is the function to produce quadratic plateau models
qpmod<-function(x, alpha, beta, gamma){
  ifelse(x < gamma, alpha + beta*x + (-beta/(2*gamma))*x*x,
         alpha + beta*gamma/2)
}
#Here is the function to produce linear plateau models
linplat<-function(x, alpha, beta, gamma){
  ifelse(x < gamma, alpha + beta*x, 
         alpha+beta*gamma)
}