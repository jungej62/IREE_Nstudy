#Functions for analysis
#Function to id aonr, and then report the yield based on aonr
aonr.out<-function(mod, yldat, simnum){
  outdat<-data.frame("Run"=1:simnum, "AONR"=NA, "EstYld"=NA)
  for(i in 1:simnum){
    modtmp<-update(mod, data=yldat[sample(1:nrow(yldat),nrow(yldat), rep=T),])
    b0<-fixef(modtmp)[1]
    b1<-fixef(modtmp)[2]
    b2<-fixef(modtmp)[3]
    outdat[i,2] <- (-b1)/(2*b2)
outdat[i,3]<-predict(modtmp, data.frame(Nfert=(-b1)/(2*b2), location=NA, seedyld=NA), level=0)
  }
print(data.frame("AONR"=mean(outdat$AONR), "UpAONR"=quantile(outdat$AONR, 0.025), "LowAONR"=quantile(outdat$AONR, 0.975),
                 "EstYld"=mean(outdat$EstYld), "UpEstYld"=quantile(outdat$EstYld, 0.025), "LowEstYld"=quantile(outdat$EstYld, 0.975)))
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