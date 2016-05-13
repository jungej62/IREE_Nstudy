#Functions for analysis
#Function to id aonr, and then report the yield based on aonr
eonr.out<-function(mod, yldat, simnum, price){ #mod is model, yldat is the name of dataset used, simnum is number of simulations, price is price of kernza in $/lb
  outdat<-data.frame("Run"=1:simnum, "AONR"=NA, "EstYld"=NA)
  for(i in 1:simnum){
    modtmp<-update(mod, data=yldat[sample(1:nrow(yldat),nrow(yldat), rep=T),])
    b0<-fixef(modtmp)[1]
    b1<-fixef(modtmp)[2]
    b2<-fixef(modtmp)[3]
    outdat[i,2] <- ((1.14/(price*2.204))-b1)/(2*b2) #for EONR, input price in dollars per pound
    #outdat[i,2] <- (-b1)/(2*b2) for AONR
outdat[i,3]<-predict(modtmp, data.frame(Nfertnew=outdat[i,2], location=NA, seedyld=NA), level=0)
  }
print(data.frame("AONR"=mean(outdat$AONR), "LowAONR"=quantile(outdat$AONR, 0.025), "UpAONR"=quantile(outdat$AONR, 0.975),
                 "EstYld"=mean(outdat$EstYld), "LowEstYld"=quantile(outdat$EstYld, 0.025), "UpEstYld"=quantile(outdat$EstYld, 0.975)))
}

eonr.out(tlidat13_mod2, tlidat13, 20, 0.25)

aonr.out<-function(mod, yldat, simnum){ #mod is model, yldat is the name of dataset used, simnum is number of simulations
  outdat<-data.frame("Run"=1:simnum, "AONR"=NA, "EstYld"=NA)
  for(i in 1:simnum){
    modtmp<-update(mod, data=yldat[sample(1:nrow(yldat),nrow(yldat), rep=T),]) #to speed up, change lme to lmer
    b0<-fixef(modtmp)[1]
    b1<-fixef(modtmp)[2]
    b2<-fixef(modtmp)[3]
    outdat[i,2] <- (-b1)/(2*b2) 
    outdat[i,3]<-predict(modtmp, data.frame(Nfertnew=outdat[i,2], location=NA, seedyld=NA), level=0)
  }
  print(data.frame("AONR"=mean(outdat$AONR), "LowAONR"=quantile(outdat$AONR, 0.025), "UpAONR"=quantile(outdat$AONR, 0.975),
                   "EstYld"=mean(outdat$EstYld), "LowEstYld"=quantile(outdat$EstYld, 0.025), "UpEstYld"=quantile(outdat$EstYld, 0.975)))
}

aonr.out(tlidat13_mod2, tlidat13, 100)

nlsboot.aonr<-function(mod, yldat, simnum){ #mod is model, yldat is the name of dataset used, simnum is number of simulations
  outdat<-data.frame("Run"=1:simnum, "AONR"=NA, "EstYld"=NA)
  for(i in 1:simnum){
    modtmp<-try(update(mod, data=yldat[sample(1:nrow(yldat),nrow(yldat), rep=T),]), silent=T)
    if(typeof(modtmp)=="character") { #skips over those cases where an error occurs
      outdat[i,]<-NA
      } else{
    outdat[i,2] <-fixef(modtmp)[3]
    outdat[i,3]<-predict(modtmp, data.frame(Nfertnew=outdat[i,2], location=NA, seedyld=NA), level=0)
  }
}
  outdat<-outdat[complete.cases(outdat),] #removes the rows where an error occured
  print(data.frame("AONR"=mean(outdat$AONR), "LowAONR"=quantile(outdat$AONR, 0.025), "UpAONR"=quantile(outdat$AONR, 0.975),
                   "EstYld"=mean(outdat$EstYld), "LowEstYld"=quantile(outdat$EstYld, 0.025), "UpEstYld"=quantile(outdat$EstYld, 0.975),
                   "Successes"=length(outdat$AONR))) #tells us how many times the model rerun was successful.

}

nlsboot.aonr(tlidatny2_mod3,data1,100)
#Here is the function to produce quadratic plateau models
qpmod<-function(x, alpha, beta, gamma){
  ifelse(x < gamma, alpha + beta*x + (-beta/(2*gamma))*x*x,
         alpha + beta*gamma/2)
  #gamma is AONR
}
#Here is the function to produce linear plateau models
linplat<-function(x, alpha, beta, gamma){
  ifelse(x < gamma, alpha + beta*x, 
         alpha+beta*gamma)
}

qmmod<-function(x, alpha, beta, gamma){
  alpha - ((2*gamma*beta)*x)+(beta*x^2)
  #gamma is AONR
}

########Function similar to nlsboot.aonr only it keeps the vectors of AONR and predicted estimates at AONR
boot.bca<-function(mod, yldat, simnum){ #mod is model, yldat is the name of dataset used, simnum is number of simulations
  outdat<-data.frame("Run"=1:simnum, "AONR"=NA, "EstYld"=NA)
  for(i in 1:simnum){
    modtmp<-try(update(mod, data=yldat[sample(1:nrow(yldat),nrow(yldat), rep=T),]), silent=T)
    if(typeof(modtmp)=="character") { #skips over those cases where an error occurs
      outdat[i,]<-NA
    } else{
      outdat[i,2] <-fixef(modtmp)[3]
      outdat[i,3]<-predict(modtmp, data.frame(Nfertnew=outdat[i,2], location=NA, seedyld=NA), level=0)
    }
  }
  outdat<-outdat[complete.cases(outdat),] #removes the rows where an error occured
  
  list(outdat,data.frame("AONR"=mean(outdat$AONR), "LowAONR"=quantile(outdat$AONR, 0.025), "UpAONR"=quantile(outdat$AONR, 0.975),
                         "EstYld"=mean(outdat$EstYld), "LowEstYld"=quantile(outdat$EstYld, 0.025), "UpEstYld"=quantile(outdat$EstYld, 0.975),
                         "Successes"=length(outdat$AONR)))
}