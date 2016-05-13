predat<-data.frame(Nfertnew=c(0:145))
predat$rep<-NA
predat$location<-NA
predat$seedyld<-predict(tlidatsy4_mod3, predat, level=0)

ggplot(summarySE(tlidatsy4, measurevar="seedyld", groupvars=c("location", "Nfertnew"), na.rm=T), 
       aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point(alpha=.5)+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  geom_line(data=predat, color='black')+
  xlim(0,200)+
  ylim(0,1200)+
  xlab(expression("N fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))+
  theme(plot.title=element_text(size=10,face='bold', hjust=.02),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        #legend.title=element_blank(),
        #legend.position=c(.85,.23),
        #legend.key=element_rect(fill='white', colour='white'),
        legend.key.size=unit(.5, 'cm'),
        legend.text=element_text(size=10),
        axis.line = element_line(color='black'),
        axis.text.x=element_text(size=10, color='black'),
        axis.text.y=element_text(size=10, color='black'),
        panel.grid.minor=element_blank(), panel.grid.major=element_blank())

#Here was an attempt to fit the quadratic plateau model to the data. However, the data only followed this pattern for two site
#years, and we know that yields decline with increasing N, so it's more accurate to use the quadratic model instead. 
#Quadratic plateau model with random effects                
tlidatny2_mod8<-nlme(seedyld ~ qpmod(Nfertnew, alpha, beta, gamma),
                     data=data1,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location,
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     #control=nlmeControl(pnlsTol = 10, verbose=T),
                     na.action=na.omit)
ranef(tlidatny2_mod8)
summary(tlidatny2_mod8)
fixef(tlidatny2_mod8)
confint(tlidatny2_mod8, "gamma", level=0.9)[[1]][2]-confint(tlidatny2_mod8, "gamma", level=0.9)[[1]][1] 

#Re-parametarized quadratic for AONR estimation with no random effect.
tlidatny2_mod3<-nls(seedyld ~ alpha - ((2*gamma*beta)*Nfertnew)+(beta*Nfertnew^2), data=tlidatny2, 
                    start = list(alpha = 250, beta = -0.1, gamma = 100),
                    control = list(maxiter=200))

#Trying a nls with random effects. Should produce the same output as mod3. Just uses function call.
tlidatny2_mod4<-nls(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                    data=tlidatny2,
                    start=list(alpha=600, beta=-0.1, gamma=100))
summary(tlidatny2_mod4)

#grouping data to use mixed effects model for accounting random effects.
data1<-tlidatny2[,c(2,3,4,11,12)]
data1<-groupedData(seedyld~Nfertnew|location/rep/plot, data=data1,
                   labels=list(x='Nitrogen rate', y="Grain yield"),
                   units=list(x="(kg2/ha)", y="(kg2/ha)"))

#Trying nlme's auto fitter. This works and produces a unique result
#However, there is no good or easy reparametarization to get AONR
#The equation is Y=Asym+(R0-Asym)exp(-exp(lrc)*X)
tlidatny2_mod5<-nlme(seedyld ~ SSasymp(Nfertnew, Asym, R0, lrc),
                     fixed=Asym+R0+lrc~1,
                     random=Asym~1|location,
                     start = c(Asym = 103, R0 = -8.5, lrc = -3.3),
                     data=data1,
                     na.action=na.omit)
sum((summary(tlidatny3_mod1)$residuals)^2)
sum((summary(tlidatny3_mod2)$residuals)^2)
sum((summary(tlidatny3_mod3)$residuals)^2)

# #Quadtratic plateau for south year 4
# tlidatsy4_mod4<-nlme(seedyld ~ qpmod(Nfertnew, alpha, beta, gamma),
#                       data=sy4dat,
#                       fixed=alpha+beta+gamma~1,
#                       random=alpha~1|location/rep,
#                       method="ML",
#                       start=c(alpha=10, beta=-0.01, gamma=100),
#                       control=nlmeControl(pnlsTol = 0.1, verbose=T),
#                       na.action=na.omit)
# AICc(tlidatsy4_mod4)

tlidatny3_mod3

nlsboot.aonr2<-function(mod, yldat, simnum){ #mod is model, yldat is the name of dataset used, simnum is number of simulations
  outdat<-data.frame("Run"=1:simnum, "AONR"=NA, "EstYld"=NA, "AONR-Dif"=NA)
  for(i in 1:simnum){
    modtmp<-update(mod, data=yldat[sample(1:nrow(yldat),nrow(yldat), rep=T),]) #to speed up, change lme to lmer
    outdat[i,2] <-fixef(modtmp)[3]
    outdat[i,4]<-fixef(modtmp)[3]-fixef(mod)[3]
    outdat[i,3]<-predict(modtmp, data.frame(Nfertnew=outdat[i,2], location=NA, seedyld=NA), level=0)
  }
  hist(outdat[,4])
  print(data.frame("AONR"=mean(outdat$AONR), "LowAONR"=quantile(outdat$AONR, 0.025), "UpAONR"=quantile(outdat$AONR, 0.975),
                   "EstYld"=mean(outdat$EstYld), "LowEstYld"=quantile(outdat$EstYld, 0.025), "UpEstYld"=quantile(outdat$EstYld, 0.975)))
}

nlsboot.aonr2(tlidatny3_mod3, ny3dat, 100)



#####################Here was a test to compare bootstrap methods. Compared my function to the boot() function and 
#boot.ci() function. My produces similar results. 
#Range
bootaonr_sy2_mod3$UpAONR-bootaonr_sy2_mod3$LowAONR
#Bootbca. This is a new function that keeps the vector of AONRs and predicted values from AONRs in a seperate list object
boot.bcax<-boot.bca(tlidatsy2_mod3, sy2dat, 100)
mean(boot.bcax$AONR)
quantile(boot.bcax$AONR, 0.025)
quantile(results$t, 0.975)

boot.bcax2<-boot.bca(tlidatsy2_mod3, sy2dat, 1000)
# Bootstrap 95% CI for R-Squared
library(boot)
# function to obtain R-Squared from the data 
rsq <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
              data=d,
              fixed=alpha+beta+gamma~1,
              random=alpha~1|location/rep,
              method="ML",
              control=nlmeControl(pnlsTol = 0.1, verbose=T, maxIter=1000),
              start=c(alpha=700, beta=-0.01, gamma=100),
              na.action=na.omit)
  return(fixef(fit)[3])
} 
# bootstrapping with 1000 replications 
results <- boot(data=sy2dat, statistic=rsq, 
                R=100)

# view results
results$t
quantile(results$t, 0.025)
quantile(results$t, 0.975)
plot(results)

# get 95% confidence interval 
boot.ci(results, type="bca")
boot.ci(results, type="perc")


auto.aonr<-function(reg, yr, crp, comp){
  if(reg=="North"){
    tpdat<-Ndat
  }else{
    tpdat<-Sdat
  }
  tp2dat<-subset(tpdat, Age==yr&var==crp)
  names(tp2dat)[which(names(tp2dat)==comp)]<-"response"
  m1<-lme(response~Nfertnew, random=~1|location/rep, data=tp2dat, na.action=na.omit, method="ML")
  #m1<-lme(grassyld~Nfertnew, random=~1|location/rep, data=tp2dat, na.action=na.omit, method="ML")
  summary(m1)
  grdat<-tp2dat[,c("location","plot","rep","Nfertnew","response")]
  summary(lm(response~Nfertnew, data=grdat))
  print(head(grdat))
}
auto.aonr("North",2,"TLI","hi")

summary(lm(hi~Nfertnew, data=subset(Ndat, Age==2&var=="TLI")))
