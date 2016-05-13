#Modelling grain yield responses to N fertilizer 
#Analyzing grain yields in south first.
seed_dats<-subset(Sdat, var!="switch")
mod1<-lme(seedyld~var*Nfertnew*Age, random=~1|fyear/location/rep, data=seed_dats, na.action=na.omit, method="ML")
anova(mod1)

#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

#Subsetting data to analyze TLI variety only
tlidats<-subset(Sdat, var=="TLI")
tlidats_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=tlidats, na.action=na.omit, method="ML")
anova(tlidats_mod1); ##tt<-data.frame(anova(tlidats_mod1))

#Subsetting data to analyze Rush variety
rushdats<-subset(Sdat, var=="rush")
rushdats_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=rushdats, na.action=na.omit, method="ML")
anova(rushdats_mod1) 

#There is a Nfert/age interaction, so we should fit Nfert for each year seperetly.

#Start with subsetting for year 2 (2013) in the South
tlidatsy2<-subset(tlidats, Age=="2")

tlidatsy2_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatsy2, na.action=na.omit, method="ML")
anova(tlidatsy2_mod1)
sem.model.fits(tlidatsy2_mod1)
AICc(tlidatsy2_mod1)
#quadratic
tlidatsy2_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatsy2, na.action=na.omit, method="ML")
anova(tlidatsy2_mod2)
sem.model.fits(tlidatsy2_mod2)
AICc(tlidatsy2_mod2)
#Id best model
selMod(list(tlidatsy2_mod1, tlidatsy2_mod2))
#Quadtratic model is superior
#Reparametarize and bootstrap AONR estimate using maximum likelihood
sy2dat<-tlidatsy2[,c(2,3,4,11,12)]
sy2dat<-groupedData(seedyld~Nfertnew|location/rep/plot, data=sy2dat,
                    labels=list(x='Nitrogen rate', y="Grain yield"),
                    units=list(x="(kg2/ha)", y="(kg2/ha)"))
tlidatsy2_mod3<-nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                     data=sy2dat,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location/rep,
                     method="ML",
                     control=nlmeControl(pnlsTol = 0.1, verbose=T, maxIter=1000),
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
fixef(tlidatsy2_mod3)
#ML CI
confint(tlidatny2_mod3, "gamma", level=0.95)[[1]][2]-confint(tlidatny3_mod3, "gamma", level=0.95)[[1]][1] 

#BootstrapCI
bootaonr_sy2_mod3<-nlsboot.aonr(tlidatsy2_mod3,sy2dat,1000)


#Year 3 in south (2014)
tlidatsy3<-subset(tlidats, Age=="3")

tlidatsy3_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatsy3, na.action=na.omit, method="ML")
anova(tlidatsy3_mod1)
sem.model.fits(tlidatsy3_mod1)
AICc(tlidatsy3_mod1)

#quadratic
tlidatsy3_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatsy3, na.action=na.omit, method="ML")
anova(tlidatsy3_mod2)
sem.model.fits(tlidatsy3_mod2)
AICc(tlidatsy3_mod2)
#Id best model
selMod(list(tlidatsy3_mod1, tlidatsy3_mod2))
#Linear model is superior, but slope is not significant

#Year 4 in south (2015)
tlidatsy4<-subset(tlidats, Age=="4")

tlidatsy4_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatsy4, na.action=na.omit, method="ML")
anova(tlidatsy4_mod1)
sem.model.fits(tlidatsy4_mod1)
AICc(tlidatsy4_mod1)

#quadratic
tlidatsy4_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatsy4, na.action=na.omit, method="ML")
anova(tlidatsy4_mod2)
sem.model.fits(tlidatsy4_mod2)
AICc(tlidatsy4_mod2)

#Reparametarized quadratic
sy4dat<-tlidatsy4[,c(2,3,4,11,12)]
sy4dat<-groupedData(seedyld~Nfertnew|location/rep/plot, data=sy4dat,
                   labels=list(x='Nitrogen rate', y="Grain yield"),
                   units=list(x="(kg2/ha)", y="(kg2/ha)"))
tlidatsy4_mod3<-nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                     data=sy4dat,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location/rep,
                     method="ML",
                     control=nlmeControl(pnlsTol = 0.1, verbose=T),
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
fixef(tlidatsy4_mod3)

#Comparing models
selMod(list(tlidatsy4_mod1, tlidatsy4_mod2, tlidatsy4_mod3))
bootaonr_sy4_mod3<-nlsboot.aonr(tlidatsy4_mod3,sy4dat,1000)


################   Analyzing northern region
seed_datn<-subset(Ndat, var!="switch")
mod1<-lme(seedyld~var*Nfertnew*Age, random=~1|fyear/location/rep, data=seed_datn, na.action=na.omit, method="ML")
anova(mod1)

#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

#Subsetting data to analyze TLI variety only
tlidatn<-subset(Ndat, var=="TLI")
tlidatn_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=tlidatn, na.action=na.omit, method="ML")
anova(tlidatn_mod1); #tt<-data.frame(anova(tlidatn_mod1))

#Subsetting data to analyze Rush variety
rushdatn<-subset(Ndat, var=="rush")
rushdatn_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=rushdatn, na.action=na.omit, method="ML")
anova(rushdatn_mod1) 

#There is a Nfert/age interaction, so maybe we should fit Nfert for each year seperetly.
#Start with subsetting for year 2 (2013)
tlidatny1<-subset(tlidatn, Age=="1")

tlidatny1_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatny1, na.action=na.omit, method="ML")
anova(tlidatny1_mod1)
sem.model.fits(tlidatny1_mod1)
AICc(tlidatny1_mod1)
#quadratic
tlidatny1_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatny1, na.action=na.omit, method="ML")
anova(tlidatny1_mod2)
sem.model.fits(tlidatny1_mod2)
AICc(tlidatny1_mod2)

ny1dat<-tlidatny1[,c(2,3,4,11,12)]
ny1dat<-groupedData(seedyld~Nfertnew|location/rep/plot, data=ny1dat,
                    labels=list(x='Nitrogen rate', y="Grain yield"),
                    units=list(x="(kg2/ha)", y="(kg2/ha)"))

tlidatny1_mod3<-nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                     data=ny1dat,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location/rep,
                     method="ML",
                     control=nlmeControl(pnlsTol = 0.01, verbose=T, maxIter=1000),
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
fixef(tlidatny1_mod3)

#comparing models
selMod(list(tlidatny1_mod1,tlidatny1_mod2,tlidatny1_mod3))

#bootstrapped AONR of reparametarized model
bootaonr_ny1_mod3<-nlsboot.aonr(tlidatny1_mod3,ny1dat,1000)


#Year 2 in north (2014)
tlidatny2<-subset(tlidatn, Age=="2")

tlidatny2_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatny2, na.action=na.omit, method="ML")
anova(tlidatny2_mod1)
sem.model.fits(tlidatny2_mod1)
AICc(tlidatny2_mod1)

#quadratic
tlidatny2_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatny2, na.action=na.omit, method="ML")
anova(tlidatny2_mod2)
fixef(tlidatny2_mod2)
sem.model.fits(tlidatny2_mod2)
AICc(tlidatny2_mod2)
selMod(list(tlidatny2_mod1, tlidatny2_mod2))

#Calculating bootstrapped CIs (95%)
bootaonr_ny2_mod2<-aonr.out(tlidatny2_mod2, tlidatny2, 100)
#Range from boot CIs
bootaonr_ny2_mod2$UpAONR-bootaonr_ny2_mod2$LowAONR

#Rearranged quadratic model with random effects
#grouping data to use mixed effects model for accounting random effects.
ny2dat<-tlidatny2[,c(2,3,4,11,12)]
ny2dat<-groupedData(seedyld~Nfertnew|location/rep/plot, data=ny2dat,
                   labels=list(x='Nitrogen rate', y="Grain yield"),
                   units=list(x="(kg2/ha)", y="(kg2/ha)"))

tlidatny2_mod3<-nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                     data=ny2dat,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location/rep,
                     method="ML",
                     control=nlmeControl(pnlsTol = 0.01, verbose=T, maxIter=1000),
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
fixef(tlidatny2_mod3)
#bootstrapped AONR of reparametarized model
bootaonr_ny2_mod3<-nlsboot.aonr(tlidatny2_mod3,ny2dat,1000)
#Range of bootstraped AONR of reparametarized model
bootaonr_ny2_mod3$UpAONR-bootaonr_ny2_mod3$LowAONR

#Year 3 in north (2015)
tlidatny3<-subset(tlidatn, Age=="3")

tlidatny3_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, method="ML", data=tlidatny3, na.action=na.omit)
anova(tlidatny3_mod1)
sem.model.fits(tlidatny3_mod1)
AICc(tlidatny3_mod1)

#quadratic
tlidatny3_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, method="ML", data=tlidatny3, na.action=na.omit)
anova(tlidatny3_mod2)
sem.model.fits(tlidatny3_mod2)
AICc(tlidatny3_mod2)
selMod(list(tlidatny3_mod1, tlidatny3_mod2))
#grouped data for nls mixed models
ny3dat<-tlidatny3[,c(2,3,4,11,12)]
ny3dat<-groupedData(seedyld~Nfertnew|location/rep/plot, data=ny3dat,
                    labels=list(x='Nitrogen rate', y="Grain yield"),
                    units=list(x="(kg2/ha)", y="(kg2/ha)"))

tlidatny3_mod3<-nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                     data=ny3dat,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location/rep,
                     control=nlmeControl(pnlsTol = 0.01, verbose=T, maxIter=10000),
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
fixef(tlidatny3_mod3)
#bootstrapped AONR of reparametarized model
bootaonr_ny3_mod3<-nlsboot.aonr(tlidatny3_mod3,ny3dat,1000)
#Range by max likelihood
confint(tlidatny3_mod4, "gamma", level=0.95)
#Range of bootstraped AONR of reparametarized model
bootaonr_ny3_mod3$UpAONR-bootaonr_ny3_mod3$LowAONR
ny3modlst<-list(tlidatny3_mod1,tlidatny3_mod2,tlidatny3_mod3)
selMod(ny3modlst)
##################################################
AONRtbl<-data.frame(rbind(bootaonr_sy2_mod3,
                          bootaonr_sy4_mod3,
                          bootaonr_ny1_mod3,
                          bootaonr_ny2_mod3,
                          bootaonr_ny3_mod3))
AONRtbl$Region<-c("South","South",rep("North",3))
AONRtbl$StandAge<-c(2,4,1,2,3)
write.csv(AONRtbl, "/Users/junge037/Documents/Projects/IREE-IWG/Data-2015/FinalAnalysis/Nstudy_Writeup/AONRgraintable.csv")
