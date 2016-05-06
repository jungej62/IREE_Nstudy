

#Analyzing grain yields in south first.
seed_dats<-subset(Sdat, var!="switch")
mod1<-lme(seedyld~var*Nfertnew*Age, random=~1|fyear/location/rep, data=seed_dats, na.action=na.omit)
anova(mod1)

#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

#Subsetting data to analyze TLI variety only
tlidats<-subset(Sdat, var=="TLI")
tlidats_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=tlidats, na.action=na.omit)
anova(tlidats_mod1); tt<-data.frame(anova(tlidats_mod1))

#Subsetting data to analyze Rush variety
rushdats<-subset(Sdat, var=="rush")
rushdats_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=rushdats, na.action=na.omit)
anova(rushdats_mod1) 

#There is a Nfert/age interaction, so maybe we should fit Nfert for each year seperetly.
#Start with subsetting for year 2 (2013)
tlidatsy2<-subset(tlidats, Age=="2")

tlidatsy2_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatsy2, na.action=na.omit)
anova(tlidatsy2_mod1)
sem.model.fits(tlidatsy2_mod1)

#quadratic
tlidatsy2_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatsy2, na.action=na.omit)
anova(tlidatsy2_mod2)
sem.model.fits(tlidatsy2_mod2)

#Year 3 in south (2014)
tlidatsy3<-subset(tlidats, Age=="3")

tlidatsy3_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatsy3, na.action=na.omit)
anova(tlidatsy3_mod1)
sem.model.fits(tlidatsy3_mod1)

#quadratic
tlidatsy3_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatsy3, na.action=na.omit)
anova(tlidatsy3_mod2)
sem.model.fits(tlidatsy3_mod2)

#Year 4 in south (2015)
tlidatsy4<-subset(tlidats, Age=="4")

tlidatsy4_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatsy4, na.action=na.omit)
anova(tlidatsy4_mod1)
sem.model.fits(tlidatsy4_mod1)

#quadratic
tlidatsy4_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatsy4, na.action=na.omit)
anova(tlidatsy4_mod2)
sem.model.fits(tlidatsy4_mod2)


################   Analyzing northern region
seed_datn<-subset(Ndat, var!="switch")
mod1<-lme(seedyld~var*Nfertnew*Age, random=~1|fyear/location/rep, data=seed_datn, na.action=na.omit)
anova(mod1)

#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

#Subsetting data to analyze TLI variety only
tlidatn<-subset(Ndat, var=="TLI")
tlidatn_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=tlidatn, na.action=na.omit)
anova(tlidatn_mod1); tt<-data.frame(anova(tlidatn_mod1))

#Subsetting data to analyze Rush variety
rushdatn<-subset(Ndat, var=="rush")
rushdatn_mod1<-lme(seedyld~Nfertnew*Age, random=~1|fyear/location/rep, data=rushdatn, na.action=na.omit)
anova(rushdatn_mod1) 

#There is a Nfert/age interaction, so maybe we should fit Nfert for each year seperetly.
#Start with subsetting for year 2 (2013)
tlidatny1<-subset(tlidatn, Age=="1")

tlidatny1_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatny1, na.action=na.omit)
anova(tlidatny1_mod1)
sem.model.fits(tlidatny1_mod1)

#quadratic
tlidatny1_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatny1, na.action=na.omit)
anova(tlidatny1_mod2)
sem.model.fits(tlidatny1_mod2)

#Year 2 in north (2014)
tlidatny2<-subset(tlidatn, Age=="2")

tlidatny2_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatny2, na.action=na.omit)
anova(tlidatny2_mod1)
sem.model.fits(tlidatny2_mod1)

#quadratic
tlidatny2_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatny2, na.action=na.omit)
anova(tlidatny2_mod2)
sem.model.fits(tlidatny2_mod2)

#Year 3 in north (2015)
tlidatny3<-subset(tlidatn, Age=="3")

tlidatny3_mod1<-lme(seedyld~Nfertnew, random=~1|location/rep, data=tlidatny3, na.action=na.omit)
anova(tlidatny3_mod1)
sem.model.fits(tlidatny3_mod1)

#quadratic
tlidatny3_mod2<-lme(seedyld~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tlidatny3, na.action=na.omit)
anova(tlidatny3_mod2)
sem.model.fits(tlidatny3_mod2)
##################################################
#Re-parametarized quadratic for AONR estimation with no random effect.
tlidatny2_mod3<-nls(seedyld ~ alpha - ((2*gamma*beta)*Nfertnew)+(beta*Nfertnew^2), data=tlidatny2, 
                    start = list(alpha = 250, beta = -0.1, gamma = 100),
                    control = list(maxiter=200))

#Trying a nls with random effects. Should produce the same output as mod3. Just uses function call.
tlidatny2_mod4<-nls(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                    data=tlidatny2,
                    start=list(alpha=600, beta=-0.1, gamma=100))
summary(tlidatny2_mod4)
#Trying nlme's auto fitter. This works and produces a unique result
tlidatny2_mod5<-nls(seedyld ~ SSasymp(Nfertnew, Asym, R0, lrc),
                    data=tlidatny2)
summary(tlidatny2_mod5)
#grouping data to use mixed effects model for accounting random effects.
data1<-tlidatny2[,c(2,3,11,12)]
data1<-groupedData(seedyld~Nfertnew|location/plot, data=data1,
                   labels=list(x='Nitrogen rate', y="Grain yield"),
                   units=list(x="(kg2/ha)", y="(kg2/ha)"))
#Rearranged quadratic model with random effects
tlidatny2_mod6<-nlme(seedyld ~ qmmod(Nfertnew, alpha, beta, gamma),
                     data=data1,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location,
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
summary(tlidatny2_mod6)

#Comparing mod4 and mod6 shows effect of accounting for random effects. 
#Comparing models with AIC shows that random effects matter.

#Quadratic plateau model with random effects                
tlidatny2_mod8<-nlme(seedyld ~ qpmod(Nfertnew, alpha, beta, gamma),
                     data=data1,
                     fixed=alpha+beta+gamma~1,
                     random=alpha~1|location,
                     start=c(alpha=700, beta=-0.01, gamma=100),
                     na.action=na.omit)
summary(tlidatny2_mod8)
#The quadratic plateau estimates a much smaller AONR. We should compare SE of estimates to see if they overlap with quadratic. 
anova(tlidatny2_mod6, tlidatny2_mod8)

