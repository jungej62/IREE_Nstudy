#Statistical models for IREE_Nstudy. This will be for grain only!
#First, we have to separate the northern sites and the southern sites.
#We have to omit the 2012 data from the southern sites. One idea is to subset the 0N plots and analyze 
#stand declines in these plots only. Doing this in the south would provide 4 years of decline data.

datn<-droplevels(subset(dat, region=="North"))
dats<-droplevels(subset(dat, region=="South"))

#Analyzing grain yields in south first.
seed_dats<-subset(dats, var!="switch")
mod1<-lme(seedyld~var*Nfert2*Age, random=~1|fyear/location/rep, data=seed_dats, na.action=na.omit)
anova(mod1)
#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

#Subsetting data to analyze TLI variety only
tlidats<-subset(dats, var=="TLI")
tlidats_mod1<-lme(seedyld~Nfert2*Age, random=~1|fyear/location/rep, data=tlidats, na.action=na.omit)
anova(tlidat_mod1)
#There is a Nfert/age interaction, so maybe we should fit Nfert for each year seperetly.
#Start with subsetting for year 2 (2013)
tlidatsy2<-subset(tlidats, Age=="2"&var=="TLI")

tlidatsy2_mod1<-lme(seedyld~Nfert2, random=~1|location/rep, data=tlidatsy2, na.action=na.omit)
anova(tlidatsy2_mod1)
sem.model.fits(tlidatsy2_mod1)

#quadratic
tlidatsy2_mod2<-lme(seedyld~Nfert2+I(Nfert2^2), random=~1|location/rep, data=tlidatsy2, na.action=na.omit)
anova(tlidatsy2_mod2)
sem.model.fits(tlidatsy2_mod2)

#Another approach is to see if grain yields at moderate N rates
#were different from 0 N rate by changing N to factor and using
#means comparisons
tlidatsy2_mod3<-lme(seedyld~factor(Nfert2), random=~1|location/rep, data=tlidatsy2, na.action=na.omit)
summary(tlidatsy2_mod3)

#Analyzing grain yield by location 
######South
##Year 1 (2012)
#Calculating mean grain yields in control plots
summarySE(subset(tlidats, Nfert2=="0"), measurevar="seedyld", 
          groupvars=c("Age", "location"), na.rm=T)

##South
######Year 2 (2013)

Lammod<-lme(seedyld~Nfert2+I(Nfert2^2),
            random=~1|rep, data=subset(tlidatsy2, location=="Lam"), na.action=na.omit)
anova(Lammod)
sem.model.fits(Lammod)
aonr.out(Lammod, subset(tlidatsy2, location=="Lam"), 100)


Wasmod<-lme(seedyld~Nfert2+I(Nfert2^2),
            random=~1|rep, data=subset(tlidatsy2, location=="Was"), na.action=na.omit)
anova(Wasmod)
sem.model.fits(Wasmod)

Mormod<-lme(seedyld~Nfert2+I(Nfert2^2),
            random=~1|rep, data=subset(tlidatsy2, location=="Mor"), na.action=na.omit)
anova(Mormod)
sem.model.fits(Mormod)


###South
### Year 3 (2014)
tlidatsy3<-subset(tlidats, Age=="3"&var=="TLI")

tlidat14_mod0<-lme(seedyld~Nfert2*location, random=~1|rep, data=tlidatsy3, na.action=na.omit)
anova(tlidat14_mod0)
sem.model.fits(tlidat14_mod0)

tlidat14_mod01<-lme(seedyld~Nfert2*location+I(Nfert2^2)*location, random=~1|rep, data=tlidatsy3, na.action=na.omit)
anova(tlidat14_mod01)
sem.model.fits(tlidat14_mod01)

tlidat14_mod1<-lme(seedyld~Nfert2, random=~1|location/rep, data=tlidatsy3, na.action=na.omit)
anova(tlidat14_mod1)
sem.model.fits(tlidat14_mod1)

#quadratic
tlidat14_mod2<-lme(seedyld~Nfert2+I(Nfert2^2), random=~1|location/rep, data=tlidatsy3, na.action=na.omit)
anova(tlidat14_mod2)
sem.model.fits(tlidat14_mod2)

#######NO EFFECT OF N FERTILIZER IN YEAR 3

########################### Year 4    2015      ###########

tlidatsy4<-subset(dats, fyear=="2015"&var=="TLI")

tlidat15_mod0<-lme(seedyld~Nfert2*location, random=~1|rep, data=tlidatsy4, na.action=na.omit)
anova(tlidat15_mod0)
sem.model.fits(tlidat15_mod0)

tlidat15_mod01<-lme(seedyld~Nfert2*location+I(Nfert2^2)*location, random=~1|rep, data=tlidatsy4, na.action=na.omit)
anova(tlidat15_mod01)
sem.model.fits(tlidat15_mod01)

#quadratic
tlidat15_mod2<-lme(seedyld~Nfert2+I(Nfert2^2), random=~1|location/rep, data=tlidatsy4, na.action=na.omit)
anova(tlidat15_mod2)
sem.model.fits(tlidat15_mod2)


################################################## North
tlidatn<-subset(datn, var=="TLI")

########## Year 1

cromod<-lme(seedyld~Nfert,#+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Cro"), na.action=na.omit)
anova(cromod)
sem.model.fits(cromod)

Rosmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Ros"), na.action=na.omit)
anova(Rosmod)
sem.model.fits(Rosmod)






#looking at aonr for seed yield 2013 data
aonr.out(tlidat13_mod2, tlidat13, 20, 0.25)

#Re-arranged version of quadratic to ID the peak. However, doesn't include random effects.
tlidat13_qmod<-nls(seedyld ~ alpha - ((2*gamma*beta)*Nfert)+(beta*Nfert^2), data=tlidat13, 
                  start = list(alpha = 2300, beta = -0.1, gamma = 100),
                  control = list(maxiter=200))
summary(tlidat13_qmod)
AIC(tlidat13_qmod)

#The quadratic with random effects has a lower AIC value than the model without random effects,
#so we should use the random model and bootstrap it to ID variation around max.

#Data frame to determine range for starting values. 
gstart <- data.frame(alpha = c(-100, 10000), beta = c(-10000, 1000), gamma=c(-500,500))

#This doesn't fit well. 
tlidat13_qpmod<-nls2(seedyld ~ qpmod(Nfert, alpha, beta, gamma), data=tlidat13, 
                     start = gstart, algorithm = "brute-force",
                     control = list(maxiter=1000), na.action=na.omit)
summary(tlidat13_qpmod)
AIC(tlidat13_mod2)

