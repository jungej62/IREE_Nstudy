sumdat<-summarySE(dat, measurevar="seedyld", groupvars=c("fyear", "location", "var", "Nfert"), na.rm=T)
seed_dat<-subset(dat, fyear!="2012"&var!="switch")
mod1<-lme(seedyld~var*Nfert*Age, random=~1|fyear/location/rep, data=seed_dat, na.action=na.omit)
anova(mod1)
#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

#Subsetting data to analyze TLI variety only
tlidat<-subset(dat, fyear!="2012"&var=="TLI")
tlidat_mod1<-lme(seedyld~Nfert*Age, random=~1|fyear/location/rep, data=tlidat, na.action=na.omit)
anova(tlidat_mod1)

#There is a Nfert/age interaction, so maybe we should fit Nfert for each year seperetly.
#Start with subsetting for 2013
tlidat13<-subset(dat, fyear=="2013"&var=="TLI")

tlidat13_mod1<-lme(seedyld~Nfert, random=~1|location/rep, data=tlidat13, na.action=na.omit)
anova(tlidat13_mod1)
sem.model.fits(tlidat13_mod1)

#quadratic
tlidat13_mod2<-lme(seedyld~Nfert+I(Nfert^2), random=~1|location/rep, data=tlidat13, na.action=na.omit)
anova(tlidat13_mod2)
sem.model.fits(tlidat13_mod2)

#looking at aonr for seed yield 2013 data
aonr.out(tlidat13_mod2, tlidat13, 20)

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
summary(tlidat13_qpmod);
AIC(tlidat13_mod2)

