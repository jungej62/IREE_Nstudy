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

#Analyzing grain yield by location
cromod<-lme(seedyld~Nfert,#+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Cro"), na.action=na.omit)
anova(cromod)
sem.model.fits(cromod)

Lammod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Lam"), na.action=na.omit)
anova(Lammod)
sem.model.fits(Lammod)

Wasmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Was"), na.action=na.omit)
anova(Wasmod)
sem.model.fits(Wasmod)

Mormod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Mor"), na.action=na.omit)
anova(Mormod)
sem.model.fits(Mormod)

Rosmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat13, location=="Ros"), na.action=na.omit)
anova(Rosmod)
sem.model.fits(Rosmod)

###########################     2014      ###########

tlidat14<-subset(dat, fyear=="2014"&var=="TLI")

tlidat14_mod0<-lme(seedyld~Nfert*location, random=~1|rep, data=tlidat14, na.action=na.omit)
anova(tlidat14_mod0)
sem.model.fits(tlidat14_mod0)

tlidat14_mod01<-lme(seedyld~Nfert*location+I(Nfert^2)*location, random=~1|rep, data=tlidat14, na.action=na.omit)
anova(tlidat14_mod01)
sem.model.fits(tlidat14_mod01)

tlidat14_mod1<-lme(seedyld~Nfert, random=~1|location/rep, data=tlidat14, na.action=na.omit)
anova(tlidat14_mod1)
sem.model.fits(tlidat14_mod1)

#quadratic
tlidat14_mod2<-lme(seedyld~Nfert+I(Nfert^2), random=~1|location/rep, data=tlidat14, na.action=na.omit)
anova(tlidat14_mod2)
sem.model.fits(tlidat14_mod2)

#Analyzing grain yield by location
#Crookston
cromod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat14, location=="Cro"), na.action=na.omit)
anova(cromod)
sem.model.fits(cromod)

#Looking at quadratic plateau for this location
gstart <- data.frame(alpha = c(0,1000), beta = c(-100, 100), gamma=c(-50,50))
Cro14_qpmod<-nls2(seedyld ~ qpmod(Nfert, alpha, beta, gamma), data=subset(tlidat14, location=="Cro"), 
                     start = gstart, algorithm = "brute-force",
                     control = list(maxiter=10000), na.action=na.omit)

#Doesn't work. Now trying linear plateau model
Cro14_lpmod<-nls2(seedyld ~ linplat(Nfert, alpha, beta, gamma), data=subset(tlidat14, location=="Cro"), 
                  start = gstart, algorithm = "brute-force",
                  control = list(maxiter=10000), na.action=na.omit)

Cro14_lpmod<-nls(seedyld ~ linplat(Nfert, alpha, beta, gamma), data=subset(tlidat14, location=="Cro"),
                 start = list(alpha = 900,beta = 100, gamma = 50),  na.action=na.omit)

#Lamberton
Lammod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat14, location=="Lam"), na.action=na.omit)
anova(Lammod)
sem.model.fits(Lammod)

#Waseca
Wasmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat14, location=="Was"), na.action=na.omit)
anova(Wasmod)
sem.model.fits(Wasmod)

#Roseau
Rosmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat14, location=="Ros"), na.action=na.omit)
anova(Rosmod)
sem.model.fits(Rosmod)


###########################     2015      ###########

tlidat15<-subset(dat, fyear=="2015"&var=="TLI")

tlidat15_mod0<-lme(seedyld~Nfert*location, random=~1|rep, data=tlidat15, na.action=na.omit)
anova(tlidat15_mod0)
sem.model.fits(tlidat15_mod0)

tlidat15_mod01<-lme(seedyld~Nfert*location+I(Nfert^2)*location, random=~1|rep, data=tlidat15, na.action=na.omit)
anova(tlidat15_mod01)
sem.model.fits(tlidat15_mod01)

tlidat15_mod1<-lme(seedyld~Nfert, random=~1|location/rep, data=tlidat15, na.action=na.omit)
anova(tlidat15_mod1)
sem.model.fits(tlidat15_mod1)

#quadratic
tlidat15_mod2<-lme(seedyld~Nfert+I(Nfert^2), random=~1|location/rep, data=tlidat15, na.action=na.omit)
anova(tlidat15_mod2)
sem.model.fits(tlidat15_mod2)

#Analyzing grain yield by location
#Crookston
cromod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat15, location=="Cro"), na.action=na.omit)
anova(cromod)
sem.model.fits(cromod)

#Lamberton
Lammod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat15, location=="Lam"), na.action=na.omit)
anova(Lammod)
sem.model.fits(Lammod)

#Waseca
Wasmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat15, location=="Was"), na.action=na.omit)
anova(Wasmod)
sem.model.fits(Wasmod)

#Morris
Mormod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat15, location=="Mor"), na.action=na.omit)
anova(Mormod)
sem.model.fits(Mormod)

#Roseau
Rosmod<-lme(seedyld~Nfert+I(Nfert^2),
            random=~1|rep, data=subset(tlidat15, location=="Ros"), na.action=na.omit)
anova(Rosmod)
sem.model.fits(Rosmod)

##################################################

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

