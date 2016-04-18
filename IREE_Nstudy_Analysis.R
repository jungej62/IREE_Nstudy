sumdat<-summarySE(dat, measurevar="seedyld", groupvars=c("fyear", "location", "var", "Nfert"), na.rm=T)
seed_dat<-subset(dat, fyear!="2012"&var!="switch")
mod1<-lme(seedyld~var*Nfert*fyear, random=~1|location, data=seed_dat, na.action=na.omit)
anova(mod1)
#Grain yields differed by variety. Changes in grain yield through time and in
#response to N fert were unique for each variety. So seperate and analyze independently

