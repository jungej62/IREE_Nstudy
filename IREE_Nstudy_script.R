library(agricolae);library(nlme);library(car);library(faraway); library(ggplot2); library(lattice); library(reshape)
library(multcomp); library(vegan); library(lme4); library(AICcmodavg);
library(arm); library(coefplot); library(gridExtra); library(devtools); library(nls2); library(piecewiseSEM)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","gray30")
source_gist('https://gist.github.com/jungej62/5a76b72bcd0b12a7ce8a')
#ssh: git@github.com:jungej62/IREE_Nstudy.git

dat<-read.csv(file="IREE-N-All.csv", header=T)
str(dat)
dat$plot<-factor(dat$plot); dat$rep<-factor(dat$rep)
dat$fyear<-factor(dat$Year)
dat$fNfert<-factor(dat$Nfert)
dat$trt<-factor(dat$trt)
dat$hi<-dat$seedyld/dat$grassyld
dat$Nc<-dat$N
dat$Age<-dat$Year-2012

#Checking if crude protein and N are correlated
xyplot(CP~N|location, groups=var, auto.key=T, subset(dat, Year=="2013"))

#Looking at grain yields for all environments
sumdat<-summarySE(dat, measurevar="seedyld", groupvars=c("fyear", "location", "var", "Nfert"), na.rm=T)

ggplot(sumdat,
       aes(x=Nfert, y=seedyld, color=var))+
  facet_grid(fyear~location)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))

#Looking at biomass yields for all environments
sumdat_b<-summarySE(dat, measurevar="grassyld", groupvars=c("fyear", "location", "var", "Nfert"), na.rm=T)
ggplot(sumdat_b,
       aes(x=Nfert, y=grassyld, color=var))+
  facet_grid(fyear~location)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=grassyld-se, ymax=grassyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Biomass Yield " ~ (kg ~ ha^{-1})))

#Looking at N content in 2013 and 2014
ggplot(summarySE(subset(dat, fyear=="2013"|fyear=="2014"),
                 measurevar="Nc", groupvars=c("fyear", "location", "var", "Nfert"), na.rm=T),
       aes(x=Nfert, y=Nc, color=var))+
  facet_grid(fyear~location)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=Nc-se, ymax=Nc+se))+
  xlab("Nfert")+
  ylab(expression("N content " ~ (g ~ kg^{-1})))
