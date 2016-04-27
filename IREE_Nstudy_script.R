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

#Plotting TLI grain yields for all locations in 2013
predatt<-data.frame(Nfert=c(0:200))
predatt$location<-NA
predatt$seedyld<-predict(tlidat13_mod2, predatt, level=0)
aonrAll<-aonr.out(tlidat13_mod2, tlidat13, 100)


ggplot(tlidat13, aes(x=Nfert, y=seedyld, color=location))+
  geom_point(alpha=.5)+
  #geom_point(x=76.4, y=929, shape=8, color=2, size=4)+
  geom_point(aes(x=aonrAll$AONR, y=aonrAll$EstYld), shape=8, color=2, size=4)+
  geom_line(data=predatt)+
  geom_segment(aes(x=aonrAll$LowAONR, 
                   y=aonrAll$EstYld, xend=aonrAll$UpAONR, yend=aonrAll$EstYld), linetype=2, size=1, color=2)+
  geom_segment(aes(x=aonrAll$AONR, 
                   y=aonrAll$LowEstYld, xend=aonrAll$AONR, yend=aonrAll$UpEstYld), linetype=1, size=1, color=4)+
  #geom_text(aes(label=Nfert, data=tt))+
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
ggsave("GrainAllLocs2013.pdf", width=9, height=7, units="in", path="/Users/junge037/Documents/Projects/IREE-IWG/Data-2015/FinalAnalysis/Nstudy_Writeup/Figures") 

#Simple plots to show data by location
plot(seedyld ~ Nfert, pch = 16,
xlab = expression(paste("Nitrogen rate [kg ha" ^-1,"]")),
ylab = expression(paste("Grain yield [kg ha"^-1,"]")),
data=subset(tlidat15, location=="Cro"))
