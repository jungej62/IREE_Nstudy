#Run IREE-N-DataPrep.R first
#Looking at North region 
#Looking at grain yields for all locations in North
sumdat<-summarySE(Ndat, measurevar="seedyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)
ggplot(sumdat, 
       aes(x=Nfertnew, y=seedyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))


#Looking at biomass yields for all locations in North
sumdat_b<-summarySE(Ndat, measurevar="grassyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_b,
       aes(x=Nfertnew, y=grassyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=grassyld-se, ymax=grassyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Biomass Yield " ~ (kg ~ ha^{-1})))

#Nitrogen content... spotty data
sumdat_nc<-summarySE(Ndat, measurevar="Nc", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_nc,
       aes(x=Nfertnew, y=Nc, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=Nc-se, ymax=Nc+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Nitrogen content " ~ (g ~ kg^{-1})))

#Lodging scores
sumdat_lodge<-summarySE(Ndat, measurevar="lod", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_lodge,
       aes(x=Nfertnew, y=lod, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=lod-se, ymax=lod+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("lodging ")

#Plant height
sumdat_ht<-summarySE(Ndat, measurevar="ht", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_ht,
       aes(x=Nfertnew, y=ht, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=ht-se, ymax=ht+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("Plant height (cm)")

#Harvest index
sumdat_hi<-summarySE(Ndat, measurevar="hi", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_hi,
       aes(x=Nfertnew, y=hi, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=hi-se, ymax=hi+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("Harvest index")

######################################## Making similar figures for southern locations
#Looking at grain yields for all locations in South
sumdats<-summarySE(Sdat, measurevar="seedyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)
ggplot(sumdats, 
       aes(x=Nfertnew, y=seedyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))


#Looking at biomass yields for all locations in south
sumdats_b<-summarySE(Sdat, measurevar="grassyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdats_b,
       aes(x=Nfertnew, y=grassyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=grassyld-se, ymax=grassyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Biomass Yield " ~ (kg ~ ha^{-1})))

sumdats_nc<-summarySE(Sdat, measurevar="Nc", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdats_nc,
       aes(x=Nfertnew, y=Nc, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=Nc-se, ymax=Nc+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Nitrogen content " ~ (g ~ kg^{-1})))

sumdats_lodge<-summarySE(Sdat, measurevar="lod", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdats_lodge,
       aes(x=Nfertnew, y=lod, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=lod-se, ymax=lod+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("lodging ")

sumdats_ht<-summarySE(Sdat, measurevar="ht", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdats_ht,
       aes(x=Nfertnew, y=ht, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=ht-se, ymax=ht+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("Plant height (cm)")

sumdats_hi<-summarySE(Sdat, measurevar="hi", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdats_hi,
       aes(x=Nfertnew, y=hi, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=hi-se, ymax=hi+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("Harvest index")

######### Now plotting TLI grain yield points and model fit regression lines
predatpts<-aonr.out(tlidatny2_mod2, tlidatny2, 100)

predat<-data.frame(Nfertnew=c(0:145))
predat$rep<-NA
predat$location<-NA
predat$seedyld<-predict(tlidatny2_mod2, predat, level=0)

ggplot(summarySE(tlidatny2, measurevar="seedyld", groupvars=c("location", "Nfertnew")), 
       aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point(alpha=.5)+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  geom_point(aes(x=predatpts$AONR, y=predatpts$EstYld), shape=4, color='black', size=5)+
  geom_line(data=predat, color='black')+
  geom_segment(aes(x=predatpts$LowAONR, 
                  y=predatpts$EstYld, xend=predatpts$UpAONR, yend=predatpts$EstYld), alpha=.5, linetype=2, size=1, color="black")+
  #geom_segment(aes(x=predatpts$AONR, 
  #                 y=predatpts$LowEstYld, xend=predatpts$AONR, yend=predatpts$UpEstYld), linetype=1, size=1, color=4)+
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
#ggsave("GrainAllLocs2013.pdf", width=9, height=7, units="in", path="/Users/junge037/Documents/Projects/IREE-IWG/Data-2015/FinalAnalysis/Nstudy_Writeup/Figures") 

#### Trying to make a latticed ggplot with regression curves. Need three data frames
#Data points to plot
bigplotpts<-summarySE(subset(dat, var=="TLI"&Age>0&exp=="N"),
                             measurevar="seedyld", groupvars=c('region', 'location', 'Age','Nfertnew'), na.rm=T)
#AONR estimates for quadratic responses
bigplotpredpts<-data.frame(expand.grid(Age=c(1:4),region=c("North", "South"), AONR=NA, LowAONR=NA, UpAONR=NA, EstYld=NA))

#Putting models and data sets for each in two lists
modlist<-list(tlidatny1_mod2, tlidatny2_mod2, tlidatny3_mod2,NA,NA,tlidatsy2_mod2, NA, tlidatsy4_mod2)
datlist<-list(tlidatny1, tlidatny2, tlidatny3,NA,NA,tlidatsy2, NA, tlidatsy4)
#A little loop to run the aonr.out function with all age/regions
for(i in 1:8){
  if(is.na(modlist[[i]])==T) next
  zz<-aonr.out(modlist[[i]],datlist[[i]],100)
  bigplotpredpts[i,3:6]<-zz[1:4]
}
#Now make a long data frame to plot the curves
bigplotpredlines<-data.frame(expand.grid(Nfertnew=c(0:200), Age=c(1:4), region=c("North", "South"), location=NA, seedyld=NA))
bigplotpredlines$seedyld[1:201]<-predict(tlidatny1_mod2, bigplotpredlines[1:201,], level=0)
bigplotpredlines$seedyld[202:323]<-predict(tlidatny2_mod2, bigplotpredlines[202:323,], level=0)
bigplotpredlines$seedyld[403:524]<-predict(tlidatny3_mod2, bigplotpredlines[403:524,], level=0)
bigplotpredlines$seedyld[1006:1206]<-predict(tlidatsy2_mod2, bigplotpredlines[1006:1206,], level=0)
bigplotpredlines$seedyld[1408:1529]<-predict(tlidatsy4_mod2, bigplotpredlines[1408:1529,], level=0)

#Plotting points, AONRs, and curves
ggplot(bigplotpts, 
       aes(x=Nfertnew, y=seedyld, color=location))+
  facet_grid(region~Age)+
  geom_point(alpha=.5)+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  #geom_point(aes(x=bigplotpredpts$AONR, y=bigplotpredpts$EstYld), shape=4, color='black', size=5)+
  geom_line(data=bigplotpredlines, color='black')+
  #geom_segment(aes(x=bigplotpredpts$LowAONR, 
   #                y=bigplotpredpts$EstYld, xend=bigplotpredpts$UpAONR, yend=bigplotpredpts$EstYld), alpha=.5, linetype=2, size=1, color="black")+
  xlab(expression("N fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))+
  scale_color_manual(values=cbPalette, labels=c("Crookston", "Lamberton", "Morris", "Roseau", "Waseca"))+
  theme(plot.title=element_text(size=10,face='bold', hjust=.02),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        #panel.border=element_blank(),
        panel.border=element_rect(fill=NA),
        legend.title=element_blank(),
        legend.position=c(.85,.83),
        legend.key=element_rect(fill='white', colour='white'),
        legend.key.size=unit(.5, 'cm'),
        legend.text=element_text(size=10),
        axis.line = element_line(color='black'),
        axis.text.x=element_text(size=10, color='black'),
        axis.text.y=element_text(size=10, color='black'))
ggsave("GrainAllLocs.pdf", width=10, height=6, units="in", path="/Users/junge037/Documents/Projects/IREE-IWG/Data-2015/FinalAnalysis/Nstudy_Writeup/Figures") 
