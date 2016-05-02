#Run IREE-N-DataPrep.R first

#Looking at grain yields for all environments
sumdat<-summarySE(Ndat, measurevar="seedyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)
ggplot(sumdat, 
       aes(x=Nfertnew, y=seedyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))


#Looking at biomass yields for all environments
sumdat_b<-summarySE(Ndat, measurevar="grassyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_b,
       aes(x=Nfertnew, y=grassyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=grassyld-se, ymax=grassyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Biomass Yield " ~ (kg ~ ha^{-1})))

sumdat_l<-summarySE(Ndat, measurevar="legumeyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_l,
       aes(x=Nfertnew, y=legumeyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=legumeyld-se, ymax=legumeyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Biomass Yield " ~ (kg ~ ha^{-1})))

sumdat_tb<-summarySE(Ndat, measurevar="biomassyld", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_tb,
       aes(x=Nfertnew, y=biomassyld, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=biomassyld-se, ymax=biomassyld+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Biomass Yield " ~ (kg ~ ha^{-1})))

sumdat_nc<-summarySE(Ndat, measurevar="Nc", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_nc,
       aes(x=Nfertnew, y=Nc, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=Nc-se, ymax=Nc+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Nitrogen content " ~ (g ~ kg^{-1})))

sumdat_lodge<-summarySE(Ndat, measurevar="lod", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_lodge,
       aes(x=Nfertnew, y=lod, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=lod-se, ymax=lod+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("lodging ")

sumdat_ht<-summarySE(Ndat, measurevar="ht", groupvars=c("Age", "location", "var", "Nfertnew"), na.rm=T)

ggplot(sumdat_ht,
       aes(x=Nfertnew, y=ht, color=var))+
  facet_grid(location~Age)+
  geom_point()+
  geom_line()+
  geom_errorbar(width=.2, aes(ymin=ht-se, ymax=ht+se))+
  xlab(expression("Nitrogen fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab("lodging (cm)")


########################################

predatt<-data.frame(Nfert2=c(0:200))
predatt$rep<-NA
predatt$seedyld<-predict(Mormod, predatt, level=0)

ggplot(subset(tlidatsy2, location=="Mor"), aes(x=Nfert2, y=seedyld))+
  geom_point(alpha=.5)+
  geom_line(data=predatt)+
  xlab(expression("N fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))
  
  


