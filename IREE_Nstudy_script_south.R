#Run IREE-N-datsaPrep.R first

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

########################################

predatst<-datsa.frame(Nfert2=c(0:200))
predatst$rep<-NA
predatst$seedyld<-predict(Mormod, predatst, level=0)

ggplot(subset(tlidatssy2, location=="Mor"), aes(x=Nfert2, y=seedyld))+
  geom_point(alpha=.5)+
  geom_line(datsa=predatst)+
  xlab(expression("N fertilizer rate " ~ (kg ~ ha^{-1})))+
  ylab(expression("Seed Yield " ~ (kg ~ ha^{-1})))





