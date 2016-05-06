predat<-data.frame(Nfertnew=c(0:145))
predat$rep<-NA
predat$location<-NA
predat$seedyld<-predict(tlidatny2_mod5, predat, level=0)

ggplot(summarySE(tlidatny2, measurevar="seedyld", groupvars=c("location", "Nfertnew")), 
       aes(x=Nfertnew, y=seedyld, color=location))+
  geom_point(alpha=.5)+
  geom_errorbar(width=.2, aes(ymin=seedyld-se, ymax=seedyld+se))+
  geom_line(data=predat, color='black')+
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