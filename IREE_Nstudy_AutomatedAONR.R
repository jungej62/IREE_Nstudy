# make sure other scripts have run.

# reg = region, yr = stand age, crp = crop of interest
auto.aonr<-function(reg, yr, crp, comp){
  if(reg=="North"){
    tpdat<-Ndat
  }else{
    tpdat<-Sdat
  }
  tp2dat<-subset(tpdat, Age==yr&var==crp)
  names(tp2dat)[which(names(tp2dat)==comp)]<-"response"
  m1<-lme(response~Nfertnew, random=~1|location/rep, data=tp2dat, na.action=na.omit, method="ML")
  m2<-lme(response~Nfertnew+I(Nfertnew^2), random=~1|location/rep, data=tp2dat, na.action=na.omit, method="ML")

  if(selMod(list(m1,m2))[1,3]>selMod(list(m1,m2))[2,3]){
    
    grdat<-tp2dat[,c("location","plot","rep","Nfertnew","response")]
    grdat<-groupedData(response~Nfertnew|location/rep/plot, data=grdat,
                       labels=list(x='Nitrogen rate', y="Response"),
                       units=list(x="(kg2/ha)", y="(kg2/ha)"))
    
    m3<-nlme(response ~ qmmod(Nfertnew, alpha, beta, gamma),
             data=grdat,
             fixed=alpha+beta+gamma~1,
             random=alpha~1|location/rep,
             method="ML",
             control=nlmeControl(pnlsTol = 0.01, verbose=T, maxIter=1000),
             start=c(alpha=5000, beta=-0.01, gamma=100),
             na.action=na.omit)
    #print(summary(m3))
    #nlsboot.aonr(m3,grdat,1000)
    #looing up grain AONR to predict biomass from at correct region/age
    tmplook<-subset(AONRtbl, Region==tpdat$region[1]&StandAge==yr)[1]
    colnames(tmplook)[1]<-"Nfertnew"
    tmplook$rep<-NA
    tmplook$location<-NA
    tmplook$response<-predict(m3, tmplook, level=0)
    names(tmplook)[4]<-comp
    tmplook$ModelType<-"quadratic"
    print(tmplook)
  }else{
    tmplook<-subset(AONRtbl, Region==tpdat$region[1]&StandAge==yr)[1]
    colnames(tmplook)[1]<-"Nfertnew"
    tmplook$rep<-NA
    tmplook$location<-NA
    tmplook$response<-predict(m1, tmplook, level=0)
    names(tmplook)[4]<-comp
    tmplook$ModelType<-"linear"
    print(tmplook)
  }
  
}

auto.aonr("North",1,"TLI", "grassyld")
tligrass<-data.frame(Region=c("North","North", "North", "South","South"),
                     Age=c(1,2,3,2,4),
                     Crop=rep("TLI", 5),
                     grassyld=NA,
                     IVTD=NA,
                     hi=NA,
                     lod=NA,
                     ht=NA)
for(i in 1:length(tligrass$hi)){
  for(j in 4:8){
  tligrass[i,j]<-auto.aonr(as.character(tligrass[i,1]),
                           as.character(tligrass[i,2]), 
                           as.character(tligrass[i,3]), 
                           names(tligrass[j]))[4]
  }
}

write.csv(tligrass, "/Users/junge037/Documents/Projects/IREE-IWG/Data-2015/FinalAnalysis/Nstudy_Writeup/ResponsesatAONR.csv")

