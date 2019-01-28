df<-read.csv("MCM_NFLIS_Data.csv")
df.state <- unique(data.frame(Year=df$YYYY,State=df$State,TotalDrug=df$TotalDrugReportsState))
df.county <- unique(data.frame(Year=df$YYYY,State=df$State,County=df$COUNTY,TotalDrugC=df$TotalDrugReportsCounty,TotalDrugS=df$TotalDrugReportsState))

df.heroin<- subset.data.frame(df,df$SubstanceName=="Heroin")
df.nonheroin<- subset.data.frame(df,(df$SubstanceName=="Heroin")==FALSE)
df.VA<-subset.data.frame(df.heroin,df.heroin$State=="VA")
df.no.VA<-subset.data.frame(df.nonheroin,df.nonheroin$State=="VA")
df.OH<-subset.data.frame(df.heroin,df.heroin$State=="OH")
df.no.OH<-subset.data.frame(df.nonheroin,df.nonheroin$State=="OH")
df.PA<-subset.data.frame(df.heroin,df.heroin$State=="PA")
df.no.PA<-subset.data.frame(df.nonheroin,df.nonheroin$State=="PA")
df.KY<-subset.data.frame(df.heroin,df.heroin$State=="KY")
df.no.KY<-subset.data.frame(df.nonheroin,df.nonheroin$State=="KY")
df.WV<-subset.data.frame(df.heroin,df.heroin$State=="WV")
df.no.WV<-subset.data.frame(df.nonheroin,df.nonheroin$State=="WV")


#Draw graph of state
loop=2010
data<-df.no.WV
sum1<-c()
for (loop in 2010:2017) {
sub1<- subset(data,data$YYYY==loop)
sum1[loop-2009]<- sum(sub1$DrugReports)
loop=loop+1
}
sum1
T1<-2010:2017
plot(T1, sum1,type = "h", lwd = 20,col = "blue",ylim = c(0,2500),xlab = "Year from 2010 to 2017", ylab = "Sum Reports of Heroin",main="WV_Non-Heroin Reports")

#Draw graph of county
  #Heroin
library(plyr)
process1<-ddply(df.KY,~COUNTY,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.KY.COUNTY<-data.frame(COUNTY=process2$COUNTY[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.OH,~COUNTY,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.OH.COUNTY<-data.frame(COUNTY=process2$COUNTY[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.PA,~COUNTY,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.PA.COUNTY<-data.frame(COUNTY=process2$COUNTY[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.VA,~COUNTY,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.VA.COUNTY<-data.frame(COUNTY=process2$COUNTY[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

#Non-Heroin,first step:find drug with large shares

process1<-ddply(df.no.WV,~SubstanceName,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.no.WV.substance<-data.frame(SubstanceName=process2$SubstanceName[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.no.KY,~SubstanceName,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.no.KY.substance<-data.frame(SubstanceName=process2$SubstanceName[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.no.OH,~SubstanceName,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.no.OH.substance<-data.frame(SubstanceName=process2$SubstanceName[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.no.PA,~SubstanceName,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.no.PA.substance<-data.frame(SubstanceName=process2$SubstanceName[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])

process1<-ddply(df.no.VA,~SubstanceName,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.no.VA.substance<-data.frame(SubstanceName=process2$SubstanceName[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])
  

#Non-Heroin,second step:find drug distribution in state by county

lp2<-5
  proc1 <- subset(df.no.VA,df.no.VA$SubstanceName==df.no.VA.substance[lp2,1])
  proc2 <- ddply(proc1,~COUNTY,summarise,number_of_drug=sum(DrugReports))
  proc3 <- proc2[with(proc2,order(proc2$number_of_drug,decreasing = TRUE)),]
  pro3=0
  lp3<-1
  while ((pro3 < 0.8*sum(proc3$number_of_drug))==TRUE) {
    pro3 = pro3+proc3$number_of_drug[lp3]
    lp3=lp3+1
  }
  df.no.VA.COUNTY5<-data.frame(COUNTY=proc3$COUNTY[1:lp3-1],number_of_drug=proc3$number_of_drug[1:lp3-1])

  write.csv(df.no.VA.COUNTY1,"df.no.VA.COUNTY1.csv")
  write.csv(df.no.VA.COUNTY2,"df.no.VA.COUNTY2.csv")
  write.csv(df.no.VA.COUNTY3,"df.no.VA.COUNTY3.csv")
  write.csv(df.no.VA.COUNTY4,"df.no.VA.COUNTY4.csv")
  write.csv(df.no.VA.COUNTY5,"df.no.VH.COUNTY5.csv")
  

process1<-ddply(df.no.VA.substance,~COUNTY,summarise,number_of_drug=sum(DrugReports))
process2<-process1[with(process1,order(process1$number_of_drug,decreasing = TRUE)),]
pro3=0
lp1<-1
while ((pro3 < 0.8*sum(process2$number_of_drug))==TRUE) {
  pro3 = pro3+process2$number_of_drug[lp1]
  lp1=lp1+1
}
df.no.WV.COUNTY<-data.frame(COUNTY=process2$COUNTY[1:lp1-1],number_of_drug=process2$number_of_drug[1:lp1-1])


#

write.csv(df.PA.COUNTY,"PA_H.csv")
write.csv(df.OH.COUNTY,"OH_H.csv")
write.csv(df.VA.COUNTY,"VA_H.csv")
write.csv(df.WV.COUNTY,"WV_H.csv")
write.csv(df.KY.COUNTY,"KY_H.csv")
write.csv(df.no.PA.COUNTY,"PA_noH.csv")
write.csv(df.no.OH.COUNTY,"OH_noH.csv")
write.csv(df.no.VA.COUNTY,"VA_noH.csv")
write.csv(df.no.WV.COUNTY,"WV_no.csv")
write.csv(df.no.KY.COUNTY,"KY_noH.csv")
#
write.csv(df.no.PA.COUNTY.substance,"PA_noH.substance.csv")
write.csv(df.no.OH.COUNTY.substance,"OH_noH.substance.csv")
write.csv(df.no.VA.COUNTY.substance,"VA_noH.substance.csv")
write.csv(df.no.WV.COUNTY.substance,"WV_no.substance.csv")
write.csv(df.no.KY.COUNTY.substance,"KY_noH.substance.csv")

i <- sapply(df.PA.COUNTY, is.factor)
df.PA.COUNTY[i] <- lapply(df.PA.COUNTY[i], as.character)
barplot(df.PA.COUNTY$number_of_drug,width = 3,space = NULL,names.arg = df.PA.COUNTY$COUNTY)
#Haroin
lp5<-1
betak<-c()
for (lp5 in 1:dim(df.VA.COUNTY)[1]) {
  df.KY1<-subset(df.VA,df.VA$COUNTY==df.VA.COUNTY[lp5,1])
  reg <- lm(DrugReports~YYYY,data=df.KY1)
  coef1<-coef(reg)
  betak[lp5]<-coef(summary(reg))[2,1]
}
slope.VA<-betak
HaroinSlope<-data.frame(slope.OH=slope.OH)
HaroinSlope$slope.KY<-c(slope.KY,rep("", nrow(HaroinSlope)-length(slope.KY)))
HaroinSlope$slope.PA<-c(slope.PA,rep("", nrow(HaroinSlope)-length(slope.PA)))
HaroinSlope$slope.VA<-c(slope.VA,rep("", nrow(HaroinSlope)-length(slope.VA)))
HaroinSlope$slope.WV<-c(slope.WV,rep("", nrow(HaroinSlope)-length(slope.WV)))

write.csv(HaroinSlope,"HaroinSlope.csv")
#Non-Haroin
lp6<-1
betak<-c()
abc <- df.no.WV.COUNTY4
for (lp6 in 1:dim(abc)[1]) {
  df.WV1<-subset(df.no.WV,(df.no.WV$SubstanceName==df.no.WV.COUNTY.substance[4,1])&(df.no.WV$COUNTY==abc[lp6,1]))
  reg <- lm(DrugReports~YYYY,data=df.WV1)
  coef1<-coef(reg)
  betak[lp6]<-coef(summary(reg))[2,1]
  lp6=lp6+1
}
slope.no.WV4<-betak

NonHaroinSlope.WV<-data.frame(slope.no.WV1=c(slope.no.WV1,"","","","",""))
NonHaroinSlope.WV$slope.no.WV2<-c(slope.no.WV2,rep("", nrow(NonHaroinSlope.WV)-length(slope.no.WV2)))
NonHaroinSlope.WV$slope.no.WV3<-c(slope.no.WV3,rep("", nrow(NonHaroinSlope.WV)-length(slope.no.WV3)))
NonHaroinSlope.WV$slope.no.WV4<-c(slope.no.WV4,rep("", nrow(NonHaroinSlope.WV)-length(slope.no.WV4)))
NonHaroinSlope.WV$slope.no.WV5<-c(slope.no.WV5,rep("", nrow(NonHaroinSlope.WV)-length(slope.no.WV5)))


write.csv(NonHaroinSlope.WV,"NonHaroinSlope.WV.csv")
