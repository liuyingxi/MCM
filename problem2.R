df<-read.csv("MCM_NFLIS_Data.csv")
df10<-read.csv("ACS_10_5YR_DP02_with_ann.csv")
df11<-read.csv("ACS_11_5YR_DP02_with_ann.csv")
df12<-read.csv("ACS_12_5YR_DP02_with_ann.csv")
df13<-read.csv("ACS_13_5YR_DP02_with_ann.csv")
df14<-read.csv("ACS_14_5YR_DP02_with_ann.csv")
df15<-read.csv("ACS_15_5YR_DP02_with_ann.csv")
df16<-read.csv("ACS_16_5YR_DP02_with_ann.csv")

lp1<-1
d01<-c()
d1<-df10
d2<-df16

for(lp1 in (1:dim(d1)[2]-3)){
  lp2<-1
  sum2<-0
  for (lp2 in 1:(dim(d2)[2])-3) {
    if(names(d1)[lp1+3]==names(d2)[lp2+3]){
      sum2=sum2+1
    }
    lp2=lp2+1
  }
  d01[lp1]<-sum2
}

dd<-c()
lp3<-1
for(lp3 in 1:length(d01)){
  if(sum(d01[lp3]+d02[lp3]+d03[lp3]+d04[lp3]+d05[lp3]+d06[lp3])==6){
    dd[lp3]=1
  }else{
    dd[lp3]=0
  }
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(dd) ) {
  if( dd[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex


var_new1<-df10[-(dindex+3)]
var_new2<-var_new1[-c(1,2,3)]
var_new3<-var_new2[-c(2+4*(0:110),3+4*(0:110),4+4*(0:110))]

#create new data frames that contain the variables we want only
  #d01
lp5=1
output1<-c()
for(lp5 in 1:dim(df10)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df10)[lp5+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output1[lp5]<-summ
  lp5=lp5+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output1) ) {
  if( output1[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df10[-(dindex+3)]
new_df10<-var_new01[-c(1,3)]

  #d02
lp6=1
output2<-c()
for(lp6 in 1:dim(df11)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df11)[lp6+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output2[lp6]<-summ
  lp6=lp6+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output2) ) {
  if( output2[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df10[-(dindex+3)]
new_df11<-var_new01[-c(1,3)]

  #d03
lp7=1
output3<-c()
for(lp7 in 1:dim(df12)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df12)[lp7+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output3[lp7]<-summ
  lp7=lp7+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output3) ) {
  if( output3[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df12[-(dindex+3)]
new_df12<-var_new01[-c(1,3)]

  #d04
lp8=1
output4<-c()
for(lp8 in 1:dim(df13)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df13)[lp8+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output4[lp8]<-summ
  lp8=lp8+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output4) ) {
  if( output4[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df13[-(dindex+3)]
new_df13<-var_new01[-c(1,3)]

  #d05
lp9=1
output5<-c()
for(lp9 in 1:dim(df14)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df14)[lp9+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output5[lp9]<-summ
  lp9=lp9+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output5) ) {
  if( output5[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df14[-(dindex+3)]
new_df14<-var_new01[-c(1,3)]
names(new_df14)==names(new_df10)

  #d06
lp91=1
output6<-c()
for(lp91 in 1:dim(df15)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df15)[lp91+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output6[lp91]<-summ
  lp91=lp91+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output6) ) {
  if( output6[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df15[-(dindex+3)]
new_df15<-var_new01[-c(1,3)]
names(new_df15)==names(new_df10)
  #d07
lp92=1
output7<-c()
for(lp92 in 1:dim(df16)[2]-3){
  lpp<-1
  summ<-0
  for (lpp in 1:length(var_new3)) {
    if(names(df16)[lp92+3]==names(var_new3)[lpp]){
      summ=summ+1
    }
    lpp=lpp+1
  }
  output7[lp92]<-summ
  lp92=lp92+1
}

lp4=1
ind=0
dindex<-c()
for (lp4 in 1:length(output7) ) {
  if( output7[lp4]==0 ){
    ind=ind+1
    dindex[ind]<-lp4
  }
  lp4=lp4+1
}
dindex
var_new01<-df16[-(dindex+3)]
new_df16<-var_new01[-c(1,3)]
names(new_df16)==names(new_df10)
#

loop1<-1
ddd<-c()
d1<-new_df10
d2<-new_df15

for(loop1 in (1:dim(d1)[1])){
  loop2<-1
  sum22<-0
  for (loop2 in 1:(dim(d2)[1])) {
    if((d1$GEO.id2)[loop1]==d2$GEO.id2[loop2]){
      sum22=sum22+1
    }
    loop2=loop2+1
  }
  ddd[loop1]<-sum22
}
ddd

new_df10<-new_df10[-c(373),]
new_df11<-new_df11[-c(373),]
new_df12<-new_df12[-c(373),]
new_df13<-new_df13[-c(373),]

new_df10<-new_df10[-c(1),]
new_df11<-new_df11[-c(1),]
new_df12<-new_df12[-c(1),]
new_df13<-new_df13[-c(1),]
new_df14<-new_df14[-c(1),]
new_df15<-new_df15[-c(1),]
new_df16<-new_df16[-c(1),]

t2010<-subset.data.frame(df,df$YYYY=="2010")
t2011<-subset.data.frame(df,df$YYYY=="2011")
t2012<-subset.data.frame(df,df$YYYY=="2012")
t2013<-subset.data.frame(df,df$YYYY=="2013")
t2014<-subset.data.frame(df,df$YYYY=="2014")
t2015<-subset.data.frame(df,df$YYYY=="2015")
t2016<-subset.data.frame(df,df$YYYY=="2016")
#find the missed value
lp<-1
dd1<-new_df10$GEO.id2
dd2<-t2010$FIPS_Combined
lp1=0
out1<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out1[lp1]<-lp
  }
  lp=lp+1
}
out1

lp<-1
dd1<-new_df11$GEO.id2
dd2<-t2011$FIPS_Combined
lp1=0
out2<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out2[lp1]<-lp
  }
  lp=lp+1
}
out2

lp<-1
dd1<-new_df12$GEO.id2
dd2<-t2012$FIPS_Combined
lp1=0
out3<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out3[lp1]<-lp
  }
  lp=lp+1
}
out3

lp<-1
dd1<-new_df13$GEO.id2
dd2<-t2013$FIPS_Combined
lp1=0
out4<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out4[lp1]<-lp
  }
  lp=lp+1
}
out4

lp<-1
dd1<-new_df14$GEO.id2
dd2<-t2014$FIPS_Combined
lp1=0
out5<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out5[lp1]<-lp
  }
  lp=lp+1
}
out5

lp<-1
dd1<-new_df15$GEO.id2
dd2<-t2015$FIPS_Combined
lp1=0
out6<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out6[lp1]<-lp
  }
  lp=lp+1
}
out6

lp<-1
dd1<-new_df16$GEO.id2
dd2<-t2016$FIPS_Combined
lp1=0
out7<-c()
for(lp in 1:length(dd1)){
  if(length(which(dd1[lp]==dd2))==0){
    lp1=lp1+1
    out7[lp1]<-lp
  }
  lp=lp+1
}
out7

out<-unique(c(out1,out2,out3,out4,out5,out6,out7))
new_df10<-new_df10[-c(out),]
new_df11<-new_df11[-c(out),]
new_df12<-new_df12[-c(out),]
new_df13<-new_df13[-c(out),]
new_df14<-new_df14[-c(out),]
new_df15<-new_df15[-c(out),]
new_df16<-new_df16[-c(out),]

find.target<-function(data1,data2,data3){
  floop<-1
  foutput<-c()
  for(floop in 1: length(data1)){
    foutput[floop]<-unique(data3[which(data2==data1[floop])])
    floop=floop+1
  }
  return(foutput)
}
target2010<-find.target(new_df10$GEO.id2,t2010$FIPS_Combined,t2010$TotalDrugReportsCounty)
target2011<-find.target(new_df11$GEO.id2,t2011$FIPS_Combined,t2011$TotalDrugReportsCounty)
target2012<-find.target(new_df12$GEO.id2,t2012$FIPS_Combined,t2012$TotalDrugReportsCounty)
target2013<-find.target(new_df13$GEO.id2,t2013$FIPS_Combined,t2013$TotalDrugReportsCounty)
target2014<-find.target(new_df14$GEO.id2,t2014$FIPS_Combined,t2014$TotalDrugReportsCounty)
target2015<-find.target(new_df15$GEO.id2,t2015$FIPS_Combined,t2015$TotalDrugReportsCounty)
target2016<-find.target(new_df16$GEO.id2,t2016$FIPS_Combined,t2016$TotalDrugReportsCounty)

target2010
target2011
target2012
target2013
target2014
target2015
target2016

new_df10$target2010<-target2010
new_df11$target2011<-target2011
new_df12$target2012<-target2012
new_df13$target2013<-target2013
new_df14$target2014<-target2014
new_df15$target2015<-target2015
new_df16$target2016<-target2016
new_df10<-unfactor(new_df10)
new_df11<-unfactor(new_df11)
new_df12<-unfactor(new_df12)
new_df13<-unfactor(new_df13)
new_df14<-unfactor(new_df14)
new_df15<-unfactor(new_df15)
new_df16<-unfactor(new_df16)
new_df10<-new_df10[-c(56,57,58)]
new_df11<-new_df11[-c(56,57,58)]
new_df12<-new_df12[-c(56,57,58)]
new_df13<-new_df13[-c(56,57,58)]
new_df14<-new_df14[-c(56,57,58)]
new_df15<-new_df15[-c(56,57,58)]
new_df16<-new_df16[-c(56,57,58)]
library(varhandle)

library(MASS)
fit  <- lm(target2010 ~.-GEO.id2,data=new_df10)
step <- stepAIC(fit, direction="both")
delet.list<-which(summary(step)$coeff[,4]>0.01)
list1<-summary(step)$coeff[,1]
list1<-list1[-c(delet.list)]
waiting.list1<-names(list1)

fit  <- lm(target2011 ~.-GEO.id2,data=new_df11)
step <- stepAIC(fit, direction="both")
delet.list<-which(summary(step)$coeff[,4]>0.01)
list2<-summary(step)$coeff[,1]
list2<-list2[-c(delet.list)]
waiting.list2<-names(list2)

fit1  <- lm(target2012 ~.-GEO.id2,data=new_df12)
step <- stepAIC(fit1, direction="both")
delet.list<-which(summary(step)$coeff[,4]>0.01)
list3<-summary(step)$coeff[,1]
list3<-list3[-c(delet.list)]
waiting.list3<-names(list3)

fit  <- lm(target2013 ~.-GEO.id2,data=new_df13)
step <- stepAIC(fit, direction="both")
delet.list<-which(summary(step)$coeff[,4]>0.01)
list4<-summary(step)$coeff[,1]
list4<-list4[-c(delet.list)]
waiting.list4<-names(list4)

fit  <- lm(target2014 ~.-GEO.id2,data=new_df14)
step <- stepAIC(fit, direction="both")
delet.list<-which(summary(step)$coeff[,4]>0.01)
list5<-summary(step)$coeff[,1]
list5<-list5[-c(delet.list)]
waiting.list5<-names(list5)

fit  <- lm(target2015 ~.-GEO.id2,data=new_df15)
step <- stepAIC(fit, direction="both")
delet.list<-which(summary(step)$coeff[,4]>0.01)
list6<-summary(step)$coeff[,1]
list6<-list6[-c(delet.list)]
waiting.list6<-names(list6)

waiting.list<-intersect(waiting.list1,waiting.list2)

clean.v<-function(data1,data2){
lpp1<-1
outout<-c()
  for(lpp1 in 1:length(data2)){
    outout[lpp1]<-which(names(data1)==data2[lpp1])
  }
return(outout)
}
list1<-clean.v(new_df10,waiting.list)
new_df10<-new_df101
list2<-clean.v(new_df11,waiting.list)
new_df11<-new_df11[c(list2[2:37],110)]
list3<-clean.v(new_df12,waiting.list)
new_df12<-new_df12[c(list2[2:37],110)]
list4<-clean.v(new_df13,waiting.list)
new_df13<-new_df13[c(list4[2:37],110)]
list5<-clean.v(new_df14,waiting.list)
new_df14<-new_df14[c(list5[2:37],110)]
list6<-clean.v(new_df15,waiting.list)
new_df15<-new_df15[c(list6[2:37],110)]
list7<-clean.v(new_df16,waiting.list)
new_df16<-new_df16[c(list7[2:37],110)]

i<-1
val<-c()
for(i in 1:dim(new_df101)[1]){
  val[i]<-which(new_df101[i,1]==var_new01$HC01_VC03)
  i=i+1
}
val
value<-var_new01$GEO.id2[val]
new_df101$id<-value

i1<-1
val1<-c()
for(i1 in 1:dim(new_df101)[1]){
  val1[i1]<-which(new_df101[i1,37]==t2010$FIPS_Combined)
  i1=i1+1
}
val1
value1<-t2010$TotalDrugReportsCounty[val1]
new_df101$target2010<-value1
new_df10=new_df102

new_df10<-unfactor(new_df10)
new_df11<-unfactor(new_df11)
new_df12<-unfactor(new_df12)
new_df13<-unfactor(new_df13)
new_df14<-unfactor(new_df14)
new_df15<-unfactor(new_df15)
new_df16<-unfactor(new_df16)

fit1  <- lm(target2010 ~.,data=new_df10)
coef(fit1)
fit2  <- lm(target2011 ~.,data=new_df11)
coef(fit2)
fit3  <- lm(target2012 ~.,data=new_df12)
coef(fit3)
fit4  <- lm(target2013 ~.,data=new_df13)
coef(fit4)
fit5  <- lm(target2014 ~.,data=new_df14)
coef(fit5)
fit6  <- lm(target2015 ~.,data=new_df15)
coef(fit6)
fit7  <- lm(target2016 ~.,data=new_df16)
coef(fit7)

rloop<-1
rv<-c()
for(rloop in 1: length(coef(fit1))){
  if(coef(fit1)[rloop]>0&coef(fit2)[rloop]>0){
    rv[rloop]=1
  }else if(coef(fit1)[rloop]<0&coef(fit2)[rloop]<0){
    rv[rloop]=1
  }else{rv[rloop]=0}
  
}
rv


clean_data10<-new_df10
clean_data11<-new_df11
clean_data12<-new_df12
clean_data13<-new_df13
clean_data14<-new_df14
clean_data15<-new_df15
clean_data16<-new_df16

sele.var<-names(coef(fit1)[c(2:8,10,12:37)])

get.new.var<-function(data1,data2){
  gloop<-1
  gout <-c()
  for(gloop in 1:length(data1)){
    gout[gloop]<-which(data1[gloop]==names(data2))
    gloop=gloop+1
  }
  finalout<-data2[c(gout,37)]
  return(finalout)
}
clean_data2010<-get.new.var(sele.var,clean_data10)
clean_data2011<-get.new.var(sele.var,clean_data11)
clean_data2012<-get.new.var(sele.var,clean_data12)
clean_data2013<-get.new.var(sele.var,clean_data13)
clean_data2014<-get.new.var(sele.var,clean_data14)
clean_data2015<-get.new.var(sele.var,clean_data15)
clean_data2016<-get.new.var(sele.var,clean_data16)

PCA2010<-prcomp(clean_data10[,c(1:36)])
PCA2010_2<-print(PCA2010)
str(PCA2010)
summary(PCA2010)
plot(PCA2010,type="l")

PCA2011<-prcomp(clean_data11[,c(1:36)])
PCA2011_2<-print(PCA2011)
str(PCA2011)
plot(PCA2011,type="l")

PCA2012<-prcomp(clean_data12[,c(1:36)])
PCA2012_2<-print(PCA2012)
str(PCA2012)
plot(PCA2012,type="l")

PCA2013<-prcomp(clean_data13[,c(1:36)])
PCA2013_2<-print(PCA2013)
str(PCA2013)
plot(PCA2013,type="l")

PCA2014<-prcomp(clean_data14[,c(1:36)])
PCA2014_2<-print(PCA2014)
str(PCA2014)
plot(PCA2014,type="l")

PCA2015<-prcomp(clean_data15[,c(1:36)])
PCA2015_2<-print(PCA2015)
str(PCA2015)
plot(PCA2015,type="l")

PCA2016<-prcomp(clean_data16[,c(1:36)])
PCA2016_2<-print(PCA2016)
str(PCA2016)
plot(PCA2016,type="l")


