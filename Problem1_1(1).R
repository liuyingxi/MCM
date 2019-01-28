df<-read.csv("MCM_NFLIS_Data.csv")
df.VA<-subset.data.frame(df,df$State=="VA")
df.OH<-subset.data.frame(df,df$State=="OH")
df.PA<-subset.data.frame(df,df$State=="PA")
df.KY<-subset.data.frame(df,df$State=="KY")
df.WV<-subset.data.frame(df,df$State=="WV")
df.state <- unique(data.frame(Year=df$YYYY,State=df$State,TotalDrug=df$TotalDrugReportsState))
df.county <- unique(data.frame(Year=df$YYYY,State=df$State,County=df$COUNTY,TotalDrugC=df$TotalDrugReportsCounty,TotalDrugS=df$TotalDrugReportsState))

df.state.VA <- unique(data.frame(Year=df.VA$YYYY,TotalDrug=df.VA$TotalDrugReportsState))
df.state.OH <- unique(data.frame(Year=df.OH$YYYY,TotalDrug=df.OH$TotalDrugReportsState))
df.state.WV <- unique(data.frame(Year=df.WV$YYYY,TotalDrug=df.WV$TotalDrugReportsState))
df.state.KY <- unique(data.frame(Year=df.KY$YYYY,TotalDrug=df.KY$TotalDrugReportsState))
df.state.PA <- unique(data.frame(Year=df.PA$YYYY,TotalDrug=df.PA$TotalDrugReportsState))

#OH
T79 <- 1:8
Tdelt <- (0:80)/10
TotaDrug_State <- df.state.OH$TotalDrug
CuDrug <- cumsum(TotaDrug_State)
Bass.nls1 <- nls(TotaDrug_State ~ 120000*(1/(1+exp(-T79+5))) + M * (((P + Q)^2/P) * exp(-(P + Q) * T79))/(1 + (Q/P)*exp(-(P + Q) * T79))^2, start = list(M = 12000000, P = 0.03, Q = 0.03))
summary(Bass.nls1)
Bcoef1 <- coef(Bass.nls1)
m <- Bcoef1[1]
p <- Bcoef1[2]
q <- Bcoef1[3]
ngete <- exp(-(p + q) * (Tdelt))
Bpdf <- m * ((p + q)^2/p) * (ngete)/(1 + (q/p) * (ngete))^2
Apdf <- 120000*(1/(1+exp(-(Tdelt)+5)))
plot(Tdelt, (Apdf+Bpdf), xlab = "Year from 2010 to 2017", ylab = "Total DrugReports State", type = "l",main="Distribution in OH",col = "red",lwd = 5,xlim = c(1,8))
T79 <- 2010:2017
points(T79, TotaDrug_State,type = "h", lwd = 20,col = "blue")
#CDF
Bcdf <- m * (1 - ngete)/(1 + (q/p) * ngete)
plot(Tdelt, Bcdf, xlab = "Year from 1979", ylab = "Cumulative sales", type = "l")
points(T79, CuDrug)

#OH
T79 <- 2010:2017
Tdelt <- (20100:20170)/10
Bass.nls2 <- lm(TotalDrug ~ Year,data=df.state.OH)
summary(Bass.nls2)
Bcoef2 <- coef(Bass.nls2)
b<-coef(summary(Bass.nls2))[1,1]
k<-coef(summary(Bass.nls2))[2,1]
Bpdf <- k*Tdelt+b
plot(Tdelt, Bpdf, xlab = "Year from 2010 to 2017", ylab = "Total DrugReports State", type = "l",ylim=c(0,130000),main="Distribution in OH",col = "red",lwd = 5)
points(T79, df.state.OH$TotalDrug,type = "h", lwd = 20,col = "blue")

#KY
T79 <- 2010:2017
Tdelt <- (20100:20170)/10
Bass.nls2 <- lm(TotalDrug ~ Year,data=df.state.KY)
summary(Bass.nls2)
Bcoef2 <- coef(Bass.nls2)
b<-coef(summary(Bass.nls2))[1,1]
k<-coef(summary(Bass.nls2))[2,1]
Bpdf <- k*Tdelt+b
plot(Tdelt, Bpdf, xlab = "Year from 2010 to 2017", ylab = "Total DrugReports State", type = "l",ylim=c(0,30000),main="Distribution in KY",col = "red",lwd = 5, ylim=c(0,125000))
points(T79, df.state.KY$TotalDrug,type = "h", lwd = 20,col = "blue")

#VA

T79 <- 2010:2017
Tdelt <- (20100:20170)/10
Bass.nls2 <- lm(TotalDrug ~ Year,data=df.state.VA)
summary(Bass.nls2)
Bcoef2 <- coef(Bass.nls2)
b<-coef(summary(Bass.nls2))[1,1]
k<-coef(summary(Bass.nls2))[2,1]
Bpdf <- k*Tdelt+b
plot(Tdelt, Bpdf, xlab = "Year from 2010 to 2017", ylab = "Total DrugReports State", type = "l",ylim=c(0,50000),main="Distribution in VA",col = "red",lwd = 5)
points(T79, df.state.VA$TotalDrug,type = "h", lwd = 20,col = "blue")

#PA
T79 <- 2010:2017
Tdelt <- (20100:20170)/10
Bass.nls2 <- lm(TotalDrug ~ Year,data=df.state.PA)
summary(Bass.nls2)
Bcoef2 <- coef(Bass.nls2)
b<-coef(summary(Bass.nls2))[1,1]
k<-coef(summary(Bass.nls2))[2,1]
Bpdf <- k*Tdelt+b
plot(Tdelt, Bpdf, xlab = "Year from 2010 to 2017", ylab = "Total DrugReports State", type = "l",ylim=c(0,100000),main="Distribution in PA",col = "red",lwd = 5)
points(T79, df.state.PA$TotalDrug,type = "h", lwd = 20,col = "blue")


#WV
T79 <- 2010:2017
Tdelt <- (20100:20170)/10
Bass.nls2 <- lm(TotalDrug ~ Year,data=df.state.WV)
summary(Bass.nls2)
Bcoef2 <- coef(Bass.nls2)
b<-coef(summary(Bass.nls2))[1,1]
k<-coef(summary(Bass.nls2))[2,1]
Bpdf <- k*Tdelt+b
plot(Tdelt, Bpdf, xlab = "Year from 2010 to 2017", ylab = "Total DrugReports State", type = "l",ylim=c(0,10000),main="Distribution in WV",col = "red",lwd = 5)
points(T79, df.state.WV$TotalDrug,type = "h", lwd = 20,col = "blue")

