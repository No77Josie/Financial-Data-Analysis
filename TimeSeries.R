# Financial time series
setwd("/Users/wujiaqi/Desktop/FIN3380/Data")
library(dplyr)

# 1. get data
crude = read.csv("crude_oil.csv")
head(crude)
str(crude)  # always check the class
crude = as_tibble(crude)
head(crude)
crude$DATE=as.Date(crude$DATE)
colnames(crude)=c("date","price")
head(crude)
# calculate log return
crude = crude %>%
  arrange(date) %>%
  mutate(ret=log(price)-log(dplyr::lag(price,1))) %>%
  na.omit()
head(crude)

# 2. visualise the returns and ACF function
par(mfcol=c(2,1))  # plot multiple charts, plot 1 in (1,1), plot 2 in (2,1)
plot(crude$ret,xlab="month",ylab="returns",type="l",
     main="(a): Simple returns")
# ACF function:
crude.acf = acf(crude$ret,lag=24,plot=F)
head(crude.acf$lag)
crude.acf$lag = crude.acf$lag/12 # change month into year
plot(crude.acf)
# lag0 always equals to 1
# blue line: 2 sd range - beyond the range means that there is autocorrelation

# 3. calculate the t stats to test the relationship
# take lag1 as example:
crude.acf$acf
crude.acf$n.used
tstat = crude.acf$acf[2]*sqrt(crude.acf$n.used)
tstat
# t quite large, so autocorrelation equals to 0

# 4. Joint test for the serial autocorrelation
Box.test(crude$ret, lag=12, type="Ljung")
# p is quite small, reject H0
save(crude,file="crude.RData")



# GNP AR model
library(zoo)
# 1. load data and visualise its growth
da = as_tibble(read.table("q-gnp4710.txt",header = T))
head(da)
da = da %>%
  mutate(lg=log(VALUE),
         gnp.g=lg-lag(lg,1)) %>%
  na.omit()
head(da)
da = da %>%
  mutate(date=as.yearmon(paste0(Year,'-',Mon)))
head(da)
par(mfcol=c(2,1))
with(da,plot(lg~date,xlab="Year",ylab="GNP",type="l")) # expontential:log is linear
with(da,plot(gnp.g,type="l",xlab="Year",ylab="growth"))

# 2. look into the PACF to select the AR model
acf(da$gnp.g,lag=12)    # every single factor effect without considering others
pacf(da$gnp.g,lag=12)    # control for the rest, the added factor effect
# for PACF: we should stop when the factor is no longer significant
# from pacf: significant up tp lag 3, we use AR(3) model
m1 = arima(da$gnp.g,order=c(3,0,0))
m1  # get AIC

# 3. look into the IC to select the AR model
AIC(m1)
aic = NULL
bic = NULL
for (i in 0:12){
  tmp=arima(da$gnp.g,c(i,0,0))
  tmpaic=AIC(tmp)
  tmpbic=BIC(tmp)
  aic=append(aic,tmpaic)
  bic=append(bic,tmpbic)
}
# remember to -1 cuz we start from 0
which(aic==min(aic))-1
which(bic==min(bic))-1
# visualize:
par(mfcol=c(2,1),mar=c(2,3,3,3))  # mar: margin for the figure
# -min for easier visualization
plot(aic-min(aic)~seq(0,length(aic)-1),type="h",   # h: horizontal line
     xlab="",ylab="",main="AIC")
lines(seq(0,length(aic)-1),aic-min(aic),lty=2)

plot(bic-min(bic)~seq(0,length(bic)-1),type="h",
     xlab="",ylab="",main="BIC")
lines(seq(0,length(bic)-1),bic-min(bic),lty=2)

# 4. Examine the coefficients of the AR(3) model
gnp.g.ar3 = arima(da$gnp.g,c(3,0,0))
gnp.g.ar3
gnp.g.ar9 = arima(da$gnp.g,c(9,0,0))
gnp.g.ar9
# the intercept is the E(Xt)

# 5. modified AR(9) model
# lag 4,5,6,7,8 not significant: restrict the coefficients to 0
# in arima(): fixed = NA (estimation, significnat), fixed = 0 (fix the parameter to 0)
const = rep(NA,10)
const[4:8]=0
gnp.g.ar9.const = arima(da$gnp.g, order=c(9,0,0), fixed=const)
gnp.g.ar9.const # lower AIC

# 6. model checking for residual serial correlations
Box.test(gnp.g.ar3$residuals,lag=12,type="Ljung")
# p larger than 5%: the serial correlation is acceptable already
Box.test(gnp.g.ar9$residuals,lag=12,type="Ljung")
# p larger than 10%: definitely sufficient enough
Box.test(gnp.g.ar9.const$residuals,lag=12,type="Ljung")
# the serial correlation is removed

# 7. 3 year (12 quarter) forecast
gnp.g.f = data.frame(date=da$date,
                     gnp.g=da$gnp.g,
                     pred=da$gnp.g,
                     pred.up=da$gnp.g,
                     pred.bt=da$gnp.g)
head(gnp.g.f)

mf = arima(gnp.g.f$gnp.g,c(3,0,0))   # build the model
mf
mf$coef
mfpred = predict(mf,n.ahead = 12)
library(lubridate)
testdt = last(gnp.g.f$date)
testdt
testdt = as.Date(testdt)
testdt
seq(testdt,by="quarter",length=3)
testdt+months(3)
tmpdt = seq(as.Date(last(gnp.g.f$date))+months(3),by="quarter",length=12)
tmpdt = as.yearmon(tmpdt)

tmp = data.frame(date=tmpdt,
                 gnp.g=NA,
                 pred=mfpred$pred,
                 pred.up=mfpred$pred+2*mfpred$se,
                 pred.bt=mfpred$pred-2*mfpred$se)
head(tmp)

gnp.g.f = rbind(gnp.g.f,tmp)   # combine the previous together with prediction
gnp.g.f = tail(gnp.g.f,20)
head(gnp.g.f)
tail(gnp.g.f)

plot(gnp.g.f$pred~gnp.g.f$date, col="red",
     ylim=range(gnp.g.f[,seq(2,5)],na.rm=T),type="l",
     lwd=2,main="GNP Growth Forecast",
     xlab="year",ylab="GNP growth")
lines(gnp.g.f$pred.up~gnp.g.f$date,lty=3)
lines(gnp.g.f$pred.bt~gnp.g.f$date,lty=3)
lines(gnp.g.f$gnp.g~gnp.g.f$date,lwd=2)

# 8. Test for unit root in the GNP data
library(fUnitRoots)
adfTest(da$VALUE)
# p-value is large, cannot reject H0, unit root exists, not stantionary
adfTest(da$gnp.g)
# p is small, no unit root, stationary


# Cointegration

# 1. load stocks and log price
require(dplyr,quietly = T,warn.conflicts = F)
require(tidyr,quietly = T,warn.conflicts = F)
require(urca,quietly = T,warn.conflicts = F)
load("crsp.sample.RData")
head(crsp.sample)

crsp.sample = crsp.sample %>%
  mutate(logprice=log(abs(prc))) %>%    # prc is negative indicating it is not real prc
  select(permno,date,logprice) %>%
  na.omit()
head(crsp.sample)

# construct all potential pairs and examine the cointegration
permno = unique(crsp.sample$permno)
permno
comb = data.frame(t(combn(permno,2)))  # combn(): all pairs
comb

t1.name = comb$X1[1]
t2.name = comb$X2[1]

df = crsp.sample
df = df[which(df$permno %in% c(t1.name,t2.name)),]
head(df)
df = spread(df,permno,logprice)

df = df[complete.cases(df),]   # use two permnos both have values
ts = df[,c(2,3)]
head(ts)

# test the cointegration
tmp = ca.jo(ts)
j = summary(tmp)
j
# r=0: means no linear dependency
# tstat is small, we can not reject H0, no cointefration in this pair
j@teststat  # t-stat
j@cval  # critical values

r = data.frame(stock1=t1.name, stock.2=t2.name, stat=j@teststat[2])
r
r[,c("pct10","pct5","pct1")]=j@cval[2,]

# function:
jtest = function(t1.name,t2.name,df){
  df = df[which(df$permno %in% c(t1.name,t2.name)),]
  df = spread(df,permno,logprice)
  df = df[complete.cases(df),]
  ts = df[,c(2,3)]
  j = summary(ca.jo(ts))
  r = data.frame(stock1=t1.name,stock2=t2.name,
                 stat=j@teststat[2])
  r[,c("pct10","pct5","pct1")] = j@cval[2,]
  return(r) }
jtest(t1.name,t2.name,df=crsp.sample)

# 3. test for all pairs
pkgs = list("doParallel","foreach")
# lapply(): apply function to each element in a list
lapply(pkgs, require,character.only=T,quietly=T,warn.conflicts=T)
# character.only=T: we can regard the character as package

# how many cores = the maximum amount we can seperate our tasks
detectCores(all.tests = FALSE,logical = TRUE)
# tell computer how many tasks to perform at the same time
registerDoParallel(cores = 4)
stat = foreach(i=1:nrow(comb),
               .combine = rbind,    # how to get results together
               .packages = c("urca","tidyr")) %dopar%  # all necessary package, ### %dopar%
  jtest(as.character(comb[i,1]),as.character(comb[i,2]),df=crsp.sample)
head(stat)
stat = stat[order(-stat$stat),]
head(stat)

# 4. select cointegrated pairs
copair = stat %>%
  dplyr::filter(stat>pct1) %>%
  mutate_at(vars(stock1,stock2),as.character)
head(copair)

goodpair = crsp.sample %>%
  dplyr::filter(permno %in% c(copair$stock1,copair$stock2))
head(goodpair)
goodpair = spread(goodpair,permno,logprice)
head(goodpair)

# 5. get hedge ratio: linear regression
est = lm(goodpair[,2]~goodpair[,3],data=goodpair)
summary(est)$coef
goodpair$spread = est$residuals
plot(goodpair$spread~goodpair$date,type="l")

# check the spread is no longer has unit root
require(fUnitRoots)
adfTest(goodpair$spread)
# p is so small, reject H0, no unit root, right pairs


# 6. create buy sell signal and calculate the return
# assumption: spread is Gaussian distributed
sd.spread = sd(goodpair$spread)














