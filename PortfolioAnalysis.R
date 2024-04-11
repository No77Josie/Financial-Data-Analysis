# portfolio analysis
setwd("/Users/wujiaqi/Desktop/FIN3380/Data")
library(dplyr)
library(zoo)

load("a.shares.RData")
head(a.shares)
size.ret = a.shares %>%
  select(stock.id,yymm,mcap,ret,mkt) %>%
  mutate(ret.mkt=ret-mkt)
head(size.ret)

# 1. Break points
p=c(0.1,0.2,0.4,0.6,0.8,0.9)
library(tidyr)
bp = size.ret %>%
  group_by(yymm) %>%
  summarise(qt=list(paste0(p*100,"%")),
            break.point=list(quantile(mcap,p,type=3)))
head(bp)
bp = bp %>%
  unnest(cols=c(qt,break.point)) %>%
  ungroup()
head(bp)
# visualize:
library(ggplot2)
library(zoo)
ggplot(data=bp,
       aes(x=as.yearmon(yymm),
           y=break.point,
           col=as.factor(qt))) +
  scale_x_yearmon() +
  geom_line() +
  labs(title="Break Points: size",
       y="Size",
       x="Date")
bp = spread(bp,"qt","break.point")
head(bp)

# 2. group the entities in the sample into portfolios

v = seq(1,10)
v
cut(v,breaks = c(1,3,5,10),include.lowest = T)
# show the results of which group each point belongs to
cut(v,breaks=c(3,1,5,10), include.lowest = T, ordered_result = T)
# ordered_result: the result is in order
cut(v,breaks = c(1,3,5,10),include.lowest = T, labels = c("1-3","3-5","5-10"))

# take 1st month as example:
eg = size.ret %>% filter(yymm==first(yymm))
head(eg)
mybreaks = quantile(eg$mcap,p,type=3)
mybreaks
mymin=min(eg$mcap)
mymax=max(eg$mcap)
mymin; mymax; mybreaks
mybreaks=c(mymin,mybreaks,mymax)
mybreaks
eg$port = cut(eg$mcap,breaks = mybreaks, include.lowest = T, ordered_result = T,
              labels = seq(1,7))
head(eg)
# for all months:
port = size.ret %>%
  group_by(yymm) %>%
  mutate(size.port=cut(mcap,
                       breaks = c(min(mcap),quantile(mcap,p,type=3),max(mcap)),
                       include.lowest = T, ordered_result = T,
                       labels = seq(1,7))) %>%
  ungroup()
head(port)
tail(port)
# summarise the number of stocks:
n.port = port %>%
  group_by(yymm) %>%
  mutate(n.total=n()) %>%
  ungroup()
head(n.port)
n.port = n.port %>%
  group_by(yymm,size.port) %>%
  summarise(n.total=first(n.total),
            n=n()) %>%
  ungroup()
head(n.port)
n.port.wide = spread(n.port,"size.port","n")
head(n.port.wide)
# visualize:
ggplot(data=n.port,
       aes(y=n,
           x=as.yearmon(yymm),
           fill=size.port)) +
  geom_bar(stat="identity") +
  scale_x_yearmon() +
  labs(x="Date",
       y="# of stocks",
       title="Num of stock in size portfolio")
ggplot(data = n.port, aes(y=n/n.total, x=as.yearmon(yymm), fill=size.port)) +  
  geom_bar(stat="identity") + scale_x_yearmon() +
  labs(x='Date',
       y='# of Stocks',
       title='Num of Stock in Size Portfolio/Total Num of Stocks')

# 3. Average portfolio values
head(size.ret)
ret.m = size.ret %>% select(stock.id,yymm,ret.mkt)
head(ret.m)
ret.m = spread(ret.m,"stock.id","ret.mkt")
ret.m = gather(ret.m,key="stock.id",value="ret.mkt",-yymm)
head(ret.m,10)

ret.m = ret.m %>%
  group_by(stock.id) %>%
  arrange(yymm) %>%
  mutate(ret.mkt.lead.m1=dplyr::lead(ret.mkt,1)) %>%   # lead: merge the following one
  ungroup() %>%
  arrange(stock.id,yymm)
head(ret.m)

ret.m = ret.m %>% select(yymm,stock.id,ret.mkt.lead.m1)
head(ret.m)

head(port)
port = port %>% inner_join(ret.m,by=c("stock.id","yymm"))
head(port)

port.ret = port %>%
  group_by(yymm,size.port) %>%
  summarise(ret.ew=mean(ret.mkt.lead.m1,na.rm = T)) %>%
  ungroup() %>%
  arrange(yymm,size.port)
head(port.ret)

ggplot(data=port.ret,
       aes(x=as.yearmon(yymm),
           y=ret.ew,
           col=size.port))+
  geom_line() +
  scale_x_yearmon() + 
  labs(x="Date",y="Monthly Excess Return",
       title="Portfolio Monthly Excess Return Formed by Size")

# calculate the difference portfolio
port.ret.wide = spread(port.ret,"size.port","ret.ew")
port.ret.wide$"7-1" = port.ret.wide$'7'-port.ret.wide$'1'
head(port.ret.wide)
port.ret = gather(port.ret.wide, size.port,ret.ew,-yymm)
head(port.ret)
tail(port.ret)

# 4. examine the variation in these average values of y across different portfolios
summary.stat = port.ret %>%
  group_by(size.port) %>%
  na.omit() %>%
  summarise(average=mean(ret.ew),
            n=n(),
            standard.error=sd(ret.ew)/sqrt(n),
            t.stat=average/standard.error,
            p.value=2*(1-pnorm(abs(t.stat))))
summary.stat
with(summary.stat,
     barplot(average,names.arg = size.port))
port.ret = port.ret %>% 
  mutate(ret.ew = ifelse(size.port == '7-1',-ret.ew,ret.ew)) %>%
  mutate(size.port = ifelse(size.port == '7-1','1-7',size.port)) %>%
  group_by(size.port) %>% arrange(yymm) %>%
  mutate(cumret = cumprod(ret.ew+1)) %>% ungroup()

ggplot(data=port.ret,
       aes(x=as.yearmon(yymm),
           y=cumret,
           col=size.port))+
  geom_line() +
  scale_x_yearmon()


