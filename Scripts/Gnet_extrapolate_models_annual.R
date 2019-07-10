library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(iterators)
library(Metrics)

registerDoParallel(cores = 8)

precip <- read_csv("big_data/precip.csv") %>% rename(Precip=Rain) %>% 
  rename(Date = time) %>% 
  select(WBIC,Date,Precip) %>% 
  mutate(Date = as.Date(Date))
precip <- precip %>% filter(year(Date)>=1981)
evap <- read_csv("big_data/evap.csv") %>% rename(Evap = evaporation.mm.d.) %>% 
  rename(Date = DateTime) %>% 
  select(WBIC,Date,Evap) %>% drop_na(Evap)
evap <- evap %>% filter(year(Date)>=1981)

dat <- precip  %>% right_join(evap)
dat$Year = year(dat$Date)
gnet_met_data <- dat %>% 
  mutate(Precip = Precip/10) %>% 
  mutate(Evap = Evap/10)
rm(dat)
rm(evap)
rm(precip)

gnet_met_data <- gnet_met_data %>% group_by(WBIC,Year) %>% 
  summarize(Precip = sum(Precip),Evap = sum(Evap))



waterlevel <- read.csv("big_data/seepage_20180414.csv",sep=",",header=TRUE) %>% 
  mutate(Date = as.Date(as.character(Date),format= "%m/%d/%Y")) %>% 
  mutate(Value = as.numeric(as.character(Value))*304.8)  %>% 
  select(WBIC,WiscID,Date,Value) %>% 
  mutate(Year=year(Date)) %>% 
  filter(Year >= 1981)
waterlevel <- waterlevel %>% 
  group_by(WBIC,WiscID,Year) %>% 
  summarize(Value = mean(Value,na.rm=TRUE)/10)

lakes_10yrs <- waterlevel %>% group_by(WiscID) %>%
  summarise(year.data = n()) %>% 
  filter(year.data >= 15)
lakes <- unique(lakes_10yrs$WiscID)
waterlevel <- waterlevel[which(waterlevel$WiscID %in% lakes),]

#standardize elevations
cdev <- read_csv("data/cdev_lakelevel_predictions.csv") %>% 
  mutate(Year = year(Date)) %>% 
  group_by(WiscID,Year) %>% 
  summarize(yhat = mean(yhat)/10)

waterlevel_means <- cdev %>% 
  left_join(waterlevel) %>% 
  drop_na() %>% 
  mutate(offset = yhat - Value) %>% 
  group_by(WiscID) %>% 
  summarize(offset = mean(offset)) %>% 
  ungroup() 
  
waterlevel <- waterlevel %>% 
  left_join(waterlevel_means) %>% 
  drop_na() %>% 
  mutate(Value = Value + offset) %>% 
  select(-offset)
  
# temp <- waterlevel %>% left_join(cdev)
# ggplot(temp,aes(x=Year,y=Value)) + geom_line() + 
#   geom_line(data = temp,aes(x=Year,y=yhat),color="red") +
#   facet_wrap(vars(WBIC))

waterlevel <- waterlevel %>% rename(stage = Value)


# #check data
# cmdv <- read_csv("data/cdev_lakelevel_predictions.csv") %>% 
#   mutate(yhat = yhat/10)
# 
# pdf("graphics/gnet_checks.pdf",width=8,height=10.5,onefile = TRUE)
# par(mfrow=c(3,1),oma=c(0,0,0,0),mar=c(4,4,4,.1))
# for(i in 1:length(lakes)) {
#   
#   levels_obs <-waterlevel %>% filter(WiscID == lakes[i])
#   level_pred <- cmdv %>% filter(WiscID == lakes[i])
#   y.min <- min(levels_obs$stage,level_pred$yhat)
#   y.max <- max(levels_obs$stage,level_pred$yhat)
#   plot(level_pred$Date,level_pred$yhat,main = lakes[i],ylim=c(y.min,y.max))
#   points(levels_obs$Date,levels_obs$stage,col="red",pch=16)
#   lines(levels_obs$Date,levels_obs$stage,col="red")
#   
#   # if(i == 1) level_out <- level_pred else level_out <- rbind(level_out,level_pred)
#   
# }
# dev.off()


# gnet_mu_sims <- read_csv("data/gnet_mu_sims.csv")
# 
# 
# gnet_met_data <- gnet_met_data %>% filter(Year >= 1981)
# waterlevel <- waterlevel %>% filter(Year >= 1981)
# 
# sims <- c(mean(gnet_mu_sims$mu.a),mean(gnet_mu_sims$mu.b)) 
# BugsOut <- read.csv("GW_Models/BUGSutputSummary.csv")
# Gnet_slopes <- read_csv("data/Gnet_slopes.csv")


#fitting stage dependant coefficient
lakes = unique(gnet_met_data$WBIC)
lakes <- lakes[lakes %in% unique(waterlevel$WBIC)]
# lakes
# lakes <- tibble(WBIC=lakes)
# HLM_Out <- read_csv("GW_models/HLM_out.csv")
# lakes.1 <- lakes %>% left_join(HLM_Out) %>% drop_na()
# stage_coef <- rep(NA,length(lakes))
# lakes <- lakes.1$WBIC
# slope <- lakes.1$slope

stage_coef <- foreach(i=1:length(lakes), .combine='c') %dopar% {
# for(i in 1:5){
    dat <- gnet_met_data %>% filter(WBIC==lakes[i])
  #Extact observed data with longest record
  dat_level <- waterlevel %>% filter(WBIC==lakes[i]) %>% 
    mutate(stage = stage + 1000)
  date_matrix = tibble(Year = seq(from=min(dat$Year),to=max(dat$Year),by=1))
  dat <- date_matrix %>% left_join(dat)
  dat <- dat %>% mutate(Evap = replace_na(Evap, median(Evap,na.rm=TRUE) )) %>% 
    mutate(Precip = replace_na(Precip, median(Precip,na.rm=TRUE) ))
  min.date = min(dat_level$Year)
  dat <- dat %>% filter(Year >= min.date)
  S = matrix(nrow = nrow(dat),ncol = 1)
    S[1,1] = dat_level$stage[1]
    rmse.diff <- 10^100
    b.levels <- seq(from = 0,to = 1,length.out=500)
    # b.levels <- 10^b.levels
    # b.levels*S[1]+sims[[1]]
    n = 0
    while(rmse.diff > 0.01 ) {
      rmse.vec = rep(NA,length(b.levels))
      for(t in 1:length(b.levels)){
      for(z in 2:nrow(S)){
      S[z,1] <- S[(z-1),1] + dat$Precip[z] + dat$Evap[z] - (b.levels[t]*(S[(z-1),1])) #+sims[[1]]
        } #end of time series loop
        # plot(S,type="l")
        # b.levels[t]
    # S
    extrap_level <- data.frame(Year= dat$Year, level = S) # predicted lake levels
    # dat.offset = dat_level %>% left_join(extrap_level) %>% 
    #   mutate(offset = level-stage) %>% drop_na()
    #   offset.it <- mean(dat.offset$offset)
    dat.compare <- dat_level %>% left_join(extrap_level) #%>% 
      #mutate(stage = stage + offset.it) %>% drop_na()
    # if(t ==1 | t==250 | t==500) {
    #       plot(extrap_level$Year,extrap_level$level,type="l")
    #   lines(dat.compare$Year,dat.compare$stage,col="red")
    # }
    rmse.vec[t] <- rmse(actual = dat.compare$stage,predicted = dat.compare$level)
    }
      min.rmse <- which(rmse.vec==min(rmse.vec))
      if(n == 0) rmse.best <- rmse.vec[min.rmse] else rmse.best = c(rmse.best,rmse.vec[min.rmse])
      if (n >= 1) rmse.diff <- (rmse.best[n]-rmse.best[n+1])
      if(rmse.diff < 0) break()
      best.b <- b.levels[min.rmse]
    b.levels <- seq(from = (b.levels[min.rmse-1]),to = (b.levels[min.rmse+1]),length.out=100)
    # b.levels <- seq(from = log10(b.levels[min.rmse-1]),to = log10(b.levels[min.rmse+1]),length.out=100)
    # b.levels <- 10^b.levels
    n = n + 1
    
    }
    best.b
} #lake foreach loop

(s_coef = median(stage_coef)) #-0.0001100618
hist(stage_coef)

s.coef.dat <- tibble(WBIC = lakes,coef = stage_coef)
# s.coef.dat <- s.coef.dat %>% left_join(lakes.1)
# ggplot(data = s.coef.dat,aes(x=Gnet,y=coef)) +geom_point() + geom_smooth(method="lm")
write_csv(x = s.coef.dat,"data/lake_gnet_coef.csv")


pdf("graphics/seepage_predictions.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,1),oma=c(0,0,0,0),mar=c(4,4,4,.1))

for(i in 1:length(lakes)) {
  dat <- gnet_met_data %>% filter(WBIC==lakes[i])
  #Extact observed data with longest record
  dat_level <- waterlevel %>% filter(WBIC==lakes[i]) %>% 
    mutate(stage = stage + 1000)
  date_matrix = tibble(Year = seq(from=min(dat$Year),to=max(dat$Year),by=1))
  dat <- date_matrix %>% left_join(dat)
  dat <- dat %>% mutate(Evap = replace_na(Evap, median(Evap,na.rm=TRUE) )) %>% 
    mutate(Precip = replace_na(Precip, median(Precip,na.rm=TRUE) ))
  min.date = min(dat_level$Year)
  dat <- dat %>% filter(Year >= min.date)
  S = matrix(nrow = nrow(dat),ncol = 1)
  S[1,1] = dat_level$stage[1]
  for(z in 2:nrow(S)){
    S[z,1] <- S[(z-1),1] + (dat$Precip[z] + dat$Evap[z]) - (stage_coef[i]*(S[(z-1),1]))
  } #end of time series loop
  # S
  extrap_level <- data.frame(Year= dat$Year, WBIC = dat$WBIC, level = S) # predicted lake levels
  # dat.offset = dat_level %>% left_join(extrap_level) %>% 
  #   drop_na() %>% 
  #   group_by(year(Date)) %>% 
  #   summarize(yhat = mean(level),y=mean(Value)) %>% 
  #   mutate(offset = yhat-y)
  # offset <- mean(dat.offset$offset)
  dat.compare <- dat_level %>% right_join(extrap_level) 
  # %>% 
    # mutate(Value = Value + offset)
  # median(dat.compare$Value-dat.compare$level,na.rm=TRUE)
  # offset
  if(i == 1 ) global_out <- dat.compare else global_out <- rbind(global_out,dat.compare)
  global_out <- global_out
  y.range = range(dat.compare$stage,dat.compare$level,na.rm = TRUE)
  plot(dat.compare$Year,dat.compare$level,type="l",col="black",
       xlab="Date",ylab="Water Level (m)",main=paste("WBIC ",lakes[i],sep=""),
       ylim=y.range)
  miss <- dat.compare[!is.na(dat.compare$stage),]
  if(nrow(miss)>5) lines(miss$Year,miss$stage,col='red',type="b",pch=16,lwd=1.5,cex=0.5)
  if(nrow(miss)<=5) lines(miss$Year,miss$stage,col='red',type="b",pch=16,lwd=1.5,cex=2)
}
dev.off()





#Generate Predictions for All Lakes
precip <- read_csv("big_data/precip.csv") %>% rename(Precip=Rain) %>% 
rename(Date = time) %>% 
  select(WBIC,Date,Precip) %>% 
  mutate(Date = as.Date(Date))
evap <- read_csv("big_data/evap.csv") %>% rename(Evap = evaporation.mm.d.) %>% 
  rename(Date = DateTime) %>% 
  select(WBIC,Date,Evap) %>% drop_na(Evap)
dat <- precip  %>% right_join(evap)
dat$Year = year(dat$Date)
gnet_met_data <- dat %>% 
  mutate(Precip = Precip/10) %>% 
  mutate(Evap = Evap/10)
rm(dat)
rm(evap)
rm(precip)

gnet_met_data <- gnet_met_data %>% group_by(WBIC,Year) %>% 
  summarize(Precip = sum(Precip),Evap = sum(Evap))
gnet_pred_coefs <- read_csv("data/gnet_pred_coefs.csv")
lakes <- unique(gnet_pred_coefs$WBIC)
lakes <- lakes[lakes %in% unique(gnet_met_data$WBIC)]

pdf("graphics/seepage_predictions_all.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,1),oma=c(0,0,0,0),mar=c(4,4,4,.1))

for(i in 1:length(lakes)) {
  dat <- gnet_met_data %>% filter(WBIC==lakes[i]) %>% drop_na()
  m <- gnet_pred_coefs %>% filter(WBIC==lakes[i])
  date_matrix = tibble(Year = seq(from=min(dat$Year),to=max(dat$Year),by=1))
  dat <- date_matrix %>% left_join(dat)
  dat <- dat %>% mutate(Evap = replace_na(Evap, median(Evap,na.rm=TRUE) )) %>% 
    mutate(Precip = replace_na(Precip, median(Precip,na.rm=TRUE) ))
  S = matrix(nrow = nrow(dat),ncol = 1)
  S[1,] = 1000
      for(z in 2:nrow(S)){
        S[z,1] <- S[(z-1),1] + (dat$Precip[z] + dat$Evap[z]) - m[[2]]*(S[(z-1),1])
      } #end of time series loop
      # S
  extrap_level <- data.frame(Year= dat$Year, WBIC = dat$WBIC, level = S) # predicted lake levels
  if(i == 1 ) global_out <- extrap_level else global_out <- rbind(global_out,extrap_level)
  global_out <- global_out
  plot(extrap_level$Year,extrap_level$level,type="l",col="black",
       xlab="Date",ylab="Water Level (cm)",main=paste("WBIC ",lakes[i],sep=""))
}
dev.off()


write_csv(x = global_out,path = "data/process_extrapolations.csv")

temp <- global_out %>% drop_na() 

rmse(actual = temp$stage,predicted = temp$level)

#####Exploratory Plots
temp <- waterlevel %>% filter(WBIC==2092500)


temp <- gnet_met_data %>% filter(WBIC==1842400) %>% 
  group_by(year(Date)) %>% summarize(tot=sum(Precip)) %>% 
  rename(Year = 'year(Date)')
ggplot(data = temp,aes(x=Year,y=tot)) + geom_line()

waterlevel_range <- global_out %>% group_by(WBIC) %>% 
  summarise(min = min(level),max=max(level)) %>% 
  mutate(wl_range = (max-min)/1000)

met_summary <- dat_met %>% group_by(WBIC,year(Date)) %>% summarise_all(funs(sum)) %>% 
  select(WBIC, Evap,Precip) %>% 
  group_by(WBIC) %>% summarise_all(funs(mean)) %>% 
  mutate(PE = Precip + Evap)

summary_data <- met_summary %>% left_join(waterlevel_range)
ggplot(data = summary_data,aes(x=PE,y=wl_range)) + geom_point() +
  labs(x="Average P-E (mm/yr)",y="Modelled Water Level Range 1950-2015 (m)")
ggplot(data = summary_data,aes(x=1,y=Evap)) + geom_violin()

