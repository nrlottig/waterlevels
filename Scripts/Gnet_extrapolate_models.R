library(tidyverse)
library(lubridate)
library(foreach)
library(doParallel)
library(iterators)
library(Metrics)

registerDoParallel(cores = 8)


# ####The Data
# Gnet_slopes <- read_csv("data/Gnet_slopes.csv") %>% select(WBIC)
# 
# #limit the evap and precip data to the 50 lakes we have GW recharge estimates 
# evap_daily <- read_csv("big_data/evap_daily.csv", 
#                        col_types = cols(X1 = col_skip())) %>% 
#   distinct(WBIC, Date,.keep_all = TRUE) %>% right_join(Gnet_slopes)
# precip_daily <- read_csv("big_data/precip_daily.csv", 
#                          col_types = cols(X1 = col_skip())) %>% 
#   distinct(WBIC, Date,.keep_all = TRUE) %>% right_join(Gnet_slopes)
# 
# dat <- evap_daily %>% full_join(precip_daily)
# write_tsv(dat,"big_data/gnet_met_data.csv")
gnet_met_data <- read.csv("big_data/gnet_met_data.csv") %>% 
  mutate(Date = as.Date(as.character(Date)))
gnet_met_data <- gnet_met_data %>% drop_na() %>% mutate(Year=year(Date))

gnet_mu_sims <- read_csv("data/gnet_mu_sims.csv")

waterlevel <- read.csv("big_data/seepage_20180414.csv",sep=",",header=TRUE) %>% 
  mutate(Date = as.Date(as.character(Date),format= "%m/%d/%Y")) %>% 
  mutate(Value = as.numeric(as.character(Value))*304.8)  %>% 
  select(WBIC,WiscID,Date,Value) %>% 
  mutate(Year=year(Date))

gnet_met_data <- gnet_met_data %>% filter(Year >= 1981)
waterlevel <- waterlevel %>% filter(Year >= 1981)

sims <- c(mean(gnet_mu_sims$mu.a),mean(gnet_mu_sims$mu.b)) 
BugsOut <- read.csv("GW_Models/BUGSutputSummary.csv")
Gnet_slopes <- read_csv("data/Gnet_slopes.csv")

lakes_10yrs <- waterlevel %>% group_by(WBIC,year(Date)) %>%
  summarise(year.data = n())
(num.rec <- table(lakes_10yrs$WBIC))
keep.rec = as.numeric(names((num.rec[which(num.rec>=10)])))
#fitting stage dependant coefficient
lakes = unique(gnet_met_data$WBIC)
lakes <- lakes[lakes %in% keep.rec]

# stage_coef <- rep(NA,length(lakes))
stage_coef <- foreach(i=1:length(lakes), .combine='c') %dopar% {
# for(i in 1:length(lakes)){
    dat <- gnet_met_data %>% filter(WBIC==lakes[i])
  #Extact observed data with longest record
  dat_level <- waterlevel %>% filter(WBIC==lakes[i])
  obs.sites <- length(unique(dat_level$WiscID))
  if(obs.sites > 1) {
    count.obs <- table(dat_level$WiscID)
    best.lake = as.numeric(names(count.obs)[which(count.obs==max(count.obs))])
    dat_level <- dat_level %>% filter(WiscID==best.lake)
  }
  date_matrix = tibble(Date = seq(from=min(dat$Date),to=max(dat$Date),by="day"))
  dat <- date_matrix %>% left_join(dat)
  dat <- dat %>% mutate(Evap = replace_na(Evap, median(Evap,na.rm=TRUE) )) %>% 
    mutate(Precip = replace_na(Precip, median(Precip,na.rm=TRUE) ))
    S = matrix(nrow = nrow(dat),ncol = 1)
    S[1,1] = 100*1000
    rmse.diff <- 10^100
    b.levels <- seq(from = log10(10^-4.3),to = log10(10^-4.9),length.out=20)
    b.levels <- 10^b.levels
    n = 0
    while(rmse.diff > 0.0001 ) {
      rmse.vec = rep(NA,length(b.levels))
      for(t in 1:length(b.levels)){
      for(z in 2:nrow(S)){
      S[z,1] <- S[(z-1),1] + sims[[2]]*(dat$Precip[z] + dat$Evap[z]) - (b.levels[t]*(S[(z-1),1])+sims[[1]])
        } #end of time series loop
    # S
    extrap_level <- data.frame(Date= dat$Date, level = S) # predicted lake levels
    dat.offset = dat_level %>% left_join(extrap_level) %>% 
      mutate(offset = level-Value) %>% drop_na()
      offset <- mean(dat.offset$offset)
    dat.compare <- dat_level %>% left_join(extrap_level) %>% 
      mutate(Value = Value + offset) %>% drop_na()
    # plot(extrap_level$Date,extrap_level$level,type="l",main=lakes[i])
    # lines(dat.compare$Date,dat.compare$Value,col="red")
    rmse.vec[t] <- rmse(actual = dat.compare$Value,predicted = dat.compare$level)
    }
    min.rmse <- which(rmse.vec==min(rmse.vec))
    if(n == 0) rmse.best <- rmse.vec[min.rmse] else rmse.best = c(rmse.best,rmse.vec[min.rmse])
    best.b <- b.levels[min.rmse]
    if (n >= 1) rmse.diff <- (rmse.best[n]-rmse.best[n+1])
    b.levels <- seq(from = log10(b.levels[min.rmse-1]),to = log10(b.levels[min.rmse+1]),length.out=20)
    b.levels <- 10^b.levels
    n = n + 1
    
    }
    best.b
} #lake foreach loop

(s_coef = median(stage_coef)) #2.35596e-05
summary(log10(stage_coef))

#Generate Predictions for All Lakes
gnet_met_data <- read.csv("big_data/gnet_met_data.csv") %>% 
  mutate(Date = as.Date(as.character(Date)))
gnet_met_data <- gnet_met_data %>% drop_na() %>% mutate(Year=year(Date))
waterlevel <- read.csv("big_data/seepage_20180414.csv",sep=",",header=TRUE) %>% 
  mutate(Date = as.Date(as.character(Date),format= "%m/%d/%Y")) %>% 
  mutate(Value = as.numeric(as.character(Value))*304.8)  %>% 
  select(WBIC,WiscID,Date,Value) %>% 
  mutate(Year=year(Date))
lakes = unique(waterlevel$WBIC)

pdf("graphics/seepage_predictions.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,1),oma=c(0,0,0,0),mar=c(4,4,4,.1))

for(i in 1:length(lakes)) {
  dat <- gnet_met_data %>% filter(WBIC==lakes[i]) %>% drop_na()
  if(nrow(dat)<1000) next()
  #Extact observed data with longest record
  dat_level <- waterlevel %>% filter(WBIC==lakes[i])
  obs.sites <- length(unique(dat_level$WiscID))
  if(obs.sites > 1) {
    count.obs <- table(dat_level$WiscID)
    best.lake = as.numeric(names(count.obs)[which(count.obs==max(count.obs))])
    dat_level <- dat_level %>% filter(WiscID==best.lake)
  }
  dat_level <- dat_level %>% select(Date,Value)
  date_matrix = tibble(Date = seq(from=min(dat$Date),to=max(dat$Date),by="day"))
  dat <- date_matrix %>% left_join(dat)
  dat <- dat %>% mutate(Evap = replace_na(Evap, median(Evap,na.rm=TRUE) )) %>% 
    mutate(Precip = replace_na(Precip, median(Precip,na.rm=TRUE) ))
  S = matrix(nrow = nrow(dat),ncol = 1)
  S[1,] = 100*1000
      for(z in 2:nrow(S)){
        S[z,1] <- S[(z-1),1] + sims[[2]]*(dat$Precip[z] + dat$Evap[z]) - (s_coef*(S[(z-1),1])+sims[[1]])
      } #end of time series loop
      # S
  extrap_level <- data.frame(Date= dat$Date, WBIC = dat$WBIC, level = S) # predicted lake levels
  dat.offset = dat_level %>% left_join(extrap_level) %>% 
    drop_na() %>% 
    group_by(year(Date)) %>% 
    summarize(yhat = mean(level),y=mean(Value)) %>% 
    mutate(offset = yhat-y)
  offset <- mean(dat.offset$offset)
  dat.compare <- dat_level %>% right_join(extrap_level) %>% 
    mutate(Value = Value + offset)
  # median(dat.compare$Value-dat.compare$level,na.rm=TRUE)
  # offset
  if(i == 1 ) global_out <- dat.compare else global_out <- rbind(global_out,dat.compare)
  global_out <- global_out
  y.range = range(dat.compare$Value/1000,dat.compare$level/1000,na.rm = TRUE)
  plot(dat.compare$Date,dat.compare$level/1000,type="l",col="black",
       xlab="Date",ylab="Water Level (m)",main=paste("WBIC ",lakes[i],sep=""),
       ylim=y.range)
  miss <- dat.compare[!is.na(dat.compare$Value),]
  lines(miss$Date,miss$Value/1000,col='red',type="l",pch=16,lwd=2)
}
dev.off()

#####Exploratory Plots

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

# 
# pdf("myOutGW.pdf",width=8,height=10.5,onefile = TRUE)
# par(mfrow=c(3,2))
# for (i in 1:length(lakes)){
#   #pull out data for each lake and generate predicted water levels
#   dat.t = dat %>% filter(BHMID==i) 
#   #plot relationship between precip and water level
#   plot(x = dat.t$PE_mmd, y = dat.t$deltaS_mmd,xlab="Precip - Evap (mm/d)",
#        ylab="Delta Water Level (mm/d)",pch=16,ylim=range(dat$deltaS_mmd),
#        xlim=range(dat$PE_mmd))
#   tryCatch({
#     abline(lm(dat.t$deltaS_mmd~dat.t$PE_mmd),col="lightblue",lwd=2)
#   },error=function(e){})
#   abline(a = reg.coef[i,1][[1]],b=reg.coef[i,2][[1]],col="red",lwd=2)
#   abline(a = out$mean$mu.a,b=out$mean$mu.b,col="green",lwd=2)
#   mtext(side=1,adj=0.9,line=-2,round(reg.coef[i,1][[1]],3))
#   mtext(side=3,line=1,paste("WIBIC:",dat.t$WBIC[1],sep=""),cex=.8)
#   legend('topleft',legend=c("linear","bayesH","global"),lty=1,col=c("lightblue","red","green"))
#   
# }
# 
#   out <- as.data.frame(t(apply(out_loop, 1, quantile, c(0.025,0.5,0.975))))
#   out <- cbind(dat$WBIC,dat$Date,out)
#   names(out) <- c("WBIC","Date","ll_val","median_val","ul_val")
# 
# 
# ggplot(data = out,aes(x=Date,y=median_val)) + geom_line() +
#   geom_line(aes(x=Date,y=ll_val),color="lightblue") + geom_line(aes(x=Date,y=ul_val),color="lightblue")
