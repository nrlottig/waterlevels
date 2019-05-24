# Lake Level Project Bayesian Hierarchical Model
# Description: This program takes the lake level data and cumulative deviation of
# precipitation data. It runs the Bayesian Hierarchical Model of lake level (response) and
# precipitation (predictor). 
##################################################
# Set WD
# setwd("G:/CFL/R_workdirectary")
rm(list=ls())
options(max.print=10000)
library(tidyverse)
library(lubridate)
library(modelr)


# Input data
cmdv = read.csv("big_data/cdev.csv")
cmdv$Date = as.Date(as.character(cmdv$obs_mo))
levels_mo <- read.csv("data/monthly_waterlevels.csv")
levels_mo$Date <- as.Date(levels_mo$Date)
seepage_lake_slope_data <- read_csv("data/seepage_lake_slope_data.csv") %>% 
  select(WiscID,mean_ann_wdrl_gals) %>% 
  mutate(mean_ann_wdrl_gals = replace_na(mean_ann_wdrl_gals,0))
levels_mo <- levels_mo %>% left_join(seepage_lake_slope_data) %>% filter(mean_ann_wdrl_gals<=10000000) %>% select(-mean_ann_wdrl_gals)

levels_mo <- levels_mo %>% 
  filter(WiscID != 394) %>% 
  filter(WiscID != 58) %>% 
  filter(WiscID !=62) %>% 
  filter(WiscID !=393) %>% 
  filter(WiscID !=428) %>% 
  filter(WiscID !=583) %>% 
  filter(WiscID !=405)  %>% 
  filter(WiscID !=57)

#filter out two outliers from early record of 772
levels_mo$Value[which(levels_mo$WiscID==772 & levels_mo$Year==1953)] = NA
levels_mo <- levels_mo %>% drop_na()

dat <- levels_mo %>% left_join(cmdv %>% dplyr:::select(WiscID,Date,ppt_cdm_96mo_mm)) %>% 
  filter(Month <= 10 & Month >=4) %>% 
  group_by(WiscID,Year) %>% group_by(WiscID) %>% 
  mutate(count=length(unique(Year))) %>% 
  filter(count >=8) 
# %>% 
#   group_by(WiscID) %>% 
#   mutate(fraction = length(unique(Year))/(max(Year)-min(Year))) %>%
#   filter(fraction >= 0.5)

lakes_choose <- unique(dat$WiscID)  

levels_mo <- levels_mo[which(levels_mo$WiscID %in% lakes_choose),]
ggplot(data = levels_mo,aes(x=Date,y=scale(Value,scale = F))) + geom_point() + geom_line() + facet_wrap(vars(WiscID),scales = "free")

dat <- levels_mo %>% right_join(cmdv %>% dplyr:::select(WiscID,Date,ppt_cdm_96mo_mm)) %>% 
  arrange(WiscID,Date) %>% 
  rename(precipCMDV = 6) %>% group_by(WiscID) %>% 
  mutate(Value=scale(Value,center = TRUE,scale = FALSE)) %>% 
  mutate(precipCMDV = scale(precipCMDV,center = TRUE,scale = FALSE)) %>%
  #mutate(precip_range = (max(precipCMDV)-min(precipCMDV))) %>%
  ungroup() #%>% 
dat <- dat[which(dat$WiscID %in% lakes_choose),]
dat_summary <- dat %>% group_by(WiscID) %>% 
  summarize(q25=quantile(precipCMDV,probs = .15),q75=quantile(precipCMDV,probs = 0.85)) %>% 
  mutate(qrange = q75-q25)
dat_complete <- dat %>% drop_na(Value) %>% 
  group_by(WiscID) %>% 
  summarize(cdev_range = max(precipCMDV)-min(precipCMDV)) %>% 
  left_join(dat_summary) %>% 
  mutate(overlap = cdev_range-qrange) %>% filter(overlap >=0)
lakes_choose <- dat_complete$WiscID
dat <- dat[which(dat$WiscID %in% lakes_choose),]
#filter(precip_range>=500)
# lakes <- unique(dat$WiscID)
# dput(lakes)
# dat.plot <- dat %>% group_by(WiscID) %>% summarize(precip_range=mean(precip_range))
# ggplot(data = dat.plot,aes(x=precip_range)) + geom_histogram()
# summary(dat.plot)
# length(unique(dat$WiscID))

ggplot(data = dat %>% drop_na(Value),aes(x=Date,y=Value)) + geom_point() + geom_line() +
  facet_wrap(vars(WiscID),scales="free")

ggsave("graphics/LT_Precip_Lakes.pdf",device = "pdf",width=11,height=8.5,units="in",dpi=300)

ggplot(data = dat,aes(x=precipCMDV,y=Value)) + geom_point() + geom_smooth(method="lm") +
  facet_wrap(vars(WiscID), scales = "free_x") + geom_vline(data = dat_complete,aes(xintercept=q25)) +
  geom_vline(data = dat_complete,aes(xintercept=q75))
ggsave("graphics/LT_Precip_regression.pdf",device = "pdf",width=11,height=8.5,units="in",dpi=300)

####Look at the data
# str(dat)
# summary(dat)

# Reassign a WiscID to all lakes
# This is not the WiscID used in the big dataset!!!
# Because the jags methods requires a consecutive ID list starting from 1
library(jagsUI)
library(lattice)
library(MCMCpack)
library(arm)

dat <- dat %>% drop_na(Value)
allLakeList = unique(dat$WiscID)
dat$BHMID = NA
for (i in 1:length(allLakeList)) {
  dat$BHMID[dat$WiscID %in% allLakeList[i]] = i
}

# The Model
sink("Model.txt")
cat("
    model {
    for (i in 1:n){
    y[i] ~ dnorm (y.hat[i], tau.y)
    #y[i] ~ dt (y.hat[i], tau.y, nu)
    y.hat[i] <- alpha[group[i]] + beta[group[i]] * x[i]  
    }
    
    tau.y <- pow(sigma.y, -2)
    sigma.y ~ dunif (0, 10)
    nu <- nuMinusOne + 1
    nuMinusOne ~ dexp( 1/29 )
    
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] <- BB[j,1]
    beta[j] <- BB[j,2]
    
    BB[j,1:K] ~ dmnorm (BB.hat[j,], Tau.B[,])
    BB.hat[j,1] <- mu.a 
    BB.hat[j,2] <- mu.b 
    
    }
    
    
    mu.a ~ dnorm(-1.1,0.0001)
    mu.b ~ dnorm(1,0.0001)
    
    
    # Model variance-covariance
    Tau.B[1:K,1:K] ~ dwish(W[,], df)
    df <- K+1
    Sigma.B[1:K,1:K] <- inverse(Tau.B[,])
    for (k in 1:K){
    for (k.prime in 1:K){
    rho.B[k,k.prime] <- Sigma.B[k,k.prime]/sqrt(Sigma.B[k,k]*Sigma.B[k.prime,k.prime])
    }
    sigma.B[k] <- sqrt(Sigma.B[k,k])
    }
    
    }
    ",fill=TRUE)
sink()

# Set up the parameters before run the model
# Number of parameters
K = 2
W <- diag(K)
# Number of lakes
J = length(unique(dat$BHMID))
# Load data raw water level data
dat = as.data.frame(dat)
summary(dat)
data = list(y =dat$Value, group = as.numeric(dat$BHMID), n = dim(dat)[1], J = J,
            x = dat$precipCMDV, K = K, W = W)
# Initial values
inits <- function (){
  list (BB=array(c(rep(rnorm(1,0,1),J),rep(rnorm(1,0,1),J)), c(J,K)), 
        mu.a=rnorm(1,0,1),mu.b=rnorm(1,0,1),
        sigma.y=runif(1,0,10), 
        Tau.B=rwish(K+1,diag(K))	 )
}

params1 <- c("BB","mu.a","mu.b", "sigma.y","sigma.B","rho.B")

# params1 <- c("mu.a","mu.b")
# Parameters monitored
# mu.alpha: global alpha
# mu.beta: global beta
# BB: local alphas and betas
# sigma: local error term
# sigma.a: variances of alpha
# sigma.b: variances of beta
# rho: covarainces of alpha and beta
# 
# MCMC settings
ni <- 5000
nb <- 3000
nc <- 7
nt <- 1
# nadp <- 15000

# Run the model
out <- jags(data, inits, params1, "Model.txt", n.chains = nc, 
            n.thin = nt, n.iter = ni, n.burnin = nb,parallel = TRUE)
# Show some of the result
BugsOut <- out$summary
BugsOut
summary(BugsOut[,8])
BugsOut[which(BugsOut[,8]>=1.1),]
jagsUI:::traceplot(out)


saveRDS(out,"BayHModels/HLM_reduced.rds")
# out <- readRDS(file ="BayHModels/HLM_reduced.rds")
# write.csv(BugsOut, "BayHModels/BUGSutputSummary.csv", row.names = T)

# 
# pred_sims = as.data.frame(t(out$BUGSoutput$sims.list$yp))
# pred_sims = cbind(dat$WiscID,dat$Date,dat$Value,pred_sims)
# names(pred_sims)[1:3] =c("WiscID","Date","obs")
# pred_sims$mean = apply(pred_sims[,c(4:4503)],1,FUN=mean)
# pred_sims$ll = apply(pred_sims[,c(4:4503)],1,FUN=function(x) quantile(x,probs=0.025))
# pred_sims$ul = apply(pred_sims[,c(4:4503)],1,FUN=function(x) quantile(x,probs=0.975))
# 
# pred_sims = pred_sims %>% select(everything(),-contains("V"))
# pred_sims$Date = as.character(dat$Date)
# pred_sims$Date = paste(dat$Date,"/15",sep="")
# pred_sims$Date = as_date(x = pred_sims$Date)

# dt = pred_sims %>% filter(WiscID==1030) %>% arrange(Date)
# plot(dt$Date,dt$obs,ylim=range(dt$ll,dt$ul))
# polygon(c(dt$Date,rev(dt$Date)),c(dt$ll,rev(dt$ul)),col="grey",border="darkgrey")
# lines(dt$Date,dt$obs,pch=16,col="black",type="p")
# lines(dt$Date,dt$mean,pch=16,type="b",col="blue")
# plot(dt$obs,dt$mean)
# abline(a=0,b=1)
# CV <- function(mean, sd){
#   (sd/mean)*100
# }

reg.coef = out$mean$BB
n.dat = rep(NA,J)
n.dat2 = rep(NA,J)
lakes = unique(dat$BHMID)
rmse <- function(actual,predicted)
{
  sqrt(mean((actual-predicted)^2))
}

pdf("BayHModels/myOut.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,2))
for (i in 1:length(lakes)){
  #pull out data for each lake and generate predicted water levels
  dat.t = dat %>% filter(BHMID==i) %>% arrange(Date)
  dat.t = dat.t %>% mutate(predValue=reg.coef[i,1] + dat.t$precipCMDV*reg.coef[i,2]) %>% 
    mutate(globalValue = out$mean$mu.a + dat.t$precipCMDV*out$mean$mu.b)
  n.dat[i] = rmse(dat.t$Value,dat.t$globalValue)
  n.dat2[i] = rmse(dat.t$Value,dat.t$predValue)
  #plot relationship between precip and water level
  plot(x = dat.t$precipCMDV, y = dat.t$Value,xlab="PrecipCMDV (mm)",
       ylab="Water Level (mm)",pch=16)
  abline(a = reg.coef[i,1][[1]],b=reg.coef[i,2][[1]],col="red",lwd=2)
  abline(a =out$mean$mu.a,b=out$mean$mu.b,col="green",lwd=2)
  mtext(side=3,line=1,paste(dat.t$SiteName[1], " WiscID:",dat.t$WiscID[1], " WIBIC:",dat.t$WBIC[1],sep=""),cex=.8)
  legend('topleft',legend=c("bayesH","global"),lty=1,col=c("red","green"))
  mtext(side=3,line=-4,paste("local slope: ",round(reg.coef[i,2][[1]],digits=3),sep=""),adj=0.02,cex=0.7)
  #plot predicted and observed water levels
  y.range = range(c(dat.t$Value,dat.t$predValue,dat.t$globalValue))
  plot(dat.t$Date,dat.t$Value,type="b",pch=16,xlab="Date",ylab="Water Level (mm)",ylim=y.range)
  points(dat.t$Date,dat.t$predValue,type="l",pch=16,col="red")
  lines(dat.t$Date,dat.t$globalValue,type="l",col="green")
  legend("top",ncol=3,legend=c("obs","regional","local"),lty=1,col=c("black","green","red"))
  
}
dev.off()
mean(n.dat)
mean(n.dat2)
out$mean$mu.b

reg.summary = data.frame(WiscID = allLakeList,slope=reg.coef[,2])
write_csv(reg.summary,"BayHModels/regressionstats.csv")
