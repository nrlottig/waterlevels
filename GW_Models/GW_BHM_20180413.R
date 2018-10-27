# Lake Level Project Bayesian Hierarchical Model
# Description: This program takes the lake level data and cumulative deviation of
# precipitation data. It runs the Bayesian Hierarchical Model of lake level (response) and
# precipitation (predictor). 
##################################################
rm(list=ls())
library(tidyverse)
library(jagsUI)
library(lattice)
library(MCMCpack)
library(arm)

# Input data
dt = read_csv("GW_Models/lake_climate_20180414_openWaterSeason.csv")
dat <- dt %>% dplyr::select(WiscID,WBIC,Date1,DeltaDate,Stage1_mm,Stage2_mm,DeltaWaterLevel_mm,
                     Precip_mm,Evap_mm) %>% drop_na() %>% arrange(WiscID,Date1) %>% 
  mutate(PE_mmd = (Precip_mm+Evap_mm)/DeltaDate) %>% 
  mutate(deltaS_mmd=DeltaWaterLevel_mm/DeltaDate)
#Filter the data so that we have at least 5 obs for each lake
(num.rec = table(dat$WBIC))
keep.rec = as.numeric(names((num.rec[which(num.rec>=10)])))
dat = dat[which(dat$WBIC %in% keep.rec),] %>% arrange(WBIC,Date1)
length(unique(dat$WBIC))
#data cleanup for clear outlier leverage data points
dat <- dat %>% filter(deltaS_mmd <= 17) %>% 
  mutate(deltaS_mmd = replace(deltaS_mmd,WBIC==197600 & deltaS_mmd< -5,NA)) %>% 
  mutate(deltaS_mmd = replace(deltaS_mmd,WBIC==589400 & deltaS_mmd< -10,NA)) %>% 
  mutate(deltaS_mmd = replace(deltaS_mmd,WBIC==968800 & deltaS_mmd< -15,NA)) %>% 
  mutate(deltaS_mmd = replace(deltaS_mmd,WBIC==2092500 & deltaS_mmd< -7,NA)) %>% drop_na(deltaS_mmd)

ggplot(data = dat, aes(x=PE_mmd,y=deltaS_mmd)) + geom_point() + facet_wrap(vars(WBIC),scales = "free_y")
####Look at the data
str(dat)
summary(dat)

# Reassign a WiscID to all lakes
# This is not the WiscID used in the big dataset!!!
# Because the jags methods requires a consecutive ID list starting from 1
allLakeList = unique(dat$WBIC)
dat$BHMID = NA
for (i in 1:length(allLakeList)) {
  dat$BHMID[dat$WBIC %in% allLakeList[i]] = i
}
dat

# The Model
sink("Model.txt")
cat("
    model {
    for (i in 1:n){
    y[i] ~ dt (y.hat[i], tau.y, nu)
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
    
    
    mu.a ~ dnorm(0,0.0001)
    mu.b ~ dnorm(0,0.0001)
    
    
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
data = list(y =dat$deltaS_mmd, group = as.numeric(dat$BHMID), n = dim(dat)[1], J = J,
            x = dat$PE_mmd, K = K, W = W)
# Initial values
r <- cor(data$x,data$y)
inits <- function (){
  list (BB=array(c(rep(rnorm(1,0,1),J),rep(rnorm(1,0,1),J)), c(J,K)), 
        mu.a=rnorm(1,0,1),mu.b=rnorm(1,0,1),
        sigma.y=runif(1,0,10), 
        Tau.B=rwish(K+1,diag(K))	 )
}

params1 <- c("BB","mu.a","mu.b", "sigma.y","sigma.B","rho.B")

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
ni <- 10000
nb <- 1000
nc <- 3
nt <- 1
# (nt <- ceiling((ni-nb)*nc/1500))

# Run the model
out <- jags(data, inits, params1, "Model.txt", n.chains = nc, 
             n.thin = nt, n.iter = ni, n.burnin = nb)

saveRDS(out,"GW_Models/HLM_reduced.rds")
out <- readRDS(file ="GW_Models/HLM_reduced.rds")
# Summarize posteriors
print(out, dig = 3)

BugsOut <- out$summary
write.csv(BugsOut, "GW_Models/BUGSutputSummary.csv", row.names = T)


#Identify lakes that have different gnets from regional pattern
SlopeDiff <- matrix(NA, ncol=J, nrow=out$mcmc.info$n.samples)
for(i in 1:J){
  SlopeDiff[,i] <- out$sims.list$mu.a - out$sims.list$BB[,i,1]
}
sum.slopediff <- apply(SlopeDiff, 2, quantile, c(0.025,0.975))
sum.slopediff <- as.data.frame(t(sum.slopediff))
sum.slopediff$WBIC <- allLakeList
names(sum.slopediff)[1:2] = c("ll","ul")
value = 0
sum.slopediff <- sum.slopediff %>% 
  mutate(gnet.reg = value >= ll & value <= ul) %>% 
  dplyr::select(WBIC,gnet.reg)

### Slope plots
dat.slope <- as.data.frame(BugsOut[1:50,])
dat.slope$WBIC <- allLakeList
names(dat.slope)[c(3,7)] <- c("ll","ul")
dat.slope <- dat.slope %>% left_join(sum.slopediff,by = "WBIC")
dat.slope$WBIC <- factor(dat.slope$WBIC,levels=dat.slope$WBIC[order(dat.slope$mean)])
dat.slope <- dat.slope %>% arrange(mean)
dat.slope <- dat.slope %>% dplyr::select(WBIC,mean,ll,ul,gnet.reg) %>% rename(slope_group = gnet.reg)
ggplot(data = dat.slope,aes(x=WBIC,y=mean,color=slope_group)) + 
  geom_hline(yintercept = BugsOut[101,3],col="blue") + 
  geom_hline(yintercept = BugsOut[101,7],col="blue")+
  geom_point() +
  geom_errorbar(aes(ymin=ll,ymax=ul)) +
  labs(x="Lake",y="Gnet")

write_csv(dat.slope,"data/Gnet_slopes.csv")
reg.coef = out$mean$BB
lakes = unique(dat$WBIC)

pdf("myOutGW.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,2))
for (i in 1:length(lakes)){
  #pull out data for each lake and generate predicted water levels
  dat.t = dat %>% filter(BHMID==i) 
  #plot relationship between precip and water level
  plot(x = dat.t$PE_mmd, y = dat.t$deltaS_mmd,xlab="Precip - Evap (mm/d)",
       ylab="Delta Water Level (mm/d)",pch=16,ylim=range(dat$deltaS_mmd),
       xlim=range(dat$PE_mmd))
  tryCatch({
    abline(lm(dat.t$deltaS_mmd~dat.t$PE_mmd),col="lightblue",lwd=2)
  },error=function(e){})
  abline(a = reg.coef[i,1][[1]],b=reg.coef[i,2][[1]],col="red",lwd=2)
  abline(a = out$mean$mu.a,b=out$mean$mu.b,col="green",lwd=2)
  mtext(side=1,adj=0.9,line=-2,round(reg.coef[i,1][[1]],3))
  mtext(side=3,line=1,paste("WIBIC:",dat.t$WBIC[1],sep=""),cex=.8)
  legend('topleft',legend=c("linear","bayesH","global"),lty=1,col=c("lightblue","red","green"))
  
}
dev.off()

write_csv(data.frame(WBIC=lakes,Gnet=reg.coef[,1],slope=reg.coef[,2]),"GW_Models/HLM_out.csv")
