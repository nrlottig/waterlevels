# Lake Level Project Bayesian Hierarchical Model
# Description: This program takes the lake level data and cumulative deviation of
# precipitation data. It runs the Bayesian Hierarchical Model of lake level (response) and
# precipitation (predictor). 
##################################################
rm(list=ls())
library(tidyverse)
library(R2jags)
library(lattice)

# Input data
dt = read_csv("GW_Models/lake_climate_20180414_openWaterSeason.csv")
dat = dt %>% select(WiscID,Date1,DeltaDate,Stage1_mm,Stage2_mm,DeltaWaterLevel_mm,
                     Precip_mm,Evap_mm) %>% drop_na() %>% arrange(WiscID,Date1) %>% 
  mutate(PE_mmd = (Precip_mm+Evap_mm)/DeltaDate) %>% 
  mutate(deltaS_mmd=DeltaWaterLevel_mm/DeltaDate)
#Filter the data so that we have at least 5 obs for each lake
num.rec = table(dat$WiscID)
keep.rec = as.numeric(names((num.rec[which(num.rec>=10)])))
dat = dat[which(dat$WiscID %in% keep.rec),]
length(unique(dat$WiscID))

####Look at the data
str(dat)
summary(dat)

# Reassign a WiscID to all lakes
# This is not the WiscID used in the big dataset!!!
# Because the jags methods requires a consecutive ID list starting from 1
allLakeList = unique(dat$WiscID)
dat$BHMID = NA
for (i in 1:length(allLakeList)) {
  dat$BHMID[dat$WiscID %in% allLakeList[i]] = i
}


# The Model
sink("model.txt")
cat("
    model {
    # Likelihood: 
    # Level-1 of the model
    for (i in 1:n){ 
    y[i] ~ dt(mu[i], tau, tdf)               
    mu[i] <- alpha[group[i]] + beta[group[i]] * precip[i]         
    } 
    # Level-2 of the model
    for(j in 1:J){
    alpha[j] <- BB[j,1]
    beta[j] <- BB[j,2]
    BB[j,1:K] ~ dmnorm(BB.hat[j,], Tau.B[,]) # bivriate normal
    BB.hat[j,1] <- mu.alpha
    BB.hat[j,2] <- mu.beta
    }
    # Priors and derived quantities
    sigma ~ dunif(0, 100)
    tau <- pow(sigma,-2) # precision
    udf ~ dunif(0,1)
    tdf <- 1 - tdfGain *log(1-udf)
    sigma2 <- pow(sigma,2)
    mu.alpha ~ dnorm(0, 0.0001)
    mu.beta ~ dnorm(0, 0.0001)
    
    # Convert covariance matrix to precision for use in bivariate normal above
    Tau.B[1:K,1:K] <- inverse(Sigma.B[,])
    # variance among intercepts
    Sigma.B[1,1] <- pow(sigma.a, 2)
    sigma.a ~ dunif (0, 100)
    
    # Variance among slopes
    Sigma.B[2,2] <- pow(sigma.b, 2)
    sigma.b ~ dunif (0, 100)
    
    # Covariance between alpha's and beta's
    Sigma.B[1,2] <- rho * sigma.a * sigma.b
    Sigma.B[2,1] <- Sigma.B[1,2]
    
    # Uniform prior on correlation
    rho ~ dunif (-1, 1)
    } # end model
    ",fill = TRUE)
sink()

# Set up the parameters before run the model
# Number of parameters
K = 2
# Number of lakes
J = length(unique(dat$BHMID))
# Load data raw water level data
tdfGain = 1
dat = as.data.frame(dat)
data = list(y = dat$deltaS_mmd, group = as.numeric(dat$BHMID), n = dim(dat)[1], J = J,
            precip = dat$PE_mmd, K = K,tdfGain = tdfGain)
# Initial values
inits = function (){
  list(mu.alpha = rnorm(1), mu.beta=rnorm(1), sigma=runif(1),
       BB=matrix(rnorm(J*K),nrow=J,ncol=K), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1),
       udf = 0.95)
}

# Parameters monitored
# mu.alpha: global alpha
# mu.beta: global beta
# BB: local alphas and betas
# sigma: local error term
# sigma.a: variances of alpha
# sigma.b: variances of beta
# rho: covarainces of alpha and beta
# 
parameters = c("mu.alpha","mu.beta","BB","sigma", "sigma.a", "sigma.b","rho")
# MCMC settings
ni <- 40000
nb <- 10000
nc <- 3
(nt <- ceiling((ni-nb)*nc/1500))

# Run the model
out = jags.parallel(data, inits, parameters, "model.txt", n.chains = 3, 
           n.thin = 60, n.iter = 40000, n.burnin = 10000)
saveRDS(out,"GW_Models/HLM_reduced.rds")
# Show some of the result
print(out, dig = 3)
which(out$BUGSoutput$summary[, c("Rhat")] > 1.1)
max(out$BUGSoutput$summary[, c("Rhat")])
out.mcmc <- as.mcmc(out)
str(out.mcmc)
# look at summary
summary(out.mcmc)
par(mfrow=c(4,4))
# Create traceplots
xyplot(out.mcmc,layout =c(4,4))
# Look at posterior density plots
densityplot(out.mcmc)

#### Just make plots for parameters of interest
out.mcmc2 <- out.mcmc[,c("mu.alpha","mu.beta")]
xyplot(out.mcmc2)
densityplot(out.mcmc2)


reg.coef = out$BUGSoutput$mean$BB
lakes = unique(dat$WiscID)

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
  abline(a = out$BUGSoutput$mean$mu.alpha,b=out$BUGSoutput$mean$mu.beta,col="green",lwd=2)
  mtext(side=1,adj=0.9,line=-2,round(reg.coef[i,1][[1]],3))
  mtext(side=3,line=1,paste(dat.t$SiteName[1], " WiscID:",dat.t$WiscID[1],sep=""),cex=.8)
  legend('topleft',legend=c("linear","bayesH","global"),lty=1,col=c("lightblue","red","green"))
  
}
dev.off()

write_csv(data.frame(WiscID=lakes,Gnet=reg.coef[,1],slope=reg.coef[,2]),"GW_Models/HLM_out.csv")
