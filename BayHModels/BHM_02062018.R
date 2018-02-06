# Lake Level Project Bayesian Hierarchical Model
# Description: This program takes the lake level data and cumulative deviation of
# precipitation data. It runs the Bayesian Hierarchical Model of lake level (response) and
# precipitation (predictor). 
##################################################
rm(list=ls())
# Set WD
# setwd("G:/CFL/R_workdirectary")
library(tidyverse)
library(R2jags)
library(lattice)  # plot

# Input data
dat = read.csv("BHM_input_02052018.csv")
####Look at the data
str(dat)
summary(dat)

dat = dat %>% drop_na()
summary(dat)

# Reassign a WiscID to all lakes
# This is not the WiscID used in the big dataset!!!
# Because the jags methods requires a consecutive ID list starting from 1
allLakeList = unique(dat$LakeID)
dat$WiscID = NA
for (i in 1:length(allLakeList)) {
  dat$WiscID[dat$LakeID %in% allLakeList[i]] = i
}

#standardize the lowest observed water level to 1000mm (1m)
lakes = unique(dat$WiscID)
for(i in 1:length(lakes)){
  temp = dat$Value[dat$WiscID %in% lakes[i]]
  std.level = temp-min(temp)+1000
  dat$Value[dat$WiscID %in% lakes[i]] = std.level
}

#standardize the lowest observed precipCMDV to 0
for(i in 1:length(lakes)){
  temp = dat$precipCMDV[dat$WiscID %in% lakes[i]]
  std.level = temp-min(temp)
  dat$precipCMDV[dat$WiscID %in% lakes[i]] = std.level
}
summary(dat)

# The Model
sink("model.txt")
cat("
    model {
    # Likelihood: 
    # Level-1 of the model
    for (i in 1:n){ 
        y[i] ~ dnorm(mu[i], tau)               
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
J = length(unique(dat$WiscID))
# Load data raw water level data
data = list(y = dat$Value, group = as.numeric(dat$WiscID), n = dim(dat)[1], J = J,
            precip = dat$precipCMDV, K = K)
# Load data deviation from mean water levels
dataDV = list(y = dat$ValueDV, group = as.numeric(dat$WiscID), n = dim(dat)[1], J = J,
              precip = dat$precipCMDV, K = K)
# Initial values
inits = function (){
  list(mu.alpha = rnorm(1), mu.beta=rnorm(1), sigma=runif(1),
       BB=matrix(rnorm(J*K),nrow=J,ncol=K), sigma.a=runif(1), sigma.b=runif(1), rho=runif(1) )
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
ni <- 10000
nt <- 1
nb <- 3000
nc <- 3

# Run the model
out = jags(data, inits, parameters, "model.txt", n.chains = nc, 
           n.thin = nt, n.iter = ni, n.burnin = nb)

# Show some of the result
print(out, dig = 3)
which(out$BUGSoutput$summary[, c("Rhat")] > 1.1)
max(out$BUGSoutput$summary[, c("Rhat")])
out.mcmc <- as.mcmc(out)
str(out.mcmc)
# look at summary
summary(out.mcmc)
# Create traceplots
xyplot(out.mcmc)
# Look at posterior density plots
densityplot(out.mcmc)

#### Just make plots for parameters of interest
out.mcmc2 <- out.mcmc[,c("mu.alpha","sigma.alpha","sigma")]
xyplot(out.mcmc2)
densityplot(out.mcmc2)


reg.coef = out$BUGSoutput$mean$BB
lakes = unique(dat$WiscID)

pdf("myOut.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,3))
for (i in 1:length(lakes)){
  plot(x = (dat %>% filter(WiscID==i) %>% select(precipCMDV))[[1]], y = (dat %>% filter(WiscID==i) %>% select(Value))[[1]],xlab="PrecipCMDV (mm)",ylab="Water Level (mm)",pch=16)
  tryCatch({
    abline(lm((dat %>% filter(WiscID==i) %>% select(Value))[[1]]~(dat %>% filter(WiscID==i) %>% select(precipCMDV))[[1]]),col="lightblue",lwd=2)
  },error=function(e){})
  abline(a = reg.coef[i,1],b=reg.coef[i,2],col="red",lwd=2)
  mtext(side=3,line=1,paste("WiscID: ",i))
}
dev.off()