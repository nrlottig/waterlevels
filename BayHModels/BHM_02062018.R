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
library(lubridate)
library(MLmetrics)

# Input data
dat = read_tsv("BayHModels/BHM_input_20180410.csv")
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

#standardize the lowest observed water level to 1000mm (1m)
lakes = unique(dat$BHMID)
for(i in 1:length(lakes)){
  temp = dat$Value[dat$BHMID %in% lakes[i]]
  std.level = temp-min(temp)+1000
  dat$Value[dat$BHMID %in% lakes[i]] = std.level
}

#standardize the lowest observed precipCMDV to 0
for(i in 1:length(lakes)){
  temp = dat$precipCMDV[dat$BHMID %in% lakes[i]]
  std.level = temp-min(temp)
  dat$precipCMDV[dat$BHMID %in% lakes[i]] = std.level
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
J = length(unique(dat$BHMID))
# Load data raw water level data
dat = as.data.frame(dat)
data = list(y = dat$Value, group = as.numeric(dat$BHMID), n = dim(dat)[1], J = J,
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
ni <- 50000
nt <- 20
nb <- 20000
nc <- 3

# Run the model
out = jags.parallel(data = data, 
                    inits = inits, 
                    parameters.to.save = parameters, 
                    model.file = "model.txt", 
                    n.chains = 3, 
                    n.thin = 20, 
                    n.iter = 50000, 
                    n.burnin = 20000,
                    n.cluster = 3)

# Show some of the result
print(out, dig = 3)
which(out$BUGSoutput$summary[, c("Rhat")] > 1.1)
max(out$BUGSoutput$summary[, c("Rhat")])

###NOTE Trying to these just freezes up my computer####

# out.mcmc <- as.mcmc(out)
# str(out.mcmc)
# require(lattice)
# look at summary
# summary(out.mcmc)
# # Create traceplots
# xyplot(out.mcmc)
# Look at posterior density plots
# densityplot(out.mcmc)


CV <- function(mean, sd){
  (sd/mean)*100
}

reg.coef = out$BUGSoutput$mean$BB
var.coef = out$BUGSoutput$sd$BB
cv.resid = rep(x = NA,466)
n.dat = rep(NA,466)
lakes = unique(dat$BHMID)
dat$Date = as.character(dat$Date)
dat$Date = paste(dat$Date,"/15",sep="")
dat$Date = as_date(x = dat$Date)

pdf("BayHModels/myOut.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,2))
for (i in 1:length(lakes)){
  #pull out data for each lake and generate predicted water levels
  dat.t = dat %>% filter(BHMID==i) %>% arrange(Date)
  dat.t = dat.t %>% mutate(predValue=reg.coef[i,1] + dat.t$precipCMDV*reg.coef[i,2])
  cv.resid[i] = MAPE(dat.t$predValue,dat.t$Value)
    #sqrt(mean((dat.t$Value-dat.t$predValue)^2)) #RMSE predicted values
  n.dat[i] = length(dat.t$Value)
  #plot relationship between precip and water level
  plot(x = dat.t$precipCMDV, y = dat.t$Value,xlab="PrecipCMDV (mm)",
       ylab="Water Level (mm)",pch=16,ylim=range(dat$Value),
       xlim=range(dat$precipCMDV))
  tryCatch({
    abline(lm(dat.t$Value~dat.t$precipCMDV),col="lightblue",lwd=2)
  },error=function(e){})
  abline(a = reg.coef[i,1][[1]],b=reg.coef[i,2][[1]],col="red",lwd=2)
  abline(a = out$BUGSoutput$mean$mu.alpha,b=out$BUGSoutput$mean$mu.beta,col="green",lwd=2)
  mtext(side=3,line=1,paste(dat.t$SiteName[1], " WiscID:",dat.t$WiscID[1], " WIBIC:",dat.t$WBIC[1],sep=""),cex=.8)
  legend('topleft',legend=c("linear","bayesH","global"),lty=1,col=c("lightblue","red","green"))
  #plot predicted and observed water levels
  y.range = range(c(dat.t$Value,dat.t$predValue))
  plot(dat.t$Date,dat.t$Value,type="b",pch=16,xlab="Date",ylab="Water Level (mm)",ylim=c(1000,max(2000,y.range[2])))
  points(dat.t$Date,dat.t$predValue,type="b",pch=16,col="blue")
  legend("top",ncol=2,legend=c("obs","modeled"),lty=1,col=c("black","blue"))
  
}
dev.off()

reg.summary = data.frame(WiscID = allLakeList,slope=reg.coef[,2],sd.slope = var.coef[,2],mape=cv.resid,n.points=n.dat)
write_csv(reg.summary,"BayHModels/regressionstats.csv")
