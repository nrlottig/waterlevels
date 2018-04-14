library(tidyverse)
# Lake Level Project Bayesian Hierarchical Model
# Description: This program takes the lake level data and cumulative deviation of
# precipitation data. It runs the Bayesian Hierarchical Model of lake level against
# precipitation. 
# History:
# 02072018  Add Noah's code for calculating the standardized value
# 02072018  Add Noah's code for plotting 
# 02072018  Switch the names of x and y labels in my code
# 02112018  Add the code for mapping coefficient of variation and slope
# 02142018  Change the name of the IDs used for this model from WiscID to BHMID to avoid confusion
# 02142018  Change the method for calculation of coefficient of variance. Now CV = std(resid)/mean
# 02202018  Do not drop lake 200900 entirely. Only get rid of the data that are "no water"
# Note the 02202018 version does not contain all the code. Its purpose is to generate a lake information file
# 03122018 Developed from 02202018 version. Add the code to generate monthly lake level data
#          and precipitation data
##################################################
rm(list=ls())
# setwd("G:/CFL/R_workdirectary")

# Input and convert to monthly data
# Lake = read.csv("seepage_20180310.csv", colClasses = "character")
dat <- read_csv("BayHModels/BHM_input_20180310.csv")
FinalSeepageLakes <- read_csv("BayHModels/FinalSeepageLakes.csv")
dat = dat %>% filter(WiscID %in% FinalSeepageLakes$WiscID)
IDList = unique(Lake$WiscID)
Lake$Date = as.Date(Lake$Date, "%m/%d/%Y") #Doesn't work because 
Lake$Year_Month = format(Lake$Date, "%Y/%m")
Lake$Value = as.numeric(Lake$Value)
MonthlyData = data.frame(WiscID = character(0),
                         WBIC = character(0),
                         SiteName=character(0),
                         CountyName=character(0),
                         Value=numeric(0),
                         Date=character(0),
                         NumOb=numeric(0),  #Num of Observations in this month         
                         stringsAsFactors = FALSE)
MonthlyData[1:nrow(Lake),] = NA
# Drop WBIC 200900, should delete 5 observations here
Lake = Lake[!is.na(Lake$Value),]
counter = 1
for (i in 1:length(IDList)) {
  print(i)
  this.lake = Lake[Lake$WiscID %in% IDList[i],]
  MonthList = unique(this.lake$Year_Month)
  for (j in 1:length(MonthList)) {
    this.month = this.lake[this.lake$Year_Month %in% MonthList[j],]
    MonthlyData$WiscID[counter] = this.month$WiscID[1]
    MonthlyData$WBIC[counter] = this.month$WBIC[1]
    MonthlyData$SiteName[counter] = this.month$SiteName[1]
    MonthlyData$CountyName[counter] = this.month$CountyName[1]
    MonthlyData$Units[counter] = "feet"
    MonthlyData$Date[counter] = MonthList[j]
    MonthlyData$Value[counter] = mean(this.month$Value, na.rm = TRUE) 
    MonthlyData$NumOb[counter] = nrow(this.month)
    counter = counter + 1
  }
}
# Drop the empty rows
MonthlyData = MonthlyData[!is.na(MonthlyData$WiscID),]
# Convert the unit from ft to mm
MonthlyData$Value = 304.8*MonthlyData$Value
MonthlyData$Units = "mm"
write.csv(MonthlyData,"seepage_monthly_20180310.csv")
#################################################################
# Precipitation Data
library(dplyr)
library(raster)
library(rasterVis)
library(rgdal)
library(readr)

setwd("G:/CFL/R_workdirectary")
LakeLevelSites = read.csv("seepageInfo_20180310.csv")
lake_points_shp = SpatialPoints(dplyr::select(LakeLevelSites, long, lat))
setwd("G:/CFL/Model/cumDev_precipitation_Bob/Prism_120_mo_cmdv")
wi_ppt_cmdv_list = list.files(getwd(), pattern="bil$", full.names=FALSE)
cmdv = stack(wi_ppt_cmdv_list)
cmdv_extract = extract(cmdv,lake_points_shp)
lakes_cmdv = cbind(LakeLevelSites, cmdv_extract)
# The tidyr has a function called extract which overlaps with the function in the raster processing
# pacakge, so the tydyr pacakge has to be loaded here
library(tidyr)
#lakes_cmdv_gather<-gather(lakes_cmdv, "ppt_ym", "ppt_cmdv", 12:1358)
startNum = which(names(lakes_cmdv) == "ppt_190501_bil")
endNum = ncol(lakes_cmdv)
lakes_cmdv_gather<-gather(lakes_cmdv, "ppt_ym", "ppt_cmdv",startNum:endNum)
lakes_cmdv_gather$ppt_ymd <- paste(substr(lakes_cmdv_gather$ppt_ym, 5, 8), "-", (substr(lakes_cmdv_gather$ppt_ym, 9, 10)), "-01", sep = "")
lakes_cmdv_gather$ppt_ym <- paste(substr(lakes_cmdv_gather$ppt_ym, 5, 8), "-", (substr(lakes_cmdv_gather$ppt_ym, 9, 10)),sep = "")
setwd("G:/CFL/R_workdirectary")
write.csv(lakes_cmdv_gather, "lake_cmdv_score_1905_to_2017.csv")
##############################################################################
# Combine the precip and lake level
#rain = read.csv("lake_cmdv_score_1905_to_2017.csv", colClasses = "character")
rain = lakes_cmdv_gather
#LakeInfo = read.csv("seepage_info_20180310.csv", colClasses = "character")
#LakeInfo = data.frame(LakeLevelSites$WiscID, LakeLevelSites$WBIC, LakeLevelSites$lat)
#colnames(LakeInfo) = c("WiscID", "WBIC", "lat")
#rain = merge(rain, LakeInfo, by.x = "WiscID", by.y = "WiscID")
#rain$Date = as.Date(rain$ppt_ymd, "%m/%d/%Y")
rain$Date = as.Date(rain$ppt_ymd)
rain$Date= format(rain$Date, "%Y/%m")
rain$ID_Date = paste(rain$WiscID, "_", rain$Date, sep="")
# Prepare the lake level data
MonthlyData = read.csv("seepage_monthly_20180310.csv", colClasses = "character")
#MonthlyData = merge(MonthlyData, LakeInfo, by.x = "LakeID", by.y = "LakeID")
#MonthlyData$Date = as.Date(MonthlyData$Date)
MonthlyData$Date = format(MonthlyData$Date, "%Y/%m")
MonthlyData$ID_Date = paste(MonthlyData$WiscID, "_", MonthlyData$Date, sep="")
# Add precip to lake level
MonthlyData$precipCMDV = NA
for (i in 1:nrow(MonthlyData)) {
  this.CMDV = rain$ppt_cmdv[rain$ID_Date %in% MonthlyData$ID_Date[i]]
  if (length(this.CMDV) != 0) {
    MonthlyData$precipCMDV[i] = this.CMDV
    #print(i)
  } else {
    print(MonthlyData$ID_Date[i])
  }
}
write.csv(MonthlyData, "BHM_input_20180310.csv")
###########################################################
# Run the model
library(R2jags)

# Input data
dat = read.csv("BHM_input_20180310.csv", colClasses = "character")
# dat = MonthlyData
dat$Value = as.numeric(dat$Value)
dat$precipCMDV = as.numeric(dat$precipCMDV)
# Reassign a BHMID to all lakes
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
data = list(y = dat$Value, group = as.numeric(dat$BHMID), n = dim(dat)[1], J = J,
            precip = dat$precipCMDV, K = K)
# # Load data deviation from mean water levels
# dataDV = list(y = dat$ValueDV, group = as.numeric(dat$BHMID), n = dim(dat)[1], J = J,
#               precip = dat$precipCMDV, K = K)
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
parameters = c("mu.alpha","mu.beta","BB","sigma", "sigma.a", "sigma.b","rho")
# MCMC settings
ni <- 10000
nt <- 1
nb <- 3000
nc <- 3

# Run the model
out = jags(data, inits, parameters, "model.txt", n.chains = nc, 
           n.thin = nt, n.iter = ni, n.burnin = nb)

# Output the result
outputData1 = cbind(out$BUGSoutput$mean$BB,out$BUGSoutput$sd$BB)
colnames(outputData1) = c("intercept","slope","intercept_sd","slope_sd")
write.csv(outputData1, "BHM_output_20180310.csv")
#################################################################################
# Make a comprehensive result information sheet
lakes_attr = read.csv("BHM_output_20180310.csv", colClasses = "character")
# lakes_attr = outputData1
location_info = read.csv("seepageInfo_20180310.csv", colClasses = "character")
lakeIDList = unique(dat$WiscID)
# Assign lakeIDs to the coefficients
lakes_attr$WiscID = NA

for (i in 1:nrow(lakes_attr)) {
  lakes_attr$WiscID[i] = lakeIDList[i]
}
# Assign other information
lakes_attr$LakeName = NA
lakes_attr$WBIC = NA
lakes_attr$County = NA
lakes_attr$Source = NA
lakes_attr$Lat = NA
lakes_attr$Long = NA

for (j in 1:nrow(lakes_attr)) {
  this.lake = location_info[location_info$WiscID %in% lakes_attr$WiscID[j],]
  lakes_attr$LakeName [j] = this.lake$Name
  lakes_attr$WBIC[j] = this.lake$WBIC
  lakes_attr$County [j] = this.lake$County
  lakes_attr$Source [j] = this.lake$source
  lakes_attr$Lat[j] = this.lake$lat
  lakes_attr$Long [j] = this.lake$long
}
colnames(lakes_attr)[1] = "BHMID"

# Calculate coefficient of variance
# Raw Data - Predicted Data - Residuals - Standard deviation of the residuals
lakes_attr$CV = NA
lakes_attr$STD = NA
true_value = dat
lakes_attr$slope = as.numeric(lakes_attr$slope)
lakes_attr$intercept = as.numeric(lakes_attr$intercept)
predicted_value = vector(mode = "numeric")
resid_value = vector(mode = "numeric")
for (i in 1:nrow(lakes_attr)) {
  this.lake = true_value[true_value$WiscID %in% lakes_attr$WiscID[i],]
  this.lake$predic = (this.lake$precipCMDV)*lakes_attr$slope[i] + lakes_attr$intercept[i]
  this.lake$resid = this.lake$Value - this.lake$predic
  lakes_attr$STD[i] = sd(this.lake$resid)
  lakes_attr$CV[i] = sd(this.lake$resid)/mean(this.lake$resid)
  predicted_value = append(predicted_value, this.lake$predic)
  resid_value = append(resid_value, this.lake$resid)
  print(i)
}
true_value$Predicted = predicted_value
true_value$Residual = resid_value

# Read the lagos information and attach it to the lake information sheet
lagos = read.csv("lagoscrosswalk.csv")
lakes_attr$LagoID = NA
lago_wbic = unique(lagos$WiscID)
for (i in 1:length(lago_wbic)) {
  if (lago_wbic[i] %in% lakes_attr$WiscID) {
    lakes_attr$LagoID[lakes_attr$WiscID %in% lago_wbic[i]] = 
      lagos$lagoslakeid[lagos$WiscID %in% lago_wbic[i]]
  } else {
    print(lago_wbic[i])
  }
}


# Output the lake information table 
write.csv(lakes_attr, "regression_coefs_20180310.csv")
# Output other data
write.csv(true_value, "regression_data_20180310.csv")
#############################################################################
# Plot the result
library(lubridate) # function as_date

true_value = read.csv("regression_data_20180310.csv", colClasses = "character")

reg.coef = out$BUGSoutput$mean$BB
lakes = unique(true_value$WiscID)
true_value$Date = as.character(true_value$Date)
true_value$Date = paste(true_value$Date,"/15",sep="")
true_value$Date = as_date(x = true_value$Date)
true_value$Value = as.numeric(true_value$Value)
true_value$precipCMDV = as.numeric(true_value$precipCMDV)
true_value$Predicted = as.numeric(true_value$Predicted)

pdf("time_series.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,2))
for (i in 1:length(lakes)){
  #pull out data for each lake and generate predicted water levels
  dat.t = true_value %>% filter(WiscID==lakes[i]) %>% arrange(Date)
  #dat.t = dat.t %>% mutate(predValue=reg.coef[i,1] + dat.t$precipCMDV*reg.coef[i,2])
  #plot relationship between precip and water level
  plot(x = dat.t$precipCMDV, y = dat.t$Value,xlab="PrecipCMDV (mm)",
       ylab="Water Level (mm)",pch=16,ylim=range(true_value$Value),
       xlim=range(true_value$precipCMDV))
  tryCatch({
    abline(lm(dat.t$Value~dat.t$precipCMDV),col="lightblue",lwd=2)
  },error=function(e){})
  abline(a = reg.coef[i,1][[1]],b=reg.coef[i,2][[1]],col="red",lwd=2)
  abline(a = out$BUGSoutput$mean$mu.alpha,b=out$BUGSoutput$mean$mu.beta,col="green",lwd=2)
  mtext(side=3,line=1,paste("WiscID ",lakes[i],"WBIC",dat.t$WBIC,dat.t$SiteName,sep = "-"))
  #plot predicted and observed water levels
  y.range = range(c(dat.t$Value,dat.t$Predicted))
  plot(dat.t$Date,dat.t$Value,type="b",pch=16,xlab="Date",ylab="Water Level (mm)",ylim=c(1000,max(2000,y.range[2])))
  points(dat.t$Date,dat.t$Predicted,type="b",pch=16,col="blue")
  
}
dev.off()