library(maps)
library(maptools)
library(scales)
EcoContext <- read_excel("RFModels/EcoContext.xlsx")
regressionstats <- read_csv("BayHModels/regressionstats.csv")
global.slope = as.numeric(quantile(out$BUGSoutput$sims.list$mu.beta,c(0.025,0.975)))
sims.out = as.data.frame(out$BUGSoutput$sims.list$BB[1:4500,1:466,2])
lakeinterval = as.data.frame(t(sapply(sims.out,function(x) quantile(x,c(0.025,0.975)))))
slope.class = rep(NA,466)
for(i in 1:nrow(lakeinterval)){
  if (lakeinterval[i,2] < global.slope[1]) slope.class[i] = 0 else if(lakeinterval[i,1] > global.slope[2]) slope.class[i]=3 else slope.class[i] =1
  
}

regressionstats$slope.class = slope.class

dat = regressionstats %>% left_join(select(EcoContext,WiscID,lat,long ))



map.regions = c('wisconsin')
options(device="quartz")
dev.new(width=6,height=6)
map('state',region=map.regions,col=grey(.98),fill=TRUE,resolution = 0,mar=c(0,0,0,0),border=grey(.5),lty=5, main="Water Level Clusters")
points(dat$long,dat$lat,col=alpha(as.numeric(dat$slope.class)+1,.7),pch=16)
legend("topright",legend=c("Slope < 1","Slope 1-1.3","Slope > 1.3"),pch=16,col=c(1,2,4),title = "Precip Relationship",bty="n")       
