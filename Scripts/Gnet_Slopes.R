library(tidyverse)
library(cowplot)
####Original Data
dt = read_csv("GW_Models/lake_climate_20180414_openWaterSeason.csv")
dat <- dt %>% dplyr::select(WiscID,Date1,DeltaDate,Stage1_mm,Stage2_mm,DeltaWaterLevel_mm,
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


BugsOut <- read_csv("GW_Models/BUGSutputSummary.csv")

### Slope plots
dat.slope <- as.data.frame(BugsOut[1:51,])
dat.slope$WiscID <- allLakeList
dat.slope$WiscID <- factor(dat.slope$WiscID,levels=dat.slope$WiscID[order(dat.slope$mean)])
dat.slope <- dat.slope %>% arrange(mean)
names(dat.slope)[c(4,8)] <- c("l2.5","l97.5")


ggplot(data = dat.slope,aes(x=1:51,y=mean)) + 
  geom_hline(yintercept = BugsOut[[103,4]],col="blue") + 
  geom_hline(yintercept = BugsOut[[103,8]],col="blue")+
  geom_point() +
  geom_errorbar(aes(ymin=l2.5,ymax=l97.5)) +
  labs(x="Lake",y="Gnet")

dat.slope$color = "blue"
dat.slope$color[c(1:4,6:9,47:51)] = "red"

ggplot(data = dat.slope,aes(x=WiscID,y=mean,color=color)) + 
  geom_hline(yintercept = BugsOut[[103,4]],col="blue") + 
  geom_hline(yintercept = BugsOut[[103,8]],col="blue")+
  geom_point() +
  geom_errorbar(aes(ymin=l2.5,ymax=l97.5)) +
  labs(x="Lake",y="Gnet")
