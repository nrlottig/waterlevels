library(tidyverse)
library(lubridate)
library(cowplot)

cmdv = read.csv("big_data/cdev.csv")
cmdv$Date <- as.Date(as.character(cmdv$obs_mo))
levels_mo <- read.csv("data/monthly_waterlevels.csv")
levels_mo$Date <- as.Date(as.character(levels_mo$Date))
seepage_20180414 <- read.csv("big_data/seepage_20180414.csv",header=TRUE) %>% 
  select(WBIC,WiscID) %>% distinct()

#Get sites we want to model....ones with the most years of data not necessarily
#the most data
sites <- levels_mo %>%
  left_join(seepage_20180414) %>% 
  select(WBIC,WiscID,Value) %>% 
  drop_na() %>%
  group_by(WBIC) %>%
  summarize(records = length(unique(WiscID))) %>% 
  ungroup()

sites.list <- sites %>% filter(records==1) %>% select(WBIC) %>% left_join(seepage_20180414)
sites <- sites %>% 
  filter(records > 1) %>% 
  select(WBIC) %>% 
  left_join(seepage_20180414) %>% 
  left_join(levels_mo) %>% 
  mutate(Year = year(Date)) %>% 
  group_by(WBIC,WiscID) %>% 
  summarize(yrs = length(unique(Year)),records = n()) %>% 
  filter(yrs==max(yrs)) %>% 
  filter(records == max(records)) %>% 
  select(WBIC,WiscID)

sites.list <- rbind(sites.list,as.data.frame(sites))

levels_mo <- levels_mo[which(levels_mo$WiscID %in% sites.list$WiscID),]

monthly_level <- cmdv %>% select(WiscID,Date,ppt_cdm_96mo_mm) %>% 
  left_join(seepage_20180414) %>% left_join(levels_mo %>% select(WiscID,Value,Date)) %>% 
  rename(cmdv_precip_scale = 3) %>% arrange(Date)

out <- readRDS(file ="BayHModels/HLM_reduced.rds")

alpha <- out$mean$mu.a
beta <- out$mean$mu.b

lakes <- monthly_level %>% select(WBIC,Date,Value) %>% 
  drop_na() %>% mutate(Year = year(Date)) %>% 
  group_by(WBIC) %>% 
  summarize(yrs = length(unique(Year))) %>% 
  arrange(desc(yrs)) %>% select(WBIC) %>% 
  pull()

seepage_20180414 <- read.csv("big_data/seepage_20180414.csv",header=TRUE) %>% 
  select(WBIC,SiteName) %>% mutate(SiteName = as.character(SiteName))

lake.names = data.frame(WBIC = lakes) %>% left_join(seepage_20180414) %>% 
  group_by(WBIC) %>% slice(1) %>% right_join(tibble(WBIC=lakes)) %>% 
  ungroup() %>% 
  select(SiteName) %>% 
  pull()


J <- length(lakes)

pdf("graphics/cdev_predictions.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,1),oma=c(0,0,0,0),mar=c(4,4,4,.1))

for(i in 1:J) {
  dat_level <- monthly_level %>% filter(WBIC==lakes[i]) %>% drop_na()
  dat_level <- dat_level %>% select(WBIC,Date,Value)
  dat <- monthly_level %>% filter(WBIC == lakes[i])

  S = matrix(nrow = nrow(dat),ncol = 1)
  S <- alpha + beta*dat$cmdv_precip_scale
  extrap_level <- data.frame(Date= dat$Date, WiscID = lakes[i], level = S) # predicted lake levels
  dat.offset = dat_level %>% left_join(extrap_level) %>% 
    drop_na() %>% 
    group_by(year(Date)) %>% 
    summarize(yhat = mean(level),y=mean(Value)) %>% 
    mutate(offset = yhat-y)
  offset <- mean(dat.offset$offset)
  dat.compare <- dat_level %>% right_join(extrap_level) %>% 
    mutate(Value = Value + offset)
  y.range = range(dat.compare$Value,dat.compare$level,na.rm = TRUE)
  plot(dat.compare$Date,dat.compare$level,type="l",col="black",
       xlab="Date",ylab="Water Level (mm)",main=paste("WBIC ",lakes[i]," ", lake.names[i],sep=""),
       ylim=y.range)
  # grid(ny = round((y.range[2]-y.range[1])/10,digits = 0))
  abline(h=seq(from=-2000,to=2000,by=250),col="lightgrey",lty=3)
  miss <- dat.compare[!is.na(dat.compare$Value),]
  lines(miss$Date,miss$Value,col='red',type="l",pch=16,lwd=2)
  if(nrow(dat.compare)==1) points(miss$Date,miss$Value,col='red',type="p",pch=16,lwd=2)
  if(i==1) dat.out <- dat.compare else dat.out <- rbind(dat.out,dat.compare)
}
dev.off()
write.csv(dat.out,"data/cdev_extrapolations.csv")
