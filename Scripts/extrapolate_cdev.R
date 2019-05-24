cmdv = read.csv("big_data/cdev.csv")
cmdv$Date = as.Date(as.character(cmdv$obs_mo))
seepage_lake_slope_data <- read_csv("data/seepage_lake_slope_data.csv") %>% select(WiscID,WBIC)
levels_mo <- read.csv("data/monthly_waterlevels.csv")
levels_mo$Date <- as.Date(levels_mo$Date)
levels_yrs <- levels_mo %>%
  left_join(seepage_lake_slope_data) %>% 
  group_by(WBIC,WiscID) %>%
  summarize(n = length(unique(Year))) %>% 
  arrange(WBIC,WiscID,desc(n)) %>% 
  slice(1)
pred_slopes <- read.csv("data/cdev_slope_pred")

pred_slopes <- pred_slopes %>% left_join(levels_yrs)
pred_slopes <- pred_slopes %>% mutate(b = -1)
lakes <- unique(pred_slopes$WiscID)
cmdv <- cmdv %>% filter(WiscID %in% lakes) %>% 
  select(WiscID,Date,ppt_cdm_96mo_mm)

for(i in 1:nrow(pred_slopes)) {
  m <- pred_slopes$slope[i]
  levels_obs <- levels_mo %>% filter(WiscID == pred_slopes$WiscID[i])
  precip_obs <- cmdv %>% filter(WiscID == pred_slopes$WiscID[i]) %>% arrange(Date) %>% 
    mutate(yhat = ppt_cdm_96mo_mm*m) %>% 
    right_join(levels_obs)
  pred_slopes$b[i] <- median(precip_obs$Value-precip_obs$yhat)
}

pred_slopes <- pred_slopes %>% arrange(desc(n))
pdf("graphics/cdev_predictions.pdf",width=8,height=10.5,onefile = TRUE)
par(mfrow=c(3,1),oma=c(0,0,0,0),mar=c(4,4,4,.1))
for(i in 1:nrow(pred_slopes)) {
  
  levels_obs <- levels_mo %>% filter(WiscID == pred_slopes$WiscID[i])
  level_pred <- cmdv %>% filter(WiscID == pred_slopes$WiscID[i]) %>% arrange(Date) %>% 
    mutate(yhat = ppt_cdm_96mo_mm*pred_slopes$slope[i]+pred_slopes$b[i])

  plot(level_pred$Date,level_pred$yhat,main = pred_slopes$WBIC[i])
  points(levels_obs$Date,levels_obs$Value,col="red",pch=16)
  lines(levels_obs$Date,levels_obs$Value,col="red")
  
}

dev.off()
