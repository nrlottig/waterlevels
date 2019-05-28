library(tidyverse)
library(cowplot) 
theme_set(theme_cowplot(font_size=10))

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
  
  if(i == 1) level_out <- level_pred else level_out <- rbind(level_out,level_pred)
  
}

dev.off()

rmse_data <- level_out %>% right_join(levels_mo) %>% drop_na() %>% 
  group_by(WiscID) %>% 
  mutate(yhat=scale(yhat,center = T,scale = F)) %>% 
  mutate(Value = scale(Value,center = T,scale = F))

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
RMSE(m = rmse_data$yhat,o = rmse_data$Value)
p3 <- ggplot(data = rmse_data, aes(x=Value/10,y=yhat/10))  + geom_abline(slope = 1,intercept = 0) + geom_point(alpha=0.2) +
  xlim(-250,250) + ylim(-250,250)+ 
  labs(x="Observed Water Level (cm)",y="Modeled Water Level (cm)")  +
  annotate("text", x = 100, y = -250, label = "RMSE: 30.2 cm")
p3

p1yhat <- level_out %>% filter(WiscID==282) %>% filter(year(Date) <= 2015)
p1obs <- levels_mo %>% filter(WiscID == 282)
p1mean <- mean(p1yhat$yhat)/10

p1 <- ggplot(data = p1yhat,aes(x=Date,y=yhat/10-p1mean)) + geom_line(size=1) + 
  geom_line(data = p1obs,aes(x=Date,y=Value/10-p1mean),color="red") +
  labs(y="Water Level (cm)")
p1
p2yhat <- level_out %>% filter(WiscID==584) %>% filter(year(Date)<=2015)
p2obs <- levels_mo %>% filter(WiscID == 584)
p2mean <- mean(p2yhat$yhat)/10
p2 <- ggplot(data = p2yhat,aes(x=Date,y=yhat/10-p2mean)) + geom_line(size=1) + 
  geom_line(data = p2obs,aes(x=Date,y=Value/10-p2mean),color="red") +
  labs(y="Water Level (cm)")
p2

col1 <- plot_grid(p1,p2,ncol = 1,align = "v",labels = c("A","B"))
col1
col3 <- plot_grid(col1,p3,ncol = 2,rel_widths = c(2,1),labels=c("","C"))
col3
ggsave(filename = "graphics/model_out.png",width = 7.5,height = 4,units = "in",dpi = 300)

level_out <- level_out %>% group_by(WiscID) %>% 
  mutate(yhat=scale(yhat,center = T,scale = F)) %>%
  ungroup()
write_csv(x = level_out,path = "data/cdev_lakelevel_predictions.csv")
